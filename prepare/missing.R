# Start session with a clean R environment
rm(list = ls(all.names = TRUE))

# This module handles missing data by using multiple imputation (MI);
# prior to MI, data is tested for being multivariate normal (MVN)
# and being missing completely at randome (MCAR).

if (!suppressMessages(require(mice))) install.packages('mice')
if (!suppressMessages(require(MissMech))) install.packages('MissMech')
if (!suppressMessages(require(BaylorEdPsych))) 
  install.packages('BaylorEdPsych')
if (!suppressMessages(require(mvnmle))) install.packages('mvnmle')
if (!suppressMessages(require(psych))) install.packages('psych')
if (!suppressMessages(require(MVN))) install.packages('MVN')
if (!suppressMessages(require(parallel))) install.packages('parallel')
if (!suppressMessages(require(ggplot2))) install.packages('ggplot2')
if (!suppressMessages(require(RColorBrewer)))
  install.packages('RColorBrewer')
if (!suppressMessages(require(Amelia))) install.packages('Amelia')

# 'mice' is needed for determining missingness patterns & MI
# 'MissMech' is needed for testing data for being MCAR
# 'BaylorEdPsych' is needed as an alternative for MCAR testing
# 'mvnmle' is needed as it is used by 'BaylorEdPsych'
# 'psych' is needed for describe()
# 'MVN' is needed for testing multivariate normality
library(mice)
library(MissMech)
library(BaylorEdPsych)
library(mvnmle)
library(psych)
library(MVN)
library(parallel)
library(ggplot2)
library(RColorBrewer)
library(Amelia)


RNG_SEED <- 100
set.seed(RNG_SEED) # for reproducibility

PRJ_HOME <- Sys.getenv("DISS_FLOSS_HOME")

source(file.path(PRJ_HOME, "utils/data.R"))
source(file.path(PRJ_HOME, "utils/factors.R"))
source(file.path(PRJ_HOME, "utils/platform.R"))

# Initially file was copied manually from "merged/SourceForge".
# Currently it is copied as a part of Makefile's merge rule.
# Implementing automatic data merging across all data sources
# should take care of this step (TODO).
MERGED_DIR <- file.path(PRJ_HOME, "data/merged")
MERGED_FILE <- "flossData"
RDS_EXT <- ".rds"

IMPUTED_DIR <- file.path(PRJ_HOME, "data/imputed")
IMPUTED_FILE <- "flossDataImputed"

DEBUG <- FALSE

NUM_CORES <- getOption("mc.cores")  # for parallel processing

NUM_IMPUTATIONS <- 5  # minimum recommended number of imputations


# additional transformations needed for data testing
prepareForMI <- function (data) {

  # convert factors to integers via as.numeric.factor() ["factors.R"]
  # the above doesn't work - however, as.integer() works just fine
  data[["Project.License"]] <- 
    as.integer(data[["Project.License"]])
  data[["License.Restrictiveness"]] <- 
    as.integer(data[["License.Restrictiveness"]])
  data[["Project.Stage"]] <- 
    as.integer(data[["Project.Stage"]])
  
  return (data)
}


message("\n===== HANDLING MISSING VALUES =====")

# ===== PREPARATION =====

fileName <- paste0(MERGED_FILE, RDS_EXT)
mergedFile <- file.path(MERGED_DIR, fileName)

# load data
message("\nLoading data...")
flossData <- loadData(mergedFile)

# use only (numeric) columns of our interest
# 'License.Category' doesn't vary, so it is excluded
flossData <- flossData[c("Repo.URL",
                         "Project.Age",
                         "Development.Team.Size",
                         "Project.License",
                         "License.Restrictiveness",
                         "Project.Stage",
                         "User.Community.Size")]

# temp fix for limited dataset - comment out/remove for full dataset
flossData[["Repo.URL"]] <- NULL

# additional transformations for MVN & MCAR testing
flossDataTest <- prepareForMI(flossData)

# Test for multivariate normality, using 'MVN' package
message("\nTesting data for multivariate normality...\n")

flossDataTest <- sampleDF(flossDataTest, 1000)

mvn.result <- MVN::mardiaTest(flossDataTest, cov = TRUE, qqplot = FALSE)
print(mvn.result)

mvn.result <- MVN::hzTest(flossDataTest, cov = TRUE, qqplot = FALSE)
print(mvn.result)

mvn.result <- MVN::roystonTest(flossDataTest, qqplot = FALSE)
print(mvn.result)

# Results show that the data is not multivariate normal. Therefore,
# we cannot use Amelia to perform MI, as it requires MV normality.
# However, we can use 'mice' package to perform MI, as it handles
# data without restrictions of being MVN and being MCAR.


# ===== VIZ =====

# TODO: loop through all variables

# visualizes multiple imputation (MI) results
vizIMresults <- function (obj) {
  
  # for categorical variables use one of the barchart types
  
  # convert IM results into "long" format
  objLong <- complete(obj, "long")
  
  # visualize difference between imputations via barchart
  g1 <- ggplot(objLong, aes(x=License.Restrictiveness)) +
    geom_bar() + facet_wrap(~ .imp)
  
  # visualize difference between imputations via stacked barchart
  g2 <- ggplot(objLong, aes(License.Restrictiveness, fill=.imp)) +
    geom_bar()
  
  
  # for continuous variables
  
  # log transform continuous data
  objLong["Project.Age"] <- log(objLong["Project.Age"])
  objLong["Development.Team.Size"] <- log(objLong["Development.Team.Size"])
  objLong["User.Community.Size"] <- log(objLong["User.Community.Size"])
  
  # visualize difference between imputations via boxplots
  g3 <- ggplot(objLong, aes(factor(.imp), User.Community.Size)) +
    geom_boxplot()
  
  print(g1)
  print(g2)
  print(g3)
}


# ===== ANALYSIS =====


# First, determine the missingness patterns
# (amount of missingness across observations and variables)
message("\nAnalyzing missingness patterns...\n")
print(mice::md.pattern(flossData))

# consider creating a heatmap of the md.pattern() return values
# OR using 'MissingDataGUI' package (https://github.com/chxy/MissingDataGUI),
# which is based on 'ggplot2' and is able to create panel displays

# the following method ('Amelia') doesn't produce nice results
# visualize missingness of the dataset
#missmap(flossData, main = "Missingness Map")


message("\nTesting data for being MCAR...\n")

# test MCAR using 'MissMech' package
TestMCARNormality(prepareForMI(flossData[sample(nrow(flossData), 10000), ]))

# let's also test, using 'BaylorEdPsych' package;
# set index condition to all rows that contain at least some data
# (partial missingness)
mcar.little <- 
  LittleMCAR(flossData[rowSums(is.na(flossData)) < ncol(flossData),])

message("\n\n")
print(mcar.little[c("chi.square", "df", "p.value")])


# ===== HANDLE MISSING VALUES =====

message("\nPerforming Multiple Imputation (MI)...", appendLF = DEBUG)

# remove "Project License" column, as this data doesn't require MI
flossData <- flossData[setdiff(names(flossData), "Project.License")]

# perform multiple imputation, using 'mice'

# first, remove totally missing data, leaving partially missing
part.miss.index <- rowSums(is.na(flossData)) < ncol(flossData)

# subset the data to those rows with at least some data (part.miss)
flossData2 <- flossData[part.miss.index, ]

# a matrix of ones, used to indicate which variables predict which
# in the multiple imputation
pmat <- matrix(1, nrow = ncol(flossData2), ncol = ncol(flossData2))

# never predict a variable from itself
diag(pmat) <- 0

# make other modifications, as needed, here,
# so that some variables are not predicted,
# or that some variables do not predict
# (for details, see the JSS article or ?mice)


# the methods should match the order of
# variables in your data
# use norm for continuous variables
# use polr for ordered variables (like
#   1, 2, 3, 4, or low, med, high type variables)
#   note for ordered variables they must be ordered factors
#   factor(1:3, ordered = TRUE)
# for binary variables (like 0/1, yes/no)
#   use logreg, again the variables should be
#   factors (but they do not have to be ordered factors)
#   factor(0:1)

# empty character vector
mi.methods <- rep("", ncol(flossData2))

# replace first with norm for continuous
mi.methods[unlist(mclapply(flossData2,
                           function(x) is.integer(x) | is.numeric(x),
                           mc.cores = NUM_CORES))] <- "norm"

# now replace factors with logreg, note that ordered factors are factors
# so this is not specific to binary
#mi.methods[unlist(lapply(flossData2, is.factor))] <- "logreg"

# use polytomous logistic regression, as we have factors with > 2 levels
mi.methods[unlist(lapply(flossData2, is.factor))] <- "polyreg" # "fastpmm"

# now replace ordered factors (a subset of factors) with polr
mi.methods[unlist(lapply(flossData2, is.ordered))] <- "polr"

# perform MI, using parallel processing on all available cores
imputed <- mclapply(seq_len(NUM_CORES), function(i) {
  mice(flossData2, m = NUM_IMPUTATIONS %/% NUM_CORES + 1,
                     method = mi.methods, predictorMatrix = pmat,
                     seed = RNG_SEED + i)
})

msg <- ifelse(DEBUG, "\n", "")
message(paste0(msg, "Completed.\n"))

if (DEBUG) print(str(imputedData))

# combine separate imputations into a single one
imputedCombined <- imputed[[1]]
for (i in seq.int(2, length(imputed), 1))
  imputedCombined <- ibind(imputedCombined, imputed[[i]])

message("\nSaving imputed data... ", appendLF = FALSE)

if (!file.exists(IMPUTED_DIR))
  dir.create(IMPUTED_DIR, recursive = TRUE)

# save imputed data to a separate directory
fileName <- paste0(IMPUTED_FILE, RDS_EXT)
imputedFile <- file.path(IMPUTED_DIR, fileName)
saveRDS(imputedCombined, imputedFile)

# visualize MI results
vizIMresults(imputedCombined)

message("Done.")

##### TODO: analyze results? In a later phase (pre-CFA/SEM).

message("")
