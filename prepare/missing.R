# Start session with a clean R environment
rm(list = ls(all.names = TRUE))

# This module will handle missing values in data by using
# multiple imputation, implemented in Amelia II software.

# Corresponding R package 'Amelia' and documentation are
# available at http://cran.r-project.org/web/packages/Amelia.

if (!suppressMessages(require(mice))) install.packages('mice')
if (!suppressMessages(require(MissMech))) install.packages('MissMech')
if (!suppressMessages(require(BaylorEdPsych))) 
  install.packages('BaylorEdPsych')
if (!suppressMessages(require(mvnmle))) install.packages('mvnmle')
if (!suppressMessages(require(methods))) install.packages('methods')
if (!suppressMessages(require(Amelia))) install.packages('Amelia')
if (!suppressMessages(require(psych))) install.packages('psych')
if (!suppressMessages(require(MVN))) install.packages('MVN')

# 'mice' is needed for determining missingness patterns
# 'MissMech' is needed for testing data for being MCAR
# 'BaylorEdPsych' is needed as an alternative for MCAR testing
# 'mvnmle' is needed as it is used by 'BaylorEdPsych'
# 'methods' is needed for 'Amelia' to alleviate the following error:
# "Error in match.fun(FUN) : object 'is' not found"
# 'psych' is needed for describe()
# 'MVN' is needed for testing multivariate normality
library(mice)
library(MissMech)
library(BaylorEdPsych)
library(mvnmle)
library(methods)
library(Amelia)
library(psych)
library(MVN)

PRJ_HOME <- Sys.getenv("DISS_FLOSS_HOME") # getwd()

source(file.path(PRJ_HOME, "utils/data.R"))
source(file.path(PRJ_HOME, "utils/factors.R"))

# Initially file was copied manually from "merged/SourceForge".
# Implementing automatic data merging across all data sources
# should take care of this step (TODO).
MERGED_DIR <- file.path(PRJ_HOME, "data/merged")
MERGED_FILE <- "flossData" # default
RDS_EXT <- ".rds"

IMPUTED_DIR <- file.path(PRJ_HOME, "data/imputed")
IMPUTED_FILE <- "flossData" # default

DEBUG <- FALSE


# additional transformations needed for missing data
# handling (multiple imputation / 'Amelia')
prepareForMI <- function (data) {

  # convert factors to integers via as.numeric.factor() ["factors.R"]
  # the above doesn't work - however, as.integer() works just fine
  data[["Project License"]] <- 
    as.integer(data[["Project License"]])
  data[["License Category"]] <- 
    as.integer(data[["License Category"]])
  data[["License Restrictiveness"]] <- 
    as.integer(data[["License Restrictiveness"]])
  data[["Development Stage"]] <- 
    as.integer(data[["Development Stage"]])
  data[["Project Maturity"]] <- 
    as.integer(data[["Project Maturity"]])

  return (data)
}


message("\n===== HANDLING MISSING VALUES: MI and FIML =====")

# ===== PREPARATION =====

fileName <- paste0(MERGED_FILE, RDS_EXT)
mergedFile <- file.path(MERGED_DIR, fileName)

# load data
message("\nLoading data...")
flossData <- loadData(mergedFile)

# additional transformations for MI
flossData <- prepareForMI(flossData)

# use only (numeric) columns of our interest;
# this is a recommended (preferred) alternative
# to declaring unused variables as ID variables
flossData <- flossData[c("Repo URL",
                         "Project License",
                         "License Restrictiveness",
                         "Development Stage",
                         "Project Maturity",
                         "User Community Size")]

# temp fix for limited dataset - comment out/remove for full dataset
flossData[["Repo URL"]] <- NULL


# Test for multivariate normality, using 'MVN' package
message("\nTesting data for multivariate normality...\n")

flossData <- sampleDF(flossData, 1000)

mvn.result <- MVN::mardiaTest(flossData, cov = TRUE, qqplot = FALSE)
print(mvn.result)

mvn.result <- MVN::hzTest(flossData, cov = TRUE, qqplot = FALSE)
print(mvn.result)

mvn.result <- MVN::roystonTest(flossData, qqplot = FALSE)
print(mvn.result)

stop()

# Results show that the data is not multivariate normal. Therefore,
# we cannot use Amelia to perform MI, as it requires MV normality.
# However, we can use 'mice' package to perform MI, as it handles
# data without restrictions of being MVN and being MCAR.



# ===== ANALYSIS =====


# First, determine the missingness patterns
# (amount of missingness across observations and variables)
message("\nAnalyzing missingness patterns...\n")
print(mice::md.pattern(flossData))

# add trailing '\n' when code below is enabled
#message("\nTesting data for being MCAR... Currently disabled.")
message("\nTesting data for being MCAR...\n")


a <- colMeans(is.na(flossData[rowSums(is.na(flossData)) < ncol(flossData), ])) * 100
print(a)


# currently disabled due to producing the following error:
# "Error: cannot allocate vector of size 4.3 Gb"
#MissMech::TestMCARNormality(flossData, nrep = 100)
#MissMech::TestMCARNormality(flossData[complete.cases(flossData),])
#MissMech::TestMCARNormality(flossData)

# instead, let's use function from 'BaylorEdPsych' package
# currently also disabled due to producing the following error:
# "Error in solve.default(cov) : 'a' is 0-diml"
#mcar.little <- BaylorEdPsych::LittleMCAR(flossData)

# try removing all incomplete cases to prevent error below
#mcar.little <- 
#  BaylorEdPsych::LittleMCAR(flossData[complete.cases(flossData),])
mcar.little <- 
  BaylorEdPsych::LittleMCAR(flossData[rowSums(is.na(flossData)) < ncol(flossData),])

message("\n\n")
print(mcar.little[c("chi.square", "df", "p.value")])
#message("\n")
#stop()

# ===== HANDLE MISSING VALUES =====

message("\nPerforming Multiple Imputation (MI)...", appendLF = FALSE)

# perform multiple imputation with 'Amelia'
a.out <- amelia(flossData, p2s = 0)

#if (DEBUG) str(a.out) #TODO: why fails?

# display results of the MI and data summary
message("Completed.\n")
message("MI Results:")
message("===========")
summary(a.out)

# output data summary before and after MI (latest MI iteration)
message("Data before MI:")
message("===============\n")
print(summary(flossData))

# suppress "NAs introduced by coercion" warnings
message("\nMore detailed summary statistics:")
message("=================================\n")
suppressWarnings(describe(flossData))

message("\nData after MI:")
message("==============\n")
print(summary(a.out$imputations$imp5))

# suppress "NAs introduced by coercion" warnings
message("\nMore detailed summary statistics:")
message("=================================\n")
suppressWarnings(describe(a.out$imputations$imp5))

message("\nSaving imputed data... ", appendLF = FALSE)

if (!file.exists(IMPUTED_DIR))
  dir.create(IMPUTED_DIR, recursive = TRUE)

# save imputed data to a separate directory
fileName <- paste0(IMPUTED_FILE, RDS_EXT)
imputedFile <- file.path(IMPUTED_DIR, fileName)
saveRDS(a.out$imputations$imp5, imputedFile)

message("Done.")

# TODO: analyze results?

message("")
