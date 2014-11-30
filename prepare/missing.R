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

PRJ_HOME <- Sys.getenv("DISS_FLOSS_HOME")

source(file.path(PRJ_HOME, "config/diss-floss-config.R"))
source(file.path(PRJ_HOME, "utils/data.R"))
source(file.path(PRJ_HOME, "utils/factors.R"))
source(file.path(PRJ_HOME, "utils/platform.R"))

DEBUG <- FALSE  # local setting

# consider moving to project's config, if used in multiple modules
# (disadvantage: would require loading 'parallel' project-wide, so TBD)
NUM_CORES <- getOption("mc.cores")  # for parallel processing


# additional transformations needed for data testing
prepareForMI <- function (data) {

  # convert factors to integers via as.numeric.factor() ["factors.R"]
  # the above doesn't work - however, as.integer() works just fine
  data[["Project.License"]] <- 
    as.integer(data[["Project.License"]])
  data[["License.Category"]] <- 
    as.integer(data[["License.Category"]])
  data[["License.Restrictiveness"]] <- 
    as.integer(data[["License.Restrictiveness"]])
  data[["Project.Stage"]] <- 
    as.integer(data[["Project.Stage"]])
  data[["Preferred.Support.Type"]] <- 
    as.integer(data[["Preferred.Support.Type"]])
  data[["Preferred.Support.Resource"]] <- 
    as.integer(data[["Preferred.Support.Resource"]])
  
  data <- data[setdiff(names(data), c("Preferred.Support.Resource"))]
  data <- data[setdiff(names(data), c("License.Category"))]
  
  return (data)
}


# Test data for multivariate normality, using 'MVN' package
mvnTests <- function (flossDataTest) {
  
  message("\nTesting data for multivariate normality...\n")
  
  flossDataTest <- sampleDF(flossDataTest, 1000)
  
  mvn.result <- MVN::mardiaTest(flossDataTest, cov = TRUE, qqplot = FALSE)
  print(mvn.result)
  
  mvn.result <- MVN::hzTest(flossDataTest, cov = TRUE, qqplot = FALSE)
  print(mvn.result)
  
  mvn.result <- MVN::roystonTest(flossDataTest, qqplot = FALSE)
  print(mvn.result)
}


missPatterns <- function (flossData) {
  
  message("\nAnalyzing missingness patterns...\n")
  print(mice::md.pattern(flossData))
  
  # consider creating a heatmap of the md.pattern() return values
  # OR using 'MissingDataGUI' package (https://github.com/chxy/MissingDataGUI),
  # which is based on 'ggplot2' and is able to create panel displays
  
  # the following method ('Amelia') doesn't produce nice results
  # visualize missingness of the dataset
  #missmap(flossData, main = "Missingness Map")
}


testMCAR <- function (flossData) {
  
  message("\nTesting data for being MCAR...\n")
  
  # test MCAR using 'MissMech' package
  TestMCARNormality(prepareForMI(flossData[sample(nrow(flossData), 10000), ]))
  
  flossData <- 
    flossData[setdiff(names(flossData), c("Preferred.Support.Resource"))]
  flossData <- 
    flossData[setdiff(names(flossData), c("License.Category"))]
  
  # let's also test, using 'BaylorEdPsych' package;
  # set index condition to all rows that contain at least some data
  # (partial missingness)
  
  flossData <- flossData[sample(nrow(flossData), 10000), ]
  
  mcar.little <- 
    LittleMCAR(flossData[rowSums(is.na(flossData)) < ncol(flossData),])
  
  message("\n\n")
  print(mcar.little[c("chi.square", "df", "p.value")])
}


transformData <- function (flossData) {
  
  message("\n\n*** Transforming data...")
  
  # log transform continuous data
  flossData["Project.Age"] <- log(flossData["Project.Age"])
  flossData["Development.Team.Size"] <- log(flossData["Development.Team.Size"])
  flossData["User.Community.Size"] <- log(flossData["User.Community.Size"])
  
  return (flossData)
}


# ===== VIZ =====

# TODO: loop through all variables

# visualizes multiple imputation (MI) results
vizMIresults <- function (obj) {
  
  # for categorical variables, use one of the barchart types
  
  # convert MI results into "long" format
  objLong <- complete(obj, "long")
  
  # visualize difference between imputations via barchart
  gImpBarChart <- ggplot(objLong, aes(x = License.Restrictiveness)) +
    geom_bar() + facet_wrap(~ .imp)
  
  # visualize difference between imputations via stacked barchart
  gImpStacked <- ggplot(objLong, aes(License.Restrictiveness, fill = .imp)) +
    geom_bar()
  
  
  # for continuous variables, use boxplots or density plots
  
  # visualize difference between imputations via boxplots
  gImpBoxPlot <- ggplot(objLong, aes(factor(.imp), User.Community.Size)) +
    geom_boxplot()
  
  # display visualizations in RStudio's Plots pane (tab)
  if (.Platform$GUI == "RStudio") {
    print(gImpBarChart)
    print(gImpStacked)
    print(gImpBoxPlot)
  }
  
  impPlots <- list(gImpBarChart, gImpStacked, gImpBoxPlot)
  #fileNames <- lapply(impPlots, function(x) deparse(substitute(x))) HOWTO ???
  fileNames <- list("gImpBarChart", "gImpStacked", "gImpBoxPlot")
  
  # save visualizations to separate files
  for (i in seq.int(impPlots)) {
    plotFile <- file.path(EDA_RESULTS_DIR,
                          paste0(fileNames[[i]], GRAPHICS_EXT))
    suppressMessages(ggsave(file = plotFile, plot = impPlots[[i]],
                            width = 4, height = 4))
  }
}


# MAIN: perform multiple imputation, using 'mice'
performMI <- function (flossData) {
  
  message("\nPerforming Multiple Imputation (MI)...", appendLF = DEBUG)
  
  # remove "Project License" column, as this data doesn't require MI
  flossData <- flossData[setdiff(names(flossData), "Project.License")]
  
  # perform multiple imputation, using 'mice'

  # TODO:
  # 1) move the following line to merge and 
  # 2) remove projects with low amount of info (optional; TBD)
  # 3) sample after that 20%
  # first, remove totally missing data, leaving partially missing
  part.miss.index <- rowSums(is.na(flossData)) < ncol(flossData)
  
  # subset the data to those rows with at least some data (part.miss)
  flossData2 <- flossData[part.miss.index, ]
  
  # a matrix of ones, used to indicate which variables predict which
  # in the multiple imputation
  pmat <- matrix(1, nrow = ncol(flossData2), ncol = ncol(flossData2))
  
  # never predict a variable from itself
  diag(pmat) <- 0

  # TODO: Test and, if all works OK, remove the overlapIdx code
  # don't allow variables with partial value overlap
  # to predict themselves
  
  #overlapIdx1 <- which(colnames(flossData2) == "Preferred.Support.Type")
  #overlapIdx2 <- which(colnames(flossData2) == "Preferred.Support.Resource")
  #pmat[c(overlapIdx1, overlapIdx2), c(overlapIdx1, overlapIdx2)] <- 0
  
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
  
  return (imputedCombined)
}


# save imputed data for further analysis (CFA/SEM)
saveImputedData <- function (imputedCombined) {
  
  message("\nSaving imputed data... ", appendLF = FALSE)
  
  if (!file.exists(IMPUTED_DIR))
    dir.create(IMPUTED_DIR, recursive = TRUE)
  
  # save imputed data to a separate directory
  fileName <- paste0(IMPUTED_FILE, RDS_EXT)
  imputedFile <- file.path(IMPUTED_DIR, fileName)
  saveRDS(imputedCombined, imputedFile)
  
  message("Done.")
}


message("\n===== HANDLING MISSING VALUES =====")

# ===== PREPARATION =====

fileName <- paste0(MERGED_FILE, RDS_EXT)
mergedFile <- file.path(MERGED_DIR, fileName)

# load data
message("\nLoading data...")
flossData <- loadData(mergedFile)

# select the columns of interest
flossData <- flossData[c("Repo.URL",
                         "Project.Age",
                         "Development.Team.Size",
                         "Project.License",
                         "License.Category",
                         "License.Restrictiveness",
                         "Project.Stage",
                         "Preferred.Support.Type",
                         "Preferred.Support.Resource",
                         "User.Community.Size")]

# temp fix for limited dataset - comment out/remove for full dataset
flossData[["Repo.URL"]] <- NULL


# ===== PREPARATION =====

# additional transformations for MVN & MCAR testing
flossDataTest <- prepareForMI(flossData)

# test for multivariate normality (MVN)
# 'License.Category' doesn't vary, when sampled,
# so it is excluded here (just for MVN tests, not for MI)
mvnResult <- mvnTests(flossDataTest[setdiff(names(flossDataTest),
                                            c("License.Category",
                                              "Preferred.Support.Type",
                                              "Preferred.Support.Resource"))])

# Results show that the data is not multivariate normal. Therefore,
# we cannot use Amelia to perform MI, as it requires MV normality.
# However, we can use 'mice' package to perform MI, as it handles
# data without restrictions of being MVN and being MCAR.

# ===== ANALYSIS =====

if (DO_MISSING_ANALYSIS) {
  
  # First, determine the missingness patterns
  # (amount of missingness across observations and variables)
  missPatterns(flossData)
  
  # test data for being MCAR
  testMCAR(flossData)
}


# ===== HANDLE MISSING VALUES =====

if (DO_MISSING_MAIN_MI) {
  
  # transform data prior to MI (log)
  flossData <- transformData(flossData)

  # perform multiple imputation, using 'mice'
  imputedCombined <- performMI(flossData)
  
  # save imputed data for further analysis (CFA/SEM)
  saveImputedData(imputedCombined)
}


if (DO_MISSING_VIZ_MI) {
  
  # retrieve imputed data, if MI is currently disabled
  if (!DO_MISSING_MAIN_MI) {
    
    fileName <- paste0(IMPUTED_FILE, RDS_EXT)
    imputedFile <- file.path(IMPUTED_DIR, fileName)
    
    if (!file.exists(imputedFile))
      stop("Cannot find imputed data, ",
           "please enable MI in config and run again!")
    
    imputedCombined <- readRDS(imputedFile)
  }
  
  # visualize MI results
  vizMIresults(imputedCombined)
}

message("")
