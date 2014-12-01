# ===== TODO: =====
# consider producing outer/inner model diagram figures,
# using 'semPlot' package (see semPaths() functon's use in CFA module)

# Start session with a clean R environment
## @knitr CleanEnv
rm(list = ls(all.names = TRUE))

## @knitr LoadPackages
if (!suppressMessages(require(plspm))) install.packages('plspm')
if (!suppressMessages(require(mice))) install.packages('mice')
if (!suppressMessages(require(ggplot2))) install.packages('ggplot2')
if (!suppressMessages(require(RColorBrewer)))
  install.packages('RColorBrewer')
if (!suppressMessages(require(reshape))) install.packages('reshape')
if (!suppressMessages(require(pander))) install.packages('pander')

library(plspm)
library(mice)
library(ggplot2)
library(RColorBrewer)
library(reshape)
library(pander)


##### PREPARATION & DEFINITIONS #####

## @knitr PrepareSEM

PRJ_HOME <- Sys.getenv("DISS_FLOSS_HOME")

source(file.path(PRJ_HOME, "config/diss-floss-config.R"))
source(file.path(PRJ_HOME, "utils/data.R"))
source(file.path(PRJ_HOME, "utils/platform.R"))
source(file.path(PRJ_HOME, "utils/qgraphtikz.R")) # fix for TikZ device
source(file.path(PRJ_HOME, "utils/factors.R"))
source(file.path(PRJ_HOME, "utils/graphics.R")) # for golden ratio
source(file.path(PRJ_HOME, "utils/knit.R"))

LOADINGS_THRESHOLD <- 0.7  # minimum value for acceptable loadings
GOF_THRESHOLD      <- 0.25  # minimum value for acceptable GoF (medium effect)

COLOR_PALETTE <- brewer.pal(8, "Set2") # OR "Accent"

GGPLOT2_PALETTE_FILL <- scale_fill_manual(values = COLOR_PALETTE)
GGPLOT2_PALETTE_LINE <- scale_color_manual(values = COLOR_PALETTE)

DEBUG <- FALSE  # local setting

datasetName <- ""


##### FUNCTIONS #####


semLoadData <- function () {
  
  fileName <- paste0(READY4SEM_FILE, RDS_EXT)
  ready4semFile <- file.path(READY4SEM_DIR, fileName)
  
  # Create results directory, if it doesn't exist
  if (!file.exists(SEM_RESULTS_DIR)) {
    dir.create(SEM_RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)
  }
  
  # load data
  message("\n\n*** Loading data...")
  flossData <- loadData(ready4semFile)
}


semSelectData <- function (flossData, impDataSet) {
  
  # due to very small amount of projects with "Non-OSI" licesnse
  # and their disapperance due to calculating correlations,
  # we don't include "License Category" into EFA (consider analyzing it
  # at later phases with inclusion of imputed data)
  
  # we also remove "Repo URL" due to injection of large # of NAs
  # due to limiting conditions at the end of the merge process
  
  # consider using "Use.Wiki" and "Use.Forum"
  # after transforming their values from character to integer
  
  # select imputed dataset
  flossData <- mice::complete(flossData, impDataSet)
  
  # NOTES on model structure and indicators availability:
  # -----------------------------------------------------
  # Currently, a second-order factor "Project.Maturity" is used in lieu of
  # the "Sponsorship" factor (TODO: consider indicators for "Sponsorship").
  
  # "Project.License" is excluded from analysis, as it's unordered factor.
  
  factors4analysis <- c("License.Category", "License.Restrictiveness",
                        "Preferred.Support.Type", "Preferred.Support.Resource",
                        "Project.Age", "Project.Stage",
                        "Development.Team.Size", "User.Community.Size")
  flossData <- flossData[, factors4analysis]
}


semTransformData <- function (flossData) {
  
  message("\n\n*** Transforming data...")
  
  # convert to unordered factors
  # (factor(..., ordered = FALSE) DOESN'T work here)
  
  flossData[["License.Category"]] <- 
    as.integer(flossData[["License.Category"]])
  flossData[["License.Restrictiveness"]] <- 
    as.integer(flossData[["License.Restrictiveness"]])
  flossData[["Project.Stage"]] <- 
    as.integer(flossData[["Project.Stage"]])
  flossData[["Preferred.Support.Type"]] <- 
    as.integer(flossData[["Preferred.Support.Type"]])
  flossData[["Preferred.Support.Resource"]] <- 
    as.integer(flossData[["Preferred.Support.Resource"]])
  
  flossData
}


semPrepareData <- function (impDataSet) {
  
  flossData <- semLoadData()
  flossData <- semSelectData(flossData, impDataSet)
  flossData <- semTransformData(flossData)

  # save name of the data set (seems redundant, but it will be useful,
  # when there will be more than one data set, i.e. 'pilot' and 'main')
  # [currently used for KNITR only]
  datasetName <- deparse(substitute(flossData))

  flossData
}


# sets up data frame with product terms for moderation

successModeration <- function (flossData) {
  
  flossDataModer <- flossData
  
  # specification of indicator products for MAT x GOV
  
  flossDataModer$PrjAgeLicCat <- 
    flossDataModer$Project.Age * flossDataModer$License.Category
  
  flossDataModer$PrjAgeLicRestr <- 
    flossDataModer$Project.Age * flossDataModer$License.Restrictiveness
  
  flossDataModer$PrjStageLicCat <- 
    flossDataModer$Project.Stage * flossDataModer$License.Category
  
  flossDataModer$PrjStageLicRestr <- 
    flossDataModer$Project.Stage * flossDataModer$License.Restrictiveness
  
  # specification of indicator products for MAT x SPON
  
  flossDataModer$PrjAgeSuppType <- 
    flossDataModer$Project.Age * flossDataModer$Preferred.Support.Type
  
  flossDataModer$PrjAgeSuppRes <- 
    flossDataModer$Project.Age * flossDataModer$Preferred.Support.Resource
  
  flossDataModer$PrjStageSuppType <- 
    flossDataModer$Project.Stage * flossDataModer$Preferred.Support.Type
  
  flossDataModer$PrjStageSuppRes <- 
    flossDataModer$Project.Stage * flossDataModer$Preferred.Support.Resource
  
  return (flossDataModer)
}


# Initial model specification
##########################################################

specifyModel <- function (modelTypeSEM, flossData) {
  
  message("\n\n*** Building model \"", modelTypeSEM, "\"...")
  
  if (modelTypeSEM == "directEffects") {
    
    # define rows of the path matrix (for inner model) - no mediation
    Governance   <- c(0, 0, 0, 0)
    Sponsorship  <- c(0, 0, 0, 0)
    Maturity     <- c(0, 0, 0, 0)
    Success      <- c(1, 1, 1, 0)  # GOV, SPON, MAT affect SUCCESS
  }
  
  if (modelTypeSEM == "mediation") {
    
    # define rows of the path matrix (for inner model) - mediation
    Governance   <- c(0, 0, 0, 0)
    Sponsorship  <- c(1, 0, 0, 0)  # GOV affects SPON
    Maturity     <- c(0, 0, 0, 0)
    Success      <- c(1, 1, 1, 0)  # GOV, SPON, MAT affect SUCCESS
    
  }
  
  if (modelTypeSEM == "moderation") {
    
    # set up data frame with product terms for moderation
    flossData <- successModeration(flossData)
    
    # define rows of the path matrix (for inner model) - single DV
    Governance   <- c(0, 0, 0, 0, 0, 0)
    Sponsorship  <- c(0, 0, 0, 0, 0, 0)
    MaturityGov  <- c(0, 0, 0, 0, 0, 0)
    MaturitySpon <- c(0, 0, 0, 0, 0, 0)
    Maturity     <- c(0, 0, 0, 0, 0, 0)
    Success      <- c(1, 1, 1, 1, 1, 0)
    
    # build the inner model matrix
    successPath <- rbind(Governance, Sponsorship,
                         MaturityGov, MaturitySpon, Maturity,
                         Success) 
    
    # add column/row names
    colnames(successPath) <- rownames(successPath) <-
      c("Governance", "Sponsorship", "MaturityGov", "MaturitySpon", "Maturity",
        "Success")
    
    # specify blocks of indicators (outer model), using variable names
    
    blockGovernance   <- c("License.Category", "License.Restrictiveness")
    blockSponsorship  <- c("Preferred.Support.Type", "Preferred.Support.Resource")
    blockMaturityGov  <- c("PrjAgeLicCat", "PrjAgeLicRestr", "PrjStageLicCat", 
                           "PrjStageLicRestr")
    blockMaturitySpon <- c("PrjAgeLicCat", "PrjAgeLicRestr", "PrjAgeSuppType",
                           "PrjAgeSuppRes")
    blockMaturity     <- c("Project.Age", "Project.Stage")
    blockSuccess      <- c("Development.Team.Size", "User.Community.Size")
    
    # build list of blocks (outer model)
    successBlocks <- list(blockGovernance, blockSponsorship,
                          blockMaturityGov, blockMaturitySpon,
                          blockMaturity, blockSuccess)
    
    # specify model's vector of modes ('A' is reflective)
    successModes <- rep("A", 6)
    
    # specify measurement scale for manifest variables
    successScales <- list(c("ord", "num"), c("ord", "num"), c("num"))
    
  } else {
    
    # build the inner model matrix
    successPath <- rbind(Governance, Sponsorship, Maturity, Success)
    
    # add column/row names
    colnames(successPath) <- rownames(successPath) <-
      c("Governance", "Sponsorship", "Maturity", "Success")
    
    # specify blocks of indicators (outer model), using variable names
    
    blockGovernance   <- c("License.Category", "License.Restrictiveness")
    blockSponsorship  <- c("Preferred.Support.Type", "Preferred.Support.Resource")
    blockMaturity     <- c("Project.Age", "Project.Stage")
    blockSuccess      <- c("Development.Team.Size", "User.Community.Size")
    
    # build list of blocks (outer model)
    successBlocks <- list(blockGovernance, blockSponsorship,
                          blockMaturity, blockSuccess)
    
    # specify model's vector of modes ('A' is reflective)
    successModes <- rep("A", 4)
    
    # specify measurement scale for manifest variables
    successScales <- list(c("ord", "num"), c("ord", "num"), c("num"))
  }

  # construct a model object and return it
  list(path = successPath, blocks = successBlocks,
       modes = successModes, scales = successScales,
       data = flossData)
}


# SEM Model Modifications
##########################################################

# Change: moved Project.Age and Project.Stage from MAT to SPON
# Result: no substantial changes in effects and model fit

specifyModel_Modif_1 <- function (semModel) {
  
  message("\n\n*** Building modified model \"", modelTypeSEM, "\"...")
  
  if (modelTypeSEM == "directEffects") {
    
    Governance   <- c(0, 0, 0)
    Sponsorship  <- c(0, 0, 0)
    Success      <- c(1, 1, 0)  # GOV, SPON, MAT affect SUCCESS
  }
  
  if (modelTypeSEM == "mediation") {
    
    # define rows of the path matrix (for inner model) - mediation
    Governance   <- c(0, 0, 0, 0)
    Sponsorship  <- c(1, 0, 0, 0)  # GOV affects SPON
    Maturity     <- c(0, 0, 0, 0)
    Success      <- c(1, 1, 1, 0)  # GOV, SPON, MAT affect SUCCESS
    
  }
  
  if (modelTypeSEM == "moderation") {
    
    # set up data frame with product terms for moderation
    flossData <- successModeration(flossData)
    
    # define rows of the path matrix (for inner model) - single DV
    Governance   <- c(0, 0, 0, 0, 0, 0)
    Sponsorship  <- c(0, 0, 0, 0, 0, 0)
    MaturityGov  <- c(0, 0, 0, 0, 0, 0)
    MaturitySpon <- c(0, 0, 0, 0, 0, 0)
    Maturity     <- c(0, 0, 0, 0, 0, 0)
    Success      <- c(1, 1, 1, 1, 1, 0)
    
    # build the inner model matrix
    successPath <- rbind(Governance, Sponsorship,
                         MaturityGov, MaturitySpon, Maturity,
                         Success) 
    
    # add column/row names
    colnames(successPath) <- rownames(successPath) <-
      c("Governance", "Sponsorship", "MaturityGov", "MaturitySpon", "Maturity",
        "Success")
    
    # specify blocks of indicators (outer model), using variable names
    
    blockGovernance   <- c("License.Category", "License.Restrictiveness")
    blockSponsorship  <- c("Preferred.Support.Type", "Preferred.Support.Resource")
    blockMaturityGov  <- c("PrjAgeLicCat", "PrjAgeLicRestr", "PrjStageLicCat", 
                           "PrjStageLicRestr")
    blockMaturitySpon <- c("PrjAgeLicCat", "PrjAgeLicRestr", "PrjAgeSuppType",
                           "PrjAgeSuppRes")
    blockMaturity     <- c("Project.Age", "Project.Stage")
    blockSuccess      <- c("Development.Team.Size", "User.Community.Size")
    
    # build list of blocks (outer model)
    successBlocks <- list(blockGovernance, blockSponsorship,
                          blockMaturityGov, blockMaturitySpon,
                          blockMaturity, blockSuccess)
    
    # specify model's vector of modes ('A' is reflective)
    successModes <- rep("A", 6)
    
    # specify measurement scale for manifest variables
    successScales <- list(c("ord", "num"), c("ord", "num"), c("num"))
    
  } else {
    
    # build the inner model matrix
    successPath <- rbind(Governance, Sponsorship, Success)
    
    # add column/row names
    colnames(successPath) <- rownames(successPath) <-
      c("Governance", "Sponsorship", "Success")
    
    # specify blocks of indicators (outer model), using variable names
    
    blockGovernance   <- c("License.Category", "License.Restrictiveness")
    blockSponsorship  <- c("Preferred.Support.Type", "Preferred.Support.Resource",
                           "Project.Age", "Project.Stage")
    blockSuccess      <- c("Development.Team.Size", "User.Community.Size")
    
    # build list of blocks (outer model)
    successBlocks <- list(blockGovernance, blockSponsorship, blockSuccess)
    
    # specify model's vector of modes ('A' is reflective)
    successModes <- rep("A", 3)
    
    # specify measurement scale for manifest variables
    successScales <- list(c("ord", "num"), c("ord", "num"), c("num"))
  }
  
  # construct a model object and return it
  list(path = successPath, blocks = successBlocks,
       modes = successModes, scales = successScales,
       data = flossData)
}



# Run PLS-PM Analysis
##########################################################

runAnalysis <- function (data, model, boot = FALSE) {
  
  message("\n\n*** Running PLS-PM analysis...")
  
  # run PLS-PM analysis
  if (boot) {
    
    message("\n\n*** Performing bootstrap validation...")
    
    # running bootstrap validation (100 samples)
    successPLS <- plspm(data,
                        path = model$path,
                        blocks = model$blocks,
                        modes = model$modes,
                        # scaling = model$scales,
                        boot.val = TRUE, br = 100)
  }
  else
    successPLS <- plspm(data,
                        path = model$path,
                        blocks = model$blocks,
                        modes = model$modes)  # scaling = model$scales
}


# 4.2. Handling PLS-PM Results
##########################################################

reportResults <- function (successPLS, boot = FALSE) {
  
  message("\n\n*** SEM-PLS analysis results:\n")
  
  # contents of the results object (what's available)
  if (DEBUG) print(successPLS)
  
  # summarized results
  if (boot) {
    print(successPLS$boot, digits = DIGITS)  # bootstrap results
  } else {
    print(summary(successPLS), digits = DIGITS)
  }
  
  # 4.3. Measurement Model Assessment: Reflective Indicators
  ##########################################################
  
  # outer model results (in a matrix way, unlike tabular in summary())
  print(successPLS$outer_model, digits = DIGITS)

  # unidimensionality
  print(successPLS$unidim)
  
  # 4.4. Measurement Model Assessment: Formative Indicators
  ##########################################################
  
  
  # 4.5. Structural Model Assessment
  ##########################################################
  
  # inner model
  print(successPLS$inner_model, digits = DIGITS)
  
  # matrix of path coefficients
  print(successPLS$path_coefs, digits = DIGITS)
  
  # inner model summary
  print(successPLS$inner_summary, digits = DIGITS)
  
  # select R2
  print(successPLS$inner_summary[, "R2", drop = FALSE], digits = DIGITS)
  
  # GoF index
  print(successPLS$gof, digits = DIGITS)
  
  ## Effects Analysis
  
  # effects summary (don't use summary() here)
  print(successPLS$effects, digits = DIGITS)
  
  # select effects ('active' rows)
  activeRows <- na.omit(successPLS$effects[successPLS$effects[, -1] != 0, ])
  activeRows <- as.integer(rownames(activeRows))
  
  # 'active' effects in matrix format
  path_effs <- as.matrix(successPLS$effects[activeRows, -1])
  
  # add rownames to path_effs
  rownames(path_effs) <- successPLS$effects[activeRows, 1]
  
  # active effects summary
  print(path_effs, digits = DIGITS)
  
  # move inner model summary here?
  # move GoF here?
}


vizResults <- function (successPLS, modelTypeSEM) {
  
  # plotting loadings
  gLoadDiag <- plot(successPLS, what = "loadings")
  
  # Not needed, since we have to call plot() method in .Rmd
  if (KNITR) {
    plspm_var <- paste0("plspm_", datasetName)
    assign(plspm_var, successPLS, envir = .GlobalEnv)
  }
  
  
  # display barchart of loadings with threshold value line
  
  gLoadBarChart <- ggplot(data = successPLS$outer_model,
                          aes(x = name, y = loading, fill = block)) +
    
    labs(x = "Indicator", y = "Loading") +
    scale_fill_discrete("Factors") +
    
    geom_bar(stat = 'identity', position = 'dodge') +
    
    # threshold line (to emphasize acceptable loadings)
    geom_hline(yintercept = LOADINGS_THRESHOLD, color = 'red') +
    
    # rotate x-axis labels, move axes titles and set optimal aspect ratio
    theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
          axis.title.x = element_text(vjust = -0.5, color = "black"),
          axis.title.y = element_text(vjust = 0.5, color = "black"),
          aspect.ratio = 1 / PHI) +
    
    # change color palette
    GGPLOT2_PALETTE_FILL + GGPLOT2_PALETTE_LINE
  
  
  if (KNITR) {
    gLoadBarChart_var <- paste0("loadBarChart_", datasetName)
    assign(gLoadBarChart_var, gLoadBarChart, envir = .GlobalEnv)
  }
  
  if (.Platform$GUI == "RStudio") {print(gLoadBarChart)}
  
  
  # plotting weights
  gWeights <- plot(successPLS, what = "weights")
  
  
  # reshape crossloadings data frame for ggplot2
  xloads <- melt(successPLS$crossloadings, id.vars = c("name", "block"),
                 variable_name = "LV")
  
  # barcharts of crossloadings by block
  gCrossLoadBlocks <- ggplot(data = xloads,
                             aes(x = name, y = value, fill = block)) +
    
    # add horizontal reference lines
    geom_hline(yintercept = 0, color = "gray75") +
    geom_hline(yintercept = 0.5, color = "gray70", linetype = 2) +
    
    # indicate the use of car-charts
    geom_bar(stat = 'identity', position = 'dodge') +
    
    # panel display (faceting)
    facet_wrap(block ~ LV) +
    
    # rotate x-axis labels, move axes titles and set optimal aspect ratio
    theme(axis.text.x = element_text(angle = 60, hjust = 1, color = "black"),
          axis.title.x = element_text(vjust = -0.5, color = "black"),
          axis.title.y = element_text(vjust = 0.5, color = "black")
          #line = element_blank(),
          #plot.title = element_text(size = 12),
          #aspect.ratio = 1 / PHI
    ) +
    
    # tweak some graphical elements
    #  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    #        line = element_blank(),
    #        plot.title = element_text(size = 12)) +
    
    # change color palette
    GGPLOT2_PALETTE_FILL + GGPLOT2_PALETTE_LINE
  
  if (KNITR) {
    gCrossLoadBlocks_var <- paste0("crossLoadBlocks_", datasetName)
    assign(gCrossLoadBlocks_var, gCrossLoadBlocks, envir = .GlobalEnv)
  }
  
  # display the crossloadings barchart panel
  if (.Platform$GUI == "RStudio") {print(gCrossLoadBlocks)}
  

  # matrix with values based on path coeffs
  arrow_lwd <- 10 * round(successPLS$path_coefs, 2)
  
  # save model diagrams due to issue, related to 'diagram' in RStudio envir.
  modFileName <- paste0("semInnerModel-", modelTypeSEM, ".png")
  png(file = modFileName)
  
  # visual: SEM path diagram (inner model)
  plot(successPLS, arr.lwd = arrow_lwd)
  dev.off()

  # visual: LV effects diagram
  # TODO: convert to ggplot2 version, rotate x-axis labels, etc.
  
  # select effects ('active' rows)
  activeRows <- na.omit(successPLS$effects[successPLS$effects[, -1] != 0, ])
  activeRows <- as.integer(rownames(activeRows))
  
  # 'active' effects in matrix format
  path_effs <- as.matrix(successPLS$effects[activeRows, -1])
  
  # add rownames to path_effs
  rownames(path_effs) <- successPLS$effects[activeRows, 1]
  
  # setting margin size
  op <- par(mar = c(8, 3, 1, 0.5))
  
  # barplots of total effects (direct + indirect)
  barplot(t(path_effs), border = NA, col = c("#9E9AC8", "#DADAEB"),
          las = 2, cex.names = 0.8, cex.axis = 0.8,
          legend = c("Direct", "Indirect"),
          args.legend = list(x = "top", ncol = 2, border = NA,
                             bty = "n", title = "Effects"))
  
  # resetting default margins
  par(op)
  
  list(loadBarChart = gLoadBarChart, crossLoadBlocks = gCrossLoadBlocks)
}


# save results of SEM analysis/validation on disk for model comparison

saveResults <- function (semResults, modelTypeSEM, impDataSet,
                         boot = FALSE) {
  
  impSuffix <- paste0("-", impDataSet)
  bootSuffix <- ifelse (boot, paste0("-", "boot"), "")
  
  fileName <- paste0(modelTypeSEM, impSuffix, bootSuffix, RDS_EXT)
  semResultsFile <- file.path(SEM_RESULTS_DIR, fileName)
  saveRDS(semResults, semResultsFile)
}


validateResults <- function (semResults) {
  
  ifelse(semResults$gof >= GOF_THRESHOLD, TRUE, FALSE)
}


adjustModel <- function (semModel) {
  
  specifyModel_Modif_1()
}


getPooledParam <- function (i, result, n) {
  
  est <- do.call(rbind, lapply(result, function (x) {
    cbind(x[i, "Estimate"], x[i, "Std..Error"])
  }))
  
  pooledResults <- 
    pool.scalar(est[, 1], (est[, 2])^2, n = n)
  
  retVal <- list(est = pooledResults$qbar,
                 se = sqrt(pooledResults$t),
                 df = pooledResults$df,
                 p = NULL)
  
  retVal$p <- 2 * pt(-abs(retVal$est / retVal$se), df = retVal$df)
  as.data.frame(retVal)
}


# extracts parameter estimates from SEM inner model and pools them together
poolPLS <- function (semResults) {
  
  result <- list()
  
  for (obj in semResults) {
    
    res <- do.call(rbind, lapply(names(obj$inner_model), function (n) {
      data.frame(Outcome = n, obj$inner_model[[n]])
    }))
    
    result <- c(result, list(res))
  }

  pooledParams <- lapply(1:nrow(result[[1]]), function (i) {
    getPooledParam(i, result, n = nrow(semResults[[1]]$data))
  })
  
  res <- do.call(rbind, pooledParams)
  
  ##TEMP DEBUG
  lapply(semResults, function (x) print(summary(x$inner_model)))
}


##### ANALYSIS #####

## @knitr PerformSEM

message("\n\n===== STRUCTURED EQUATION MODELING (SEM-PLS) ANALYSIS =====")

# available model types for SEM analysis
semModelTypes <- c()  # "directEffects", "mediation", "moderation"

semResultsList <- list()

# include into analysis models, enabled in configuration
if (DO_SEM_DIRECT_EFF) semModelTypes <- c(semModelTypes, "directEffects")
if (DO_SEM_MEDIATION) semModelTypes  <- c(semModelTypes, "mediation")
if (DO_SEM_MODERATION) semModelTypes <- c(semModelTypes, "moderation")

# perform SEM for each imputed data set
for (impDataSet in 1:NUM_IMP_EXTRACT) {
  
  # prepare data for SEM analysis
  flossData <- semPrepareData(impDataSet)
  
  # analyze each model
  for (modelType in semModelTypes) {
    
    # analyze the model
    flossModel <- specifyModel(modelType, flossData)
    successPLS <- runAnalysis(flossModel$data, flossModel)
    
    # validate results against pre-defined criteria (not bootstrapping!)
    success <- validateResults(successPLS)  # consider 'criteria' parameter
    
    # if results are not adequate, adjust model and re-run analysis
    if (!success) {
      flossModel <- adjustModel(flossModel)
      successPLS <- runAnalysis(flossData, flossModel)
    }
    
    # report, visualize and save results of the analysis
    reportResults(successPLS)
    g <- vizResults(successPLS, modelType)
    saveResults(successPLS, modelType, impDataSet)

    resObj <- paste0("successPLS", modelType)
    assign(resObj, successPLS, envir = .GlobalEnv)
    
    semResultsList <- c(semResultsList, list(successPLS))
    
    # additionally, perform bootstrap validation, if enabled
    if (DO_SEM_BOOT) {
      # validation (estimating precision of PLS parameters estimates)
      successVal <- runAnalysis(flossData, flossModel, boot = TRUE)
      reportResults(successVal, boot = TRUE)
      saveResults(successVal, modelType, impDataSet, boot = TRUE)
    }
  }
}


##TEMP DISABLED
if (FALSE) {
  pooledResults <- poolPLS(semResultsList)

  ##TEMP DEBUG
  print(str(pooledResults))
}


message("\n===== SEM-PLS analysis completed, results are ",
        "in directory \"", SEM_RESULTS_DIR, "\"\n")
