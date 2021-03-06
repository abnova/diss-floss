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

COLOR_PALETTE <- brewer.pal(8, "Set2") # OR "Accent"

GGPLOT2_PALETTE_FILL <- scale_fill_manual(values = COLOR_PALETTE)
GGPLOT2_PALETTE_LINE <- scale_color_manual(values = COLOR_PALETTE)

DEBUG <- FALSE  # local setting


##### GENERIC FUNCTIONS #####


semPLS <- function (data, factors, model, modelType) {
  
  # convert factors to integers
  factorCols <- vapply(data, is.factor, logical(1))
  data[factorCols] <- lapply(data[factorCols], as.integer)
  
  
}


##### ANALYSIS #####

## @knitr PerformSEM

message("\n\n===== STRUCTURED EQUATION MODELING (SEM-PLS) ANALYSIS =====")

fileName <- paste0(READY4SEM_FILE, RDS_EXT)
ready4semFile <- file.path(READY4SEM_DIR, fileName)

# Create results directory, if it doesn't exist
if (!file.exists(SEM_RESULTS_DIR)) {
  dir.create(SEM_RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)
}

# load data
message("\n\n*** Loading data...")
flossData <- loadData(ready4semFile)

# due to very small amount of projects with "Non-OSI" licesnse
# and their disapperance due to calculating correlations,
# we don't include "License Category" into EFA (consider analyzing it
# at later phases with inclusion of imputed data)

# we also remove "Repo URL" due to injection of large # of NAs
# due to limiting conditions at the end of the merge process

# consider using "Use.Wiki" and "Use.Forum"
# after transforming their values from character to integer

# select imputed dataset
flossData <- mice::complete(flossData, 1)

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

# save name of the data set (seems redundant, but it will be useful,
# when there will be more than one data set, i.e. 'pilot' and 'main')
# [currently used for KNITR only]
datasetName <- deparse(substitute(flossData))


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


# Initial model specification
##########################################################

message("\n\n*** Building model...")

modelTypeSEM <- "no-mediation" # TODO: consider moving to main config. file

if (modelTypeSEM == "mediation") {
  
  # define rows of the path matrix (for inner model) - mediation
  Governance   <- c(0, 0, 0, 0)
  Sponsorship  <- c(1, 0, 0, 0)  # GOV affects SPON
  Maturity     <- c(0, 0, 0, 0)
  Success      <- c(1, 1, 1, 0)  # GOV, SPON, MAT affect SUCCESS
  
} else {
  
  # define rows of the path matrix (for inner model) - no mediation
  Governance   <- c(0, 0, 0, 0)
  Sponsorship  <- c(0, 0, 0, 0)
  Maturity     <- c(0, 0, 0, 0)
  Success      <- c(1, 1, 1, 0)  # GOV, SPON, MAT affect SUCCESS
}


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

message("\n\n*** Running PLS-PM analysis...")

# run PLS-PM analysis
successPLS <- plspm(flossData,
                    successPath,
                    successBlocks,
                    modes = successModes) # scaling = successScales


# 4.2. Handling PLS-PM Results
##########################################################

message("\n\n*** SEM-PLS analysis results:\n")

# contents of the results object (what's available)
if (DEBUG) print(successPLS)

# summarized results
print(summary(successPLS), digits = DIGITS)


# 4.3. Measurement Model Assessment: Reflective Indicators
##########################################################


# plotting loadings
gLoadDiag <- plot(successPLS, what = "loadings",
                  box.prop = 2, box.cex = 1.5, cex.txt = 1.2)

# Not needed, since we have to call plot() method in .Rmd
if (KNITR) {
  plspm_var <- paste0("plspm_", datasetName)
  assign(plspm_var, successPLS, envir = .GlobalEnv)
}


# outer model results (in a matrix way, unlike tabular in summary())
print(successPLS$outer_model, digits = DIGITS)


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


# Governance outer model results
#print(subset(successPLS$outer_model, block == "Governance"))

# plotting weights
gWeights <- plot(successPLS, what = "weights",
                 box.prop = 2, box.cex = 1.5, cex.txt = 1.2)


# TODO: potential model's modifications/adjustments HERE
modSuccessBlocks <- successBlocks

# TODO: if made adjustments, uncomment below to re-estimate the model(s)
#modSuccessPLS <- plspm(flossData,
#                       successPath, modSuccessBlocks,
#                       modes = successModes)

# plot modified model's loadings
#gModLoadings <- plot(modSuccessPLS, "loadings")
#print(gModLoadings)

# unidimensionality - better results
#print(modSuccessPLS$unidim)

# loadings and communalities
#print(modSuccessPLS$outer_model)

# cross-loadings
#print(modSuccessPLS$crossloadings)


### The following visual output is for the original model
### (update, if analysis of several models is performed).

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

# matrix with values based on path coeffs
arrow_lwd <- 10 * round(successPLS$path_coefs, 2)

# visual: SEM path diagram (inner model)
plot(successPLS, arr.lwd = arrow_lwd)


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

# visual: LV effects diagram
# TODO: convert to ggplot2 version, rotate x-axis labels, etc.

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


# move inner model summary here?
# move GoF here?


# 4.6. Validation
##########################################################

if (DO_SEM_BOOT) {
  
  message("\n\n*** Performing bootstrap validation...")
  
  # running bootstrap validation (100 samples)
  successVal <- plspm(flossData, successPath, successBlocks,
                      modes = successModes,
                      boot.val = TRUE, br = 100)
  
  
  # bootstrap results
  print(successVal$boot, digits = DIGITS)
}


message("\n===== SEM-PLS analysis completed, results are ",
        "in directory \"", SEM_RESULTS_DIR, "\"\n")
