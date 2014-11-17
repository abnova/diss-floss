# ===== TODO: =====
# consider producing outer/inner model diagram figures,
# using 'semPlot' package (see semPaths() functon's use in CFA module)

# Start session with a clean R environment
rm(list = ls(all.names = TRUE))

if (!suppressMessages(require(plspm))) install.packages('plspm')
if (!suppressMessages(require(mice))) install.packages('mice')
if (!suppressMessages(require(ggplot2))) install.packages('ggplot2')
if (!suppressMessages(require(RColorBrewer)))
  install.packages('RColorBrewer')
if (!suppressMessages(require(reshape))) install.packages('reshape')

library(plspm)
library(mice)
library(ggplot2)
library(RColorBrewer)
library(reshape)


##### PREPARATION & DEFINITIONS #####

PRJ_HOME <- Sys.getenv("DISS_FLOSS_HOME")

source(file.path(PRJ_HOME, "config/diss-floss-config.R"))
source(file.path(PRJ_HOME, "utils/data.R"))
source(file.path(PRJ_HOME, "utils/platform.R"))
source(file.path(PRJ_HOME, "utils/qgraphtikz.R")) # fix for TikZ device
source(file.path(PRJ_HOME, "utils/factors.R"))

LOADINGS_THRESHOLD <- 0.7  # minimum value for acceptable loadings

COLOR_PALETTE <- brewer.pal(8, "Set2") # OR "Accent"

GGPLOT2_PALETTE_FILL <- scale_fill_manual(values = COLOR_PALETTE)
GGPLOT2_PALETTE_LINE <- scale_color_manual(values = COLOR_PALETTE)

DEBUG <- FALSE  # local setting


## @knitr PerformSEM

##### ANALYSIS #####

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

# "Project.Stage" is used instead of "Project.Maturity" (Dev.Stage ???)
# "Project.License" is excluded from analysis, as it's unordered factor
factors4analysis <- c("License.Restrictiveness", "Project.Stage",
                      "Project.Age", "Development.Team.Size",
                      "User.Community.Size")
flossData <- flossData[, factors4analysis]

# save name of the data set (seems redundant, but it will be useful,
# when there will be more than one data set, i.e. 'pilot' and 'main')
# [currently used for KNITR only]
datasetName <- deparse(substitute(flossData))


message("\n\n*** Transforming data...")

# convert to unordered factors

flossData[["License.Restrictiveness"]] <- 
  as.integer(flossData[["License.Restrictiveness"]])
flossData[["Project.Stage"]] <- 
  as.integer(flossData[["Project.Stage"]])

if (FALSE) {
  flossData[["License.Restrictiveness"]] <-
    factor(flossData[["License.Restrictiveness"]], ordered = FALSE)
  flossData[["Project.Stage"]] <-
    factor(flossData[["Project.Stage"]], ordered = FALSE)
}


# Initial model specification
##########################################################

message("\n\n*** Building model...")

# define rows of the path matrix (for inner model)
Governance  <- c(1, 0, 0)
Sponsorship <- c(1, 0, 0)
Success     <- c(1, 1, 0)

# build the inner model matrix
successPath <- rbind(Governance, Sponsorship, Success) 

# add column names
colnames(successPath) <- rownames(successPath)

# specify blocks of indicators (outer model), using variable names

depVar <- "User.Community.Size"

blockGovernance <- c("License.Restrictiveness", "Project.Age")
blockSponsorship <- c("Project.Stage", "Development.Team.Size")
# TODO
#blockControl <- c("Development.Team.Size")
blockSuccess <- depVar

# build list of blocks (outer model)
successBlocks <- list(blockGovernance, blockSponsorship, blockSuccess)

# specify model's vector of modes ('A' is reflective)
successModes <- rep("A", 3)

##temp - begin
# convert factors to numeric values
# convert factors to integers via as.numeric.factor() ["factors.R"]
# the above doesn't work - however, as.integer() works just fine

if (FALSE)
for (x in factors4analysis)
  flossData[[x]] <- as.integer(flossData[[x]])

if (FALSE)
for (x in factors4analysis)
  flossData[[x]] <- as.numeric.factor(flossData[[x]])

#print(str(flossData))
##temp - end

# specify measurement scale for manifest variables
successScales <- list(c("ord", "num"), c("ord", "num"), c("num"))

message("\n\n*** Running PLS-PM analysis...")

# run PLS-PM analysis
successPLS <- plspm(flossData,
                    successPath,
                    successBlocks,
                    modes = successModes,
                    maxiter = 500) # scaling = successScales


# 4.2. Handling PLS-PM Results
##########################################################

message("\n\n*** SEM-PLS analysis results:\n")

# contents of the results object
print(successPLS)

# summarized results
print(summary(successPLS))


# 4.3. Measurement Model Assessment: Reflective Indicators
##########################################################

# plotting loadings
gLoadings <- plot(successPLS, what = "loadings")
print(gLoadings)

# outer model results (in a matrix way, unlike tabular in summary())
print(successPLS$outer_model)

# display barchart of loadings with threshold value line

gLoadingsBarChart <- ggplot(data = successPLS$outer_model,
                            aes(x = name, y = loading, fill = block)) +
  
  geom_bar(stat = 'identity', position = 'dodge') +
  
  # threshold line (to emphasize acceptable loadings)
  geom_hline(yintercept = LOADINGS_THRESHOLD, color = 'red') +
  
  # rotate x-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black")) +
  
  # add title
  ggtitle("Barchart of Loadings") #+
  
  # change color palette
  #GGPLOT2_PALETTE_FILL + GGPLOT2_PALETTE_LINE

print(gLoadingsBarChart)

# Governance outer model results
print(subset(successPLS$outer_model, block == "Governance"))

# plotting weights
gWeights <- plot(successPLS, what = "weights")
print(gWeights)

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
  
  # tweak some graphical elements
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
        line = element_blank(),
        plot.title = element_text(size = 12)) +
  
  # add title
  ggtitle("Crossloadings") +

  # change color palette
  GGPLOT2_PALETTE_FILL + GGPLOT2_PALETTE_LINE

# display the crossloadings barchart panel
print(gCrossLoadBlocks)


# 4.4. Measurement Model Assessment: Formative Indicators
##########################################################


# 4.5. Structural Model Assessment
##########################################################

# inner model
print(successPLS$inner_model)

# matrix of path coefficients
print(successPLS$path_coefs)

# inner model summary
print(successPLS$inner_summary)

# select R2
print(successPLS$inner_summary[, "R2", drop = FALSE])

# GoF index
print(successPLS$gof)

# matrix with values based on path coeffs
arrow_lwd <- 10 * round(successPLS$path_coefs, 2)

# visual: SEM path diagram (inner model)
plot(successPLS, arr.lwd = arrow_lwd)  # arr.pos = 


## Effects Analysis

# effects summary (don't use summary() here)
print(successPLS$effects)

# select effects ('active' rows)
activeRows <- na.omit(successPLS$effects[successPLS$effects[, -1] != 0, ])
activeRows <- as.integer(rownames(activeRows))

# 'active' effects in matrix format
path_effs <- as.matrix(successPLS$effects[activeRows, -1])

# add rownames to path_effs
rownames(path_effs) <- successPLS$effects[activeRows, 1]

# active effects summary
print(path_effs)

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

message("\n\n*** Performing bootstrap validation...")

# running bootstrap validation (100 samples)
successVal <- plspm(flossData, successPath, successBlocks,
                    modes = successModes,
                    boot.val = TRUE, br = 100)


# bootstrap results
print(successVal$boot)

message("\n===== SEM-PLS analysis completed, results can be found ",
        "in directory \"", SEM_RESULTS_DIR, "\"\n")
