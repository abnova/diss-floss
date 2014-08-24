# Start session with a clean R environment
rm(list = ls(all.names = TRUE))

if (!suppressMessages(require(psych))) install.packages('psych')
if (!suppressMessages(require(GPArotation))) 
  install.packages('GPArotation')
if (!suppressMessages(require(pcaPA))) install.packages('pcaPA')
if (!suppressMessages(require(ggplot2))) install.packages('ggplot2')

library(psych)
library(GPArotation)
library(pcaPA)
library(ggplot2)

source("../utils/data.R")

READY4EFA_DIR  <- "~/diss-floss/data/ready4efa"
READY4EFA_FILE <- "flossData" # default

EFA_RESULTS_DIR <- "../results/efa"
SCREE_PLOT_FILE <- "screePlot"

RDS_EXT      <- ".rds"
GRAPHICS_EXT <- ".svg"


message("\n\n===== PERFORMING EXPLORATORY FACTOR ANALYSIS (EFA) =====")

fileName <- paste0(READY4EFA_FILE, RDS_EXT)
ready4efaFile <- file.path(READY4EFA_DIR, fileName)

# load data
message("\n\nLoading data...")
flossData <- loadData(ready4efaFile)

# determine number of factors to extract
message("\n\n*** Determining number of factors to extract...\n")

# References to support Parallel Analysis (PA):
# ---------------------------------------------
# Horn, J. L. (1965). A rationale and test for the number of factors
# in factor analysis. Psychometrika, 30, 179–185.
#
# Glorfeld, L. W. (1995). An Improvement on Horn's Parallel Analysis
# Methodology for Selecting the Correct Number of Factors to Retain.
# Educational and Psychological Measurement, 55(3), 377–393.

# See http://www.unt.edu/rss/class/Jon/R_SC/Module7/M7_PCAandFA.R
# for an approach to do PA without displaying standard scree plots.

message("\nParallel Analysis (PA) - Method 1 ('psych'):")
message("============================================\n")

# parallel analysis suggests the
# number of factors to extract
fa.parallel(flossData, fm = "pa")

message("\n\nVery Simple Structure (VSS) analysis:")
message("=====================================")

# Velicer’s minimum average partial (MAP)
VSS(flossData)

# To produce a scree plot (eigen values of a correlation matrix)
# we could use here scree() and VSS.scree(), but we will use
# ggplot2-based scree plot functions from 'pcaPA' package.

message("\nParallel Analysis (PA) - Method 2 ('pcaPA'):")
message("============================================\n")

# Run Parallel Analysis (PA) for numeric data and plot scree plot
# (the same result could be produced by using mixed data PA method)
numericPA <- PA(flossData,
               percentiles = c(0.95, 0.99), nReplicates = 100,
               type = "continuous", algorithm = "pearson")
print(numericPA)

message("\n\nProducing PA scree plot:")
message("========================")

screePlot <- plot(numericPA, percentiles = 0.99,
                  main = "Parallel analysis scree plot",
                  xlab = "Number of factors",
                  ylab = "Eigenvalues",
                  groupLabel = "")
screePlot <- screePlot + theme(aspect.ratio = 1)

if (.Platform$GUI == "RStudio") {print(screePlot)}

screePlotFile <- file.path(EFA_RESULTS_DIR,
                           paste0(SCREE_PLOT_FILE, GRAPHICS_EXT))
suppressMessages(ggsave(file = screePlotFile, plot = screePlot,
                        width = 5, height = 5))


# TODO: automate passing determined number of factors

message("\n\n*** Performing factor analysis (FA)...\n\n")

message("FA, using principal axis method:")
message("================================\n")

# perform FA, using principal axis method
fa(flossData, nfactors = 2, fm = "pa")

message("\n\nFA with 'varimax' rotation:")
message("===========================\n")

message("Currently disabled.")
# perform FA with 'varimax' rotation
#promax.fa <- fa(flossData, nfactors = 2, fm = "pa",
#                rotate = "promax")
#print(promax.fa$Structure)

message("\n\nFA with 'quartimin' rotation:")
message("=============================\n")

message("Currently disabled.")
# perform FA with 'quartimin' rotation
#quartimin <- factanal(flossData, factors = 2,
#                      rotation = "cfQ",
#                      control = list(rotate = list(kappa = 0)))
#print(quartimin, cutoff = 1e-05, digits = 2)

message("\n\nFA, using Schmid-Leiman transformation:")
message("=======================================\n")

schmid(flossData, nfactors = 2)

message("\n\n\nFA with 'bi-factor' rotation:")
message("=============================\n")

fa(flossData, nfactors = 3, fm="pa",
   rotate = "bifactor", max.iter = 500)

# Nowadays, ULS or ML is preferred to PA methods of FA
# References:
# 1) Flora DB, LaBrish C and Chalmers RP. (2012).
# Old and new ideas for data screening and assumption testing
# for exploratory and confirmatory factor analysis.
# Front. Psychology 3:55. doi: 10.3389/fpsyg.2012.00055

message("\n\n\nFA using ULS approach:")
message("======================\n")

# unweighted least squares is minres
uls <- fa(flossData, 2, rotate = "varimax")

# show the loadings sorted by absolute value
print(uls,sort=TRUE)

message("\n\nFA using WLS approach:")
message("======================\n")

# weighted least squares
wls <- fa(flossData, 2, fm = "wls")

# show the loadings sorted by absolute value
print(uls, sort = TRUE)

message("\n\nFA using ML approach:")
message("=====================\n")

message("Currently disabled.")
# Due to: "Error in cov.wt(z) : 'x' must contain finite values only"

# compare with a ML solution using factanal
#mle <- factanal(flossData, factors = 2)
#factor.congruence(list(uls, wls, mle))

message("")
