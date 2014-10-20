# Start session with a clean R environment
rm(list = ls(all.names = TRUE))

##### PACKAGES #####

if (!suppressMessages(require(psych))) install.packages('psych')
if (!suppressMessages(require(GPArotation))) 
  install.packages('GPArotation')
if (!suppressMessages(require(pcaPA))) install.packages('pcaPA')
if (!suppressMessages(require(ggplot2))) install.packages('ggplot2')

library(psych)
library(GPArotation)
library(pcaPA)
library(ggplot2)

##### SETUP #####

PRJ_HOME <- Sys.getenv("DISS_FLOSS_HOME") # getwd()

source(file.path(PRJ_HOME, "utils/data.R"))
source(file.path(PRJ_HOME, "utils/platform.R")) # for multi-core support

READY4EFA_DIR  <- file.path(PRJ_HOME, "data/ready4efa")
READY4EFA_FILE <- "flossData" # default

EFA_RESULTS_DIR <- file.path(PRJ_HOME, "results/efa")
SCREE_PLOT_FILE <- "screePlot"

RDS_EXT      <- ".rds"
GRAPHICS_EXT <- ".svg"


##### ANALYSIS #####

message("\n\n===== PERFORMING EXPLORATORY FACTOR ANALYSIS (EFA) =====")

fileName <- paste0(READY4EFA_FILE, RDS_EXT)
ready4efaFile <- file.path(READY4EFA_DIR, fileName)

# load data
message("\n\nLoading data...")
flossData <- loadData(ready4efaFile)

# due to very small amount of projects with "Non-OSI" licesnse
# and their disapperance due to calculating correlations,
# we don't include "License Category" into EFA (consider analyzing it
# at later phases with inclusion of imputed data)

# we also remove "Repo URL" due to injection of large # of NAs
# due to limiting conditionsat the end of the merge process

factors4Analysis <- c("Development Team Size", "Project Age",
                      "License Restrictiveness", "Project Stage",
                      "Software Type")
flossData <- flossData[factors4Analysis]

# sample the sample (use 1%) to reduce processing time
flossData <- sampleDF(flossData, nrow(flossData) / 100)

# first, calculate correlations for passing to FA functions

# use hetcor() from 'polycor' package instead of corr.test()
# in order to handle heterogenous data w/out conversion
corr.info <- hetcor(flossData, use="pairwise.complete.obs",
                    std.err = TRUE) # use $correlations just for corr

# extract number of observations for futher use in FA
numObs <- floor(mean(corr.info$n[upper.tri(corr.info$n)]))

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

# parallel analysis suggests the number of factors to extract
# (analytical alternative/complement to the scree plot solution)
fa.pa.info <- fa.parallel(corr.info$correlations,
                          n.obs = numObs, fm = "pa")

# extract number of factors from the PA result
numFactors <- fa.pa.info$nfact


message("\n\nVery Simple Structure (VSS) analysis:")
message("=====================================")

# Velicer’s minimum average partial (MAP)
vss.info <- VSS(corr.info$correlations, n.obs = numObs, plot = FALSE)
summary(vss.info)  # for more details, print object or str()

# To produce a scree plot (eigen values of a correlation matrix)
# we could use here scree() and VSS.scree(), but we will use
# ggplot2-based scree plot functions from 'pcaPA' package.

message("\nParallel Analysis (PA) - Method 2 ('pcaPA'):")
message("============================================\n")

# Run Parallel Analysis (PA) for numeric data and plot scree plot
# (the same result could be produced by using mixed data PA method)
numericPA <- PA(flossData,
                percentiles = c(0.95, 0.99), nReplicates = 50,
                type = "mixed", algorithm = "polycor",
                use = "pairwise.complete.obs")
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
fa(corr.info$correlations, n.obs = numObs,
   nfactors = numFactors, fm = "pa")

message("\n\nFA with 'promax' rotation:") # not varimax?
message("===========================\n")

#message("Currently disabled.")
# perform FA with 'promax' rotation
promax.fa <- fa(corr.info$correlations, n.obs = numObs,
                nfactors = numFactors, fm = "pa",
                rotate = "promax")
print(promax.fa$Structure)

message("\n\nFA with 'quartimin' rotation:")
message("=============================\n")

message("Currently disabled.")
# due to error "3 factors are too many for 5 variables"

# perform FA with 'quartimin' rotation
#quartimin <- factanal(corr.info$correlations, n.obs = numObs,
#                      factors = numFactors,
#                      rotation = "cfQ",
#                      control = list(rotate = list(kappa = 0)))
#print(quartimin, cutoff = 1e-05, digits = 2)

message("\n\nFA, using Schmid-Leiman transformation:")
message("=======================================\n")

message("Currently disabled.")
# due to (error) message "maximum iteration exceeded"

#schmid(corr.info$correlations, n.obs = numObs,
#       nfactors = numFactors, fm = "pa")


message("\n\n\nFA with 'bi-factor' rotation:")
message("=============================\n")

fa(corr.info$correlations, n.obs = numObs,
   nfactors = numFactors, fm="pa",
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
uls <- fa(corr.info$correlations, n.obs = numObs,
          nfactors = numFactors, rotate = "varimax")

# show the loadings sorted by absolute value
print(uls, sort=TRUE)

message("\n\nFA using WLS approach:")
message("======================\n")

# weighted least squares
wls <- fa(corr.info$correlations, n.obs = numObs,
          nfactors = numFactors, fm = "wls")

# show the loadings sorted by absolute value
print(uls, sort = TRUE)

message("\n\nFA using ML approach:")
message("=====================\n")

message("Currently disabled.")
# due to error "3 factors are too many for 5 variables"

# compare with a ML solution using factanal
#mle <- factanal(corr.info$correlations, n.obs = numObs,
#                factors = numFactors)
#print(factor.congruence(list(uls, wls, mle)))

message("\n===== EFA completed, results can be found ",
        "in directory \"", EFA_RESULTS_DIR, "\"\n")
