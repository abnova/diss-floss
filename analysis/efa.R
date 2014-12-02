# TODO: Add description and comments here

# For more details on bi-factor EFA and Schmid-Leiman method, see:
# http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3253271

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

# Nowadays, ULS or ML is preferred to PA methods of FA:
#
# Flora DB, LaBrish C and Chalmers RP. (2012).
# Old and new ideas for data screening and assumption testing
# for exploratory and confirmatory factor analysis.
# Front. Psychology 3:55. doi: 10.3389/fpsyg.2012.00055


# Start session with a clean R environment
## @knitr CleanEnv
rm(list = ls(all.names = TRUE))

##### PACKAGES #####

## @knitr LoadPackages
if (!suppressMessages(require(psych))) install.packages('psych')
if (!suppressMessages(require(GPArotation))) 
  install.packages('GPArotation')
if (!suppressMessages(require(polycor))) install.packages('polycor')
#if (!suppressMessages(require(pcaPA))) install.packages('pcaPA')
if (!suppressMessages(require(ggplot2))) install.packages('ggplot2')
if (!suppressMessages(require(tables))) install.packages('tables')
if (!suppressMessages(require(Hmisc))) install.packages('Hmisc') # for 'tables'
if (!suppressMessages(require(qgraph))) install.packages('qgraph')
if (!suppressMessages(require(RColorBrewer))) install.packages('RColorBrewer')
if (!suppressMessages(require(mice))) install.packages('mice')

library(psych)
library(GPArotation)
library(polycor)
#library(pcaPA)
library(ggplot2)
library(tables)
library(Hmisc)
library(qgraph)
library(RColorBrewer)
library(mice)


##### SETUP #####

## @knitr PrepareEFA

PRJ_HOME <- Sys.getenv("DISS_FLOSS_HOME") # getwd()

source(file.path(PRJ_HOME, "config/diss-floss-config.R"))
source(file.path(PRJ_HOME, "utils/data.R"))
source(file.path(PRJ_HOME, "utils/platform.R")) # for multi-core support
source(file.path(PRJ_HOME, "utils/graphics.R")) # for golden ratio
source(file.path(PRJ_HOME, "utils/knit.R"))

SCREE_PLOT_FILE <- "screePlot"

DEBUG <- FALSE  # local setting


# produce a rounded loadings matrix by setting loadings
# with absolute value lower than a cut-off value (0.3) to zero
roundLoadings <- function (fa.obj) {
  
  L <- as.matrix(fa.obj$loadings)
  L[abs(L) < .3] <- 0
  return (L)  
}


addHeader <- function(obj, newcol, factorNames) {

  a1 <- attr(obj, "colLabels")
  a2 <- rbind(a1, a1)
  a2[1, ] <- newcol
  a2[2, ] <- factorNames
  attr(a2, "justification") <- rbind(attr(a1, "justification"),
                                     attr(a1, "justification"))
  attr(a2, "formats") <- rbind(attr(a1, "formats"), attr(a1, "formats"))
  attr(obj, "colLabels") <- a2
  
  return (obj)
}


genEFAresultsTable <- function (label = "efaResults",
                                caption = "EFA results summary",
                                digits = 2, numFactors) {
  
  fa.pa <- roundLoadings(fa.pa)
  colnames(fa.pa) <- 
    paste0("fa.pa", '_', colnames(fa.pa))
  
  fa.promax <- roundLoadings(fa.promax)
  colnames(fa.promax) <- 
    paste0("fa.promax", '_', colnames(fa.promax))
  
  fa.bi <- roundLoadings(fa.bi)
  colnames(fa.bi) <- 
    paste0("fa.bi", '_', colnames(fa.bi))
  
  fa.uls <- roundLoadings(fa.uls)
  colnames(fa.uls) <- 
    paste0("fa.uls", '_', colnames(fa.uls))
  
  fa.wls <- roundLoadings(fa.wls)
  colnames(fa.wls) <- 
    paste0("fa.wls", '_', colnames(fa.wls))
  
  efaResultsMatrix <- cbind(fa.pa, fa.promax, fa.bi, fa.uls, fa.wls)

  # round matrices' values and clear zeroes to present clear staructure
  efaResultsMatrix <- round(efaResultsMatrix, digits)
  efaResultsMatrix[efaResultsMatrix == 0] <- ""
  efaResultsMatrix[efaResultsMatrix != ""] <- 
    gsub("(0)(\\..*)", "\\2", efaResultsMatrix[efaResultsMatrix != ""])

  # convert to 'tabular' object (using 'tables' package)
  efaResultsTable <- as.tabular(efaResultsMatrix)
  format(efaResultsTable, digits)
  
  # specify labels for FA methods
  methods <- c(c("Principal Axis", rep(NA, numFactors - 1)),
               c("Promax", rep(NA, numFactors - 1)),
               c("Bi-factor", rep(NA, numFactors - 1)),
               c("ULS", rep(NA, numFactors - 1)),
               c("WLS", rep(NA, numFactors - 1)))
  
  # generate factor names
  factorNames <- rep(as.character(1:numFactors),
                     length(unique(methods)) - 1)
  
  # add custom header (multicolumn format)
  efaResultsTable <- addHeader(efaResultsTable, methods, factorNames)
  
  # set the caption (specific for 'tables' package)
  latexCap <- paste0("\\caption{", caption, "}\\\\", "\n",
                     "\\toprule",
                     "\\label{tab:", label, "}")
  
  # set tabular settings
  booktabs()
  
  # output LaTeX table
  latex(efaResultsTable, options = list(tabular = "longtable",
                                        toprule = latexCap))
}


getBestEFAmodel <- function (models) {

  # compare determined EFA models (factor structure) to find the best-fitted one
  tli.info <- sapply(models, '[[', "TLI")
  chi.info <- sapply(models, '[[', "chi")
  fit.info <- sapply(models, '[[', "fit")
  
  tli.best <- which.max(tli.info)
  chi.best <- which.min(chi.info)
  fit.best <- which.min(fit.info)
  
  results <- round(rbind(tli.info, chi.info, fit.info), digits = 2)
  if (DEBUG) print(results)
  
  # currently use 'fit' as the ultimate model comparison criterion
  if (!KNITR) {
    print("Model comparison results in the following best-fitted model:\n")
    print(summary(models[[fit.best]]))
  }
  
  return (models[fit.best])
}


# TODO: implement when will have time (rows: fit indices, cols: rot. methods)
genEFAcomparisonTable <- function (label = "efaResults",
                                caption = "EFA results summary",
                                digits = 2, numFactors) {
  
}


# parameter 'latex' should be set to TRUE only for a call under KNITR

genEFAresultsDiagram <- function (models, best.fit = FALSE, latex = FALSE) {
  
  if (best.fit)
    models <- getBestEFAmodel(models)
  
  for (fa.obj in models) {
    
    standAlone <- FALSE
    filetype <- ifelse(.Platform$GUI == "RStudio", "x11", "R")
    
    # redefine output to LaTeX
    if (latex) filetype <- "tex"
  
    # specify groups to produce factors and indicators circles
    factors <- apply(abs(fa.obj$loadings), 1, which.max)
    faGroups <- vector("list", length(unique(factors)))
    
    for(i in 1:length(factors))
      faGroups[[factors[i]]] <- c(faGroups[[factors[i]]], i)
    
    # change color palette
    colPalette <- brewer.pal(5, "Pastel1")[1:length(unique(factors))]
    
    # prepare tooltips vector
    #tooltips <- c(unlist(attr(fa.obj$loadings, "dimnames")[1]), # indicator names
    #              unlist(attr(fa.obj$loadings, "dimnames")[2])) # factor names
    
    # TODO: re-arrange indicator names to match qgraph's ordering
    # (seems to be based on indicator loadings values)
    
    # TEMP
    tooltips <- c("Development Team Size", "License Restrictiveness",
                  "Project Age", "Project Stage", "Software Type",
                  "Factor 1", "Factor 2", "Factor 3")
    
    # 'qgraph' defaults to 'circular' layout (alternative: 'groups')
    qgraph(fa.obj$loadings, groups = faGroups,
           edge.labels = TRUE,
           #tooltips = tooltips,
           colors = colPalette, bg = "grey90",
           filetype = filetype, standAlone = standAlone,
           mar = c(2.5, 2.5, 2.5, 2.5)) # B/L/T/R
  }
}


## @knitr PerformEFA

##### ANALYSIS #####

message("\n\n===== PERFORMING EXPLORATORY FACTOR ANALYSIS (EFA) =====")

fileName <- paste0(READY4EFA_FILE, RDS_EXT)
ready4efaFile <- file.path(READY4EFA_DIR, fileName)

# Create results directory, if it doesn't exist
if (!file.exists(EFA_RESULTS_DIR)) {
  dir.create(EFA_RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)
}

# load data
message("\n\n*** Loading data...")
flossData <- loadData(ready4efaFile)

# select imputed dataset
flossData <- mice::complete(flossData, 1)

# due to very small amount of projects with "Non-OSI" licesnse
# and their disapperance due to calculating correlations,
# we don't include "License Category" into EFA (consider analyzing it
# at later phases with inclusion of imputed data)
# FIXED THE ABOVE-MENTIONED PROBLEM BY USING IMPUTED DATA

# we also remove "Repo URL" due to injection of large # of NAs
# due to limiting conditionsat the end of the merge process

factors4analysis <- c("License.Category", "License.Restrictiveness",
                      "Preferred.Support.Type", "Preferred.Support.Resource",
                      "Project.Age", "Project.Stage",
                      "Development.Team.Size", "User.Community.Size")

flossData <- flossData[, factors4analysis]

# sample the sample (use 1%) to reduce processing time
#flossData <- sampleDF(flossData, nrow(flossData) / 100)

# save name of the data set (seems redundant, but it will be useful,
# when there will be more than one data set, i.e. 'pilot' and 'main')
# [currently used for KNITR only]
datasetName <- deparse(substitute(flossData))

# first, calculate correlations for passing to FA functions

# use hetcor() from 'polycor' package instead of corr.test()
# in order to handle heterogenous data w/out conversion
message("\n*** Calculating correlations...")
corr.info <- hetcor(flossData, use="pairwise.complete.obs",
                    std.err = TRUE) # use $correlations just for corr

if (DEBUG) {
  message("\nCorrelations matrix:")
  message("--------------------")
  print(corr.info, digits = 2)
}

# extract number of observations for futher use in FA
numObs <- floor(mean(corr.info$n[upper.tri(corr.info$n)]))

# determine number of factors to extract
message("\n*** Determining number of factors to extract...\n")

message("\nParallel Analysis (PA) - Method 1 ('psych'):")
message("============================================\n")

# parallel analysis suggests the number of factors to extract
# (analytical alternative/complement to the scree plot solution)
fa.pa.info <- fa.parallel(corr.info$correlations,
                          n.obs = numObs, fm = "pa")

# extract number of factors from the PA result
numFactors <- fa.pa.info$nfact

message("\nProducing PA scree plot... ", appendLF = FALSE)

screePlotData <- with(fa.pa.info,
  data.frame(Eigen = c(fa.values, fa.sim),
             Data = factor(rep(1:2, each = length(fa.values)),
                           levels = 1:2,
                           labels = c("Observed", "Simulated")),
             Factor = rep(1:length(fa.values)))
)

g <- ggplot(screePlotData, aes(x = Factor, y = Eigen, color = Data)) +
  geom_line() +
  xlab("Factor numbers") + ylab("Factor eigenvalues") +
  theme(title = element_text(size = 10),
        legend.position = "top",
        plot.margin = unit(c(1, 1, 1, 1), "mm"))

screePlot <- g + theme(aspect.ratio = 1 / PHI)

if (KNITR) {
  screePlot_var <- paste0("screePlot_", datasetName)
  assign(screePlot_var, screePlot, envir = .GlobalEnv)
}

if (.Platform$GUI == "RStudio") {print(screePlot)}

screePlotFile <- file.path(EFA_RESULTS_DIR,
                           paste0(SCREE_PLOT_FILE, GRAPHICS_EXT))
suppressMessages(ggsave(file = screePlotFile, plot = screePlot,
                        width = 4, height = 4))
message("Done.")


message("\n\nVery Simple Structure (VSS) analysis:")
message("=====================================")

# Velicer’s minimum average partial (MAP)
vss.info <- VSS(corr.info$correlations, n.obs = numObs, plot = FALSE)

# KNITR: do not forget to apply summary() or other functions to such objects
if (KNITR) {
  vssInfo_var <- paste0("vssInfo_", datasetName)
  assign(vssInfo_var, vss.info, envir = .GlobalEnv)
} else {
  vss.summ <- summary(vss.info)  # for more details, print object or str()
}


# To produce a scree plot (eigen values of a correlation matrix)
# we could use here scree() and VSS.scree(), but we will use
# ggplot2-based scree plot functions from 'pcaPA' package.

message("\nParallel Analysis (PA) - Method 2 ('pcaPA'):")
message("============================================\n")

message("Currently disabled.")

if (FALSE) {
  
  # Run Parallel Analysis (PA) for numeric data and plot scree plot
  # (the same result could be produced by using mixed data PA method)
  numericPA <- PA(flossData,
                  percentiles = c(0.95, 0.99), nReplicates = 50,
                  type = "mixed", algorithm = "polycor",
                  use = "pairwise.complete.obs")
  print(numericPA)
  
  per99Val <- subset(numericPA$percentiles, typeEigenValues == 99)$eigenValues
  obsOrdEigenVal <- numericPA$observed$orderEigenValues
  obsEigenVal <- numericPA$observed$eigenValues
  
  numFactorsPcaPA <- max(obsOrdEigenVal[obsEigenVal > per99Val])
  message("\nNumber of factors, determined by PCA PA: ", numFactorsPcaPA)
  
  message("\n\nProducing PA scree plot... ", appendLF = FALSE)
  
  screePlot <- plot(numericPA, percentiles = c(0.95, 0.99),
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
  message("Done.\n")
}


# we don't need to compare anymore, as PCA PA is disabled
if (FALSE) {
  
  if (numFactors == numFactorsPcaPA) {
    message("PA, using 'psych' and 'pcaPA' packages, suggest\n",
            "the same number of factors to be extracted: ", numFactors)
  } else {
    message("Number of factors to be extracted, determined, ",
            "using 'psych' and 'pcaPA' packages, differs:\n")
    message("'psych': ", numFactors)
    message("'pcaPA': ", numFactorsPcaPA)
  }
}


message("\n\n*** Performing factor analysis (FA)...\n\n")

message("FA, using principal axis method:")
message("================================\n")

# perform FA, using principal axis method
fa.pa <- fa(corr.info$correlations, n.obs = numObs,
            nfactors = numFactors, fm = "pa")
if (DEBUG) print(fa.pa)

L <- roundLoadings(fa.pa)

message("\nRounded loadings matrix:")
message("------------------------")
print(L)

if (KNITR) {
  faPA_var <- paste0("faPA_", datasetName)
  assign(faPA_var, fa.pa, envir = .GlobalEnv)
}


message("\n\nFA with 'promax' rotation:")
message("===========================\n")

#message("Currently disabled.")
# perform FA with 'promax' rotation
fa.promax <- fa(corr.info$correlations, n.obs = numObs,
                nfactors = numFactors, fm = "pa",
                rotate = "promax")
if (DEBUG) print(fa.promax)

L <- roundLoadings(fa.promax)

message("\nRounded loadings matrix:")
message("------------------------")
print(L)

if (KNITR) {
  faPromax_var <- paste0("faPromax_", datasetName)
  assign(faPromax_var, fa.promax, envir = .GlobalEnv)
}


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


message("\n\nFA with 'bi-factor' rotation:")
message("============================\n")

fa.bi <- fa(corr.info$correlations, n.obs = numObs,
            nfactors = numFactors, fm="pa",
            rotate = "bifactor", max.iter = 500)
if (DEBUG) print(fa.bi, sort = TRUE)

L <- roundLoadings(fa.bi)

if (KNITR) {
  faBi_var <- paste0("faBi_", datasetName)
  assign(faBi_var, fa.bi, envir = .GlobalEnv)
}


message("\nRounded loadings matrix:")
message("------------------------")
print(L)


message("\n\nFA using ULS approach:")
message("=====================\n")

# unweighted least squares is minres
fa.uls <- fa(corr.info$correlations, n.obs = numObs,
             nfactors = numFactors, rotate = "varimax")

# show the loadings sorted by absolute value
if (DEBUG) print(fa.uls, sort = TRUE)

L <- roundLoadings(fa.uls)

if (KNITR) {
  faULS_var <- paste0("faULS_", datasetName)
  assign(faULS_var, fa.uls, envir = .GlobalEnv)
}


message("\nRounded loadings matrix:")
message("------------------------")
print(L)


message("\n\nFA using WLS approach:")
message("======================\n")

# weighted least squares
fa.wls <- fa(corr.info$correlations, n.obs = numObs,
             nfactors = numFactors, fm = "wls")

# show the loadings sorted by absolute value
if (DEBUG) print(fa.wls, sort = TRUE)

L <- roundLoadings(fa.wls)

if (KNITR) {
  faWLS_var <- paste0("faWLS_", datasetName)
  assign(faWLS_var, fa.wls, envir = .GlobalEnv)
}


message("\nRounded loadings matrix:")
message("------------------------")
print(L)


message("\n\nFA using ML approach:")
message("=====================\n")

message("Currently disabled.")
# due to error "3 factors are too many for 5 variables"

# compare with a ML solution using factanal
#mle <- factanal(corr.info$correlations, n.obs = numObs,
#                factors = numFactors)
#print(factor.congruence(list(uls, wls, mle)))


# generate EFA diagrams and output them in RStudio 'Plots' panel

# TODO: check if other 'graph' values make more sense (req. corr. matrix):
# "association" (default), "concentration", "factorial", "sig", "sig2"

# TODO: Consider whether it's better to pass diag. vec. & loop inside

# list of EFA models, for which diagrams/comparison should be produced
efaModels <- list(fa.pa, fa.promax, fa.bi, fa.uls, fa.wls)

# produce all requested diagrams (for appendix)
genEFAresultsDiagram(efaModels)

# determine the best-fitted EFA model
getBestEFAmodel(efaModels)

# produce the best-fitted diagram (for manuscript's body)
genEFAresultsDiagram(efaModels, TRUE)


message("\n===== EFA completed, results can be found ",
        "in directory \"", EFA_RESULTS_DIR, "\"\n")
