# Start session with a clean R environment
## @knitr CleanEnv
rm(list = ls(all.names = TRUE))

##### PACKAGES #####

## @knitr LoadPackages
if (!suppressMessages(require(polycor))) install.packages('polycor')
if (!suppressMessages(require(lavaan))) install.packages('lavaan')
if (!suppressMessages(require(tables))) install.packages('tables')
if (!suppressMessages(require(xtable))) install.packages('xtable')
if (!suppressMessages(require(Hmisc))) install.packages('Hmisc') # for 'tables'
if (!suppressMessages(require(qgraph))) install.packages('semPlot')
if (!suppressMessages(require(qgraph))) install.packages('qgraph') # for 'semPlot'

library(polycor)
library(lavaan)
library(tables)
library(xtable)
library(Hmisc)
library(semPlot)
library(qgraph)


##### SETUP #####

## @knitr PrepareCFA
set.seed(100)

PRJ_HOME <- Sys.getenv("DISS_FLOSS_HOME")

KNITR <<- isTRUE(getOption("knitr.in.progress"))

source(file.path(PRJ_HOME, "utils/data.R"))
source(file.path(PRJ_HOME, "utils/platform.R"))
source(file.path(PRJ_HOME, "utils/qgraphtikz.R")) # fix for TikZ device

READY4CFA_DIR  <- file.path(PRJ_HOME, "data/ready4cfa")
READY4CFA_FILE <- "flossData"

CFA_RESULTS_DIR <- file.path(PRJ_HOME, "results/cfa")

RDS_EXT      <- ".rds"
GRAPHICS_EXT <- ".svg"

DEBUG <- TRUE


genCFAresultsTable <- function (caption="CFA results summary",
                                digits = 2) {
  
  fit.info <- fitMeasures(cfa.fit)[c('chisq', 'df', 'pvalue', 'cfi', 'rmsea')]
  
  cfa.table <- cfa.prettyprint(cfa.fit, digits = digits)
  numCols <- ncol(cfa.table)
  cfa.table <- cbind(cfa.table, "", "")
  colnames(cfa.table)[numCols + 1:2] <- c(" ", " ")
  
  numRows <- nrow(cfa.table); lines2Add <- 2
  cfa.table <- rbind(cfa.table, "1"="", "2"="")
  rownames(cfa.table)[numRows + 1:2] <- c("CFA fit measures", " ")
  
  horLines <- c(c(-1, 0, numRows), numRows + lines2Add)
  cfa.table[numRows + 1, ] <- names(fit.info)
  cfa.table[numRows + 2, ] <- round(fit.info, digits = digits)
  
  cfaResultsTable <- xtable(cfa.table, caption = caption)
  print(cfaResultsTable, booktabs = TRUE,
        digits = digits, comment = FALSE,
        caption.placement = "top",
        hline.after = horLines)
}


# parameter 'latex' should be set to TRUE only for a call under KNITR

genCFAmodelDiagram <- function (cfa.fit, latex = FALSE) {
  
  filetype <- ifelse(.Platform$GUI == "RStudio", "x11", "R")
  
  # produce CFA model diagram/figure, using 'semPlot' package
  cfaModDiag <- semPaths(cfa.fit, whatLabels = "std",
                         intercepts = FALSE, thresholds = FALSE,
                         edge.label.cex = 1,
                         filetype = filetype, standAlone = FALSE)
  print(cfaModDiag)
  
  # fix for TikZ device (LaTeX output)
  if (latex) qgraph.tikz(cfaModDiag, filename = "cfaModDiag",
                         width = 7, height = 4, curveShape = -0.5,
                         standAlone = FALSE)
}


cfa.prettyprint <- function(object, digits = getOption("digits")) {
  x <- parameterEstimates(object)
  lv <- unique(subset(x, op == "=~")$lhs)
  indicators <- unique(subset(x, op == "=~")$rhs)
  lv.covs <- subset(x, lhs %in% lv & rhs %in% lv & op == "~~")
  lv.covmat <- matrix(numeric(0), nrow = length(lv), ncol = length(lv),
                      dimnames = list(lv, lv))
  
  for (i in seq_along(lv)) {
    for (i2 in seq_along(lv)) {
      tmp <- unique(na.omit(c(subset(lv.covs, lhs == lv[i] & rhs == lv[i2])$est,
                              subset(lv.covs, lhs == lv[i2] & rhs == lv[i])$est)))
      lv.covmat[i, i2] <- tmp
      lv.covmat[i2, i] <- tmp
    }
  }
  
  lv.covmat[] <- as.character(round(lv.covmat, digits))
  
  loadings.mat <- matrix("", nrow = length(indicators), ncol = length(lv),
                         dimnames = list(indicators, lv))
  
  for (i in seq_along(lv)) {
    tmp <- subset(x, lhs == lv[i] & op == "=~")[, c("rhs", "est", "se", "pvalue")]
    vals <- with(tmp, sprintf("%0.2f%s (%0.2f)", est,
                              symnum(pvalue, cutpoints = c(0, .001, .01, .05, 1), symbols = c("***", "**", "*", ""), na = ""),
                              se))
    
    loadings.mat[tmp$rhs, i] <- vals
  }
  
  rbind(loadings.mat, lv.covmat)
}


## @knitr PerformCFA

##### ANALYSIS #####

message("\n\n===== PERFORMING CONFIRMATORY FACTOR ANALYSIS (CFA) =====")

fileName <- paste0(READY4CFA_FILE, RDS_EXT)
ready4cfaFile <- file.path(READY4CFA_DIR, fileName)

# load data
message("\n\n*** Loading data...")
flossData <- loadData(ready4cfaFile)

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

# convert names (temp)
names(flossData) <- make.names(names(flossData))

# sample the sample (use 1%) to reduce processing time
#flossData <- sampleDF(flossData, nrow(flossData) / 100)

# save name of the data set (seems redundant, but it will be useful,
# when there will be more than one data set, i.e. 'pilot' and 'main')
# [currently used for KNITR only]
datasetName <- deparse(substitute(flossData))

# log transform continuous data
flossData["Project.Age"] <- log(flossData["Project.Age"])
flossData["Development.Team.Size"] <- log(flossData["Development.Team.Size"])

# ===

# calculate standard deviation for each variable in the data set
# (was needed for cor2cov(), left for informative purposes)
#sdValues <- unlist(lapply(seq(length(names(flossData))),
#                          function(i) sd(flossData[[i]], na.rm = TRUE)))


# specify the latent variable model

model <- "

# factor structure (fix factors' loadings to 1)

f1 =~ 1 * Development.Team.Size
f2 =~ 1 * License.Restrictiveness
f3 =~ 1 * Project.Age + Software.Type

# fix some variances/residual variances to zero
# because < 3 indicators per latent variables

# Changed from original specification due to lavaan not analyzing
# categorical variables, so the following indicators were removed:
# License.Restrictiveness, Project.Age, Software.Type

Development.Team.Size ~~ 0 * Development.Team.Size
f2 ~~ 1 * f2

# covariances between the latent variables

f1 ~~ f2
f1 ~~ f3
f2 ~~ f3
"

# perform CFA
message("\n*** Performing CFA of the model...")

cfa.fit <- cfa(model, data = flossData, meanstructure = TRUE,
           missing = "pairwise", estimator = "WLSMV")

# KNITR: do not forget to apply summary() or other functions to such objects
if (KNITR) {
  cfaFit_var <- paste0("cfaFit_", datasetName)
  assign(cfaFit_var, cfa.fit, envir = .GlobalEnv)
} else {
  # output results
  summary(cfa.fit, fit.measures = TRUE, standardize = TRUE)
}

# produce CFA model diagram and output it in RStudio environment
genCFAmodelDiagram(cfa.fit)


# produce CFA results summary table
cfa.table <- genCFAresultsTable()

# not needed, as long as table gen. function directly accesses 'cfa.table'
if (KNITR) {
  cfaTable_var <- paste0("cfaTable_", datasetName)
  assign(cfaTable_var, cfa.table, envir = .GlobalEnv)
}
