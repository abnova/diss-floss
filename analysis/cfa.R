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
if (!suppressMessages(require(RColorBrewer))) install.packages('RColorBrewer')

library(polycor)
library(lavaan)
library(tables)
library(xtable)
library(Hmisc)
library(semPlot)
library(qgraph)
library(RColorBrewer)


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

DEBUG <- FALSE


genCFAresultsTable <- function (caption = "CFA results summary",
                                type = "latex", file = "", digits = 2) {
  
  fit.info <-
    fitMeasures(cfa.fit)[c('chisq', 'df', 'pvalue', 'cfi', 'rmsea')]
  
  # customize labels for fit measures
  names(fit.info) <- c("$\\chi^{2}$", "df", "p", "CFI", "RMSEA")
  
  cfa.table <- cfaPrettyPrint(cfa.fit, digits = digits)
  numCols <- ncol(cfa.table)
  cfa.table <- cbind(cfa.table, "", "")
  colnames(cfa.table)[numCols + 1:2] <- c(" ", " ")
  
  numRows <- nrow(cfa.table)
  lines2Add <- 2
  cfa.table <- rbind(cfa.table, "1"="", "2"="")
  rownames(cfa.table)[numRows + 1:2] <- c("CFA fit measures", " ")
  
  horLines <- c(c(-1, 0, numRows), numRows + lines2Add)
  cfa.table[numRows + 1, ] <- names(fit.info)
  cfa.table[numRows + 2, ] <- round(fit.info, digits = digits)
  
  # table note/comment (decided to implement it as a part of caption)
  if (FALSE) {
    tabNote <- list()
    tabNote$pos <- list()
    tabNote$pos[[1]] <- c(nrow(cfa.table))
    tabNote$command <- paste0("Standard errors are shown in parentheses.",
                              "\\linebreak",
                              "* indicates significant results at 5% level.")
    #horLines <- c(horLines, horLines + 1)
  }
  
  #comment <- paste("Standard errors are shown in parentheses.",
  #                 "* indicates significant results at 5% level.")
  #caption <- paste(caption, comment)
  
  # generate 'xtable' object from CFA fit object
  cfaResultsTable <- xtable(cfa.table, caption = caption,
                            label = "cfaResultsTable")
  
  # output 'xtable' in requested format (LaTeX or HTML code)
  print(cfaResultsTable, type = type, file = file,
        booktabs = TRUE, digits = digits, comment = FALSE,
        caption.placement = "top",
        hline.after = horLines,
        #add.to.row = tabNote,
        sanitize.text.function = function(x) x)
}


# parameter 'latex' should be set to TRUE only for a call under KNITR

genCFAmodelDiagram <- function (cfa.fit, latex = FALSE) {
  
  filetype <- ifelse(.Platform$GUI == "RStudio", "x11", "R")
  
  # produce CFA model diagram/figure, using 'semPlot' package
  cfaModDiag <- semPaths(cfa.fit, whatLabels = "std",
                         intercepts = FALSE, thresholds = FALSE,
                         rotation = 4,    # H: factors on the right
                         #nCharNodes = 0, # 0 disables abbreviation
                         #label.cex = 5,
                         #vsize = 20,     # node size
                         edge.label.cex = 1.25,  # 1 is default
                         curve = 2,
                         curvature = 1.2,
                         sizeMan = 10,
                         sizeLat = 10,
                         sizeInt = 10,
                         filetype = filetype, standAlone = FALSE)
  
  numFactors <- length(grep("f[[:digit:]]",
                            unique(parameterEstimates(cfa.fit)$lhs),
                            value = TRUE, ignore.case = TRUE))
  
  # change color palette
  colPalette <- brewer.pal(5, "Pastel1")[1:numFactors]
  
  # fix for TikZ device (LaTeX output)
  if (latex) qgraph.tikz(cfaModDiag, filename = "cfaModDiag",
                         colors = colPalette, bg = "grey90",
                         width = 7, height = 4, # curveShape = -0.5,
                         standAlone = FALSE)
}


# Extracts loadings and LV covariances from lavaan's CFA fit object.
# Returns corresponding matrix with values as character strings.
# Significance (*'s) and standard errors are reported (in parentheses).
cfaPrettyPrint <- function(object, digits = getOption("digits")) {
  
  x <- parameterEstimates(object)
  lv <- unique(subset(x, op == "=~")$lhs)
  indicators <- unique(subset(x, op == "=~")$rhs)
  lv.covs <- subset(x, lhs %in% lv & rhs %in% lv & op == "~~")
  lv.covmat <- matrix(numeric(0),
                      nrow = length(lv), ncol = length(lv),
                      dimnames = list(lv, lv))
  
  for (i in seq_along(lv)) {
    for (i2 in seq_along(lv)) {
      tmp <-
        unique(na.omit(c(subset(lv.covs, lhs == lv[i] & rhs == lv[i2])$est,
                         subset(lv.covs, lhs == lv[i2] & rhs == lv[i])$est)))
      lv.covmat[i, i2] <- tmp
      lv.covmat[i2, i] <- tmp
    }
  }
  
  lv.covmat[] <- as.character(round(lv.covmat, digits))
  
  loadings.mat <- matrix("",
                         nrow = length(indicators), ncol = length(lv),
                         dimnames = list(indicators, lv))
  
  for (i in seq_along(lv)) {
    tmp <- subset(x, lhs == lv[i] & op == "=~")
    tmp <- tmp[, c("rhs", "est", "se", "pvalue")]
    vals <- with(tmp,
                 sprintf("%0.2f%s (%0.2f)", est,
                         symnum(pvalue,
                                cutpoints = c(0, .001, .01, .05, 1),
                                symbols = c("***", "**", "*", ""), na = ""),
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

if (DEBUG) {
  cfa.fit <- cfa(model, data = flossData, meanstructure = TRUE,
                 missing = "pairwise", estimator = "WLSMV")
} else {
  cfa.fit <- suppressWarnings(
    cfa(model, data = flossData, meanstructure = TRUE,
                 missing = "pairwise", estimator = "WLSMV")
    )
}

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


# determine output format, based on runtime environment
tab.format <- ifelse(.Platform$GUI == "RStudio", "html", "latex")
file <- ifelse(.Platform$GUI == "RStudio", tempfile(fileext = ".html"), "")

# produce CFA results summary table
cfa.table <- genCFAresultsTable(file = file, type = tab.format)

# preview HTML table, if in RStudio
if (tab.format == "html") rstudio::viewer(file)


# not needed, as long as table gen. function directly accesses 'cfa.table'
if (KNITR) {
  cfaTable_var <- paste0("cfaTable_", datasetName)
  assign(cfaTable_var, cfa.table, envir = .GlobalEnv)
}
