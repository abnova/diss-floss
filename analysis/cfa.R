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
if (!suppressMessages(require(pander))) install.packages('pander')
if (!suppressMessages(require(mice))) install.packages('mice')

library(polycor)
library(lavaan)
library(tables)
library(xtable)
library(Hmisc)
library(semPlot)
library(qgraph)
library(RColorBrewer)
library(pander)
library(mice)


##### SETUP #####

## @knitr PrepareCFA

PRJ_HOME <- Sys.getenv("DISS_FLOSS_HOME")

source(file.path(PRJ_HOME, "config/diss-floss-config.R"))
source(file.path(PRJ_HOME, "utils/data.R"))
source(file.path(PRJ_HOME, "utils/platform.R"))
source(file.path(PRJ_HOME, "utils/qgraphtikz.R")) # fix for TikZ device
source(file.path(PRJ_HOME, "utils/knit.R"))

DEBUG <- FALSE  # local setting


genCFAresultsTable <- function (caption = "CFA results summary",
                                type = "latex", file = "",
                                digits = 2, label = "cfaResultsTable") {
  
  fit.info <-
    fitMeasures(cfa.fit)[c('chisq', 'df', 'pvalue', 'cfi', 'rmsea')]
  
  # customize labels for fit measures
  names(fit.info) <- c("$\\chi^{2}$", "df", "p", "CFI", "RMSEA")
  
  cfa.table <- cfaPrettyPrint(cfa.fit, digits = digits)
  
  numCols <- ncol(cfa.table)
  numColsToAdd <- length(fit.info) - ncol(cfa.table)
  colsToAdd <- matrix(NA, nrow(cfa.table), numColsToAdd)
  cfa.table <- cbind(cfa.table, colsToAdd)
  colnames(cfa.table)[numCols + 1:numColsToAdd] <- rep(" ", numColsToAdd)
  
  numRows <- nrow(cfa.table)
  lines2Add <- 2
  cfa.table <- rbind(cfa.table, "1"="", "2"="")
  rownames(cfa.table)[numRows + 1:2] <- c("CFA fit measures", " ")
  
  horLines <- c(c(-1, 0, numRows), numRows + lines2Add)
  cfa.table[numRows + 1, ] <- names(fit.info)
  cfa.table[numRows + 2, ] <- round(fit.info, digits = digits)

  if (type == 'msword') {
    return (pandoc.table(cfa.table))
  }
  
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
  cfaResultsTable <- xtable(cfa.table, caption = caption, label = label)
  
  # output 'xtable' in requested format (LaTeX or HTML code)
  if (KNITR)
    print(cfaResultsTable, type = type, file = file,
          booktabs = TRUE, digits = digits, comment = FALSE,
          caption.placement = "top",
          hline.after = horLines,
          #add.to.row = tabNote,
          sanitize.text.function = function(x) x,
          math.style.negative = FALSE)
  else {
    table <- capture.output(
      print(cfaResultsTable, type = type, file = file,
            booktabs = TRUE, digits = digits, comment = FALSE,
            caption.placement = "top",
            hline.after = horLines,
            #add.to.row = tabNote,
            sanitize.text.function = function(x) x,
            math.style.negative = FALSE)
    )
    table
  }
}


# parameter 'latex' should be set to TRUE only for a call under KNITR

genCFAmodelDiagram <- function (cfa.fit, latex = FALSE,
                                caption = "CFA model diagram",
                                label = "cfaModelDiagram") {
  
  filetype <- ifelse(.Platform$GUI == "RStudio", "x11", "R")
  
  # produce CFA model diagram/figure, using 'semPlot' package
  cfaModDiag <-
    semPaths(cfa.fit, whatLabels = "std",
             intercepts = FALSE, thresholds = FALSE,
             rotation = 4,           # H: factors on the right
             #nCharNodes = 0,        # 0 disables abbreviation
             #label.cex = 5,
             #vsize = 20,            # node size
             edge.label.cex = 1.25,  # 1 is default
             curve = 2.5,            # 2
             curvature = 1,          # 1.2
             optimizeLatRes = TRUE,
             #curvePivot = TRUE,
             sizeMan = 10,
             sizeLat = 10,
             sizeInt = 10,
             filetype = filetype, standAlone = FALSE,
             color = list(lat = rgb(245, 253, 118, maxColorValue = 255), 
                          man = rgb(155, 253, 175, maxColorValue = 255)),
             bg = "grey90")
  
  numFactors <- length(grep("f[[:digit:]]",
                            unique(parameterEstimates(cfa.fit)$lhs),
                            value = TRUE, ignore.case = TRUE))
  
  # change color palette
  colPalette <- brewer.pal(5, "Pastel1")[1:numFactors]
  
  diagFile <- "cfaModDiag"
    
  # fix for TikZ device (LaTeX output)
  if (latex)
    cfaDiag <- qgraph.tikz(cfaModDiag, filename = diagFile,
                           colors = colPalette, bg = "grey90",
                           width = 7, height = 4, # curveShape = -0.5,
                           standAlone = FALSE)
  
  if (FALSE)  # TBD - WHY DOESN'T WORK ???
  if (latex) {
    latexOut <- paste("\\begin{figure}",
                      "\\centering",
                      "\\input{", diagFile, "}",
                      "\\caption{", caption, "}",
                      "\\label{", label, "}",
                      "\\end{figure}", sep = "\n")
    print(sanitize(latexOut))
  }
  
  if (latex) return (invisible(cfaDiag))
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

# Create results directory, if it doesn't exist
if (!file.exists(CFA_RESULTS_DIR)) {
  dir.create(CFA_RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)
}

# load data
message("\n\n*** Loading data...")
flossData <- loadData(ready4cfaFile)

# select imputed dataset
flossData <- mice::complete(flossData, 1)

# due to very small amount of projects with "Non-OSI" licesnse
# and their disapperance due to calculating correlations,
# we don't include "License Category" into EFA (consider analyzing it
# at later phases with inclusion of imputed data)
# NOT TRUE ANYMORE, as we use imputed data

# we also remove "Repo URL" due to injection of large # of NAs
# due to limiting conditions at the end of the merge process

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

# exclude outliers
flossData <- 
  subset(flossData,
         User.Community.Size > quantile(flossData$User.Community.Size,
                                        probs = .005) &
           User.Community.Size < quantile(flossData$User.Community.Size,
                                          probs = .995))

# rescale some variables as lavaan is not very happy if the
# variance of different variables are too different
##flossData$User.Community.Size <- flossData$User.Community.Size / 10000 # 0
##flossData$Project.Age <- flossData$Project.Age / 10

# ===

# calculate standard deviation for each variable in the data set
# (was needed for cor2cov(), left for informative purposes)
#sdValues <- unlist(lapply(seq(length(names(flossData))),
#                          function(i) sd(flossData[[i]], na.rm = TRUE)))


# specify the latent variable model

model <- "

# factor structure (fix factors' loadings to 1)

Governance =~ 1 * License.Category + License.Restrictiveness
Sponsorship =~ 1 * Preferred.Support.Type + Preferred.Support.Resource
Maturity =~ 1 * Project.Age + Project.Stage
Success =~ 1 * Development.Team.Size + User.Community.Size

# variances (fix one variances to one)

Governance ~~ Governance
Sponsorship ~~ Sponsorship
Maturity ~~ Maturity
Success ~~ Success

# covariances between the latent variables

Governance ~~ Sponsorship
Governance ~~ Maturity
Governance ~~ Success

Sponsorship ~~ Maturity
Sponsorship ~~ Success

Maturity ~~ Success
"

# perform CFA
message("\n*** Performing CFA of the model...\n")

if (DEBUG) {
  cfa.fit <- cfa(model, data = flossData, meanstructure = TRUE,
                 estimator = "WLSMV")
} else {
  cfa.fit <- suppressWarnings(
    cfa(model, data = flossData, meanstructure = TRUE,
                 estimator = "WLSMV")
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
