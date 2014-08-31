# Start with a clean environment
rm(list = ls(all.names = TRUE))

if (!suppressMessages(require(RCurl))) install.packages('RCurl')
if (!suppressMessages(require(stringr))) install.packages('stringr')
if (!suppressMessages(require(ggplot2))) install.packages('ggplot2')
if (!suppressMessages(require(RColorBrewer)))
  install.packages('RColorBrewer')
if (!suppressMessages(require(gridExtra))) install.packages('gridExtra')
if (!suppressMessages(require(psych))) install.packages('psych')
if (!suppressMessages(require(fitdistrplus))) 
  install.packages('fitdistrplus')
if (!suppressMessages(require(mixtools))) install.packages('mixtools')
if (!suppressMessages(require(rebmix))) install.packages('rebmix')

library(RCurl)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(psych)
library(fitdistrplus)
library(mixtools)
library(rebmix)

source("../utils/factors.R")
source("../utils/qq.R")
source("../utils/data.R")
source("../utils/mixedDist.R")

READY4EDA_DIR  <- "~/diss-floss/data/ready4eda"
READY4EDA_FILE <- "flossData" # default
RDS_EXT <- ".rds"

# temporary, until implementing EDA for single data file
TRANSFORM_DIR <- "../data/transformed"

EDA_RESULTS_DIR <- "../results/eda"

DIST_FIT_COLOR <- "green" # ggplot2 line color for distrib. fitting

DEBUG <- TRUE # TODO: retrieve debug flag via CL arguments

allPlots <- list()


##### EDA CATEGORIES #####

uniDescriptiveEDA <- function (df, var, colName, extraFun) {
  
  data <- df[[colName]]
  
  if (is.numeric(data) || is.factor(data)) {
    message("\nDecriptive statistics for '", colName, "':\n")
    print(summary(data))
  }
}


uniVisualEDA <- function (df, var, colName, extraFun) {
  
  data <- df[[colName]]
  
  #env <- new.env()
  #do.call(extraFun, list(df, var), envir = env)
  
  if (is.numeric(data)) {
    plot <- plotHistogram(df, colName)
    allPlots <<- c(allPlots, list(plot))
  }
  
  if (is.factor(data)) {
    plot <- plotBarGraph(df, colName)
    allPlots <<- c(allPlots, list(plot))
  }
  
  if (is.numeric(data)) {
    plot <- ggQQplot(data, colName)
    allPlots <<- c(allPlots, list(plot))
  }
  
  #if (TBD CONDITION) {
  #  plot <- plotDensity(df, colName)
  #  allPlots <<- c(allPlots, list(plot))
  #}
}


fitDistParam <- function (df, var, colName, extraFun) {
  
  data <- df[[colName]]
  df <- na.omit(df)
  
  message("\nParametric distribution fitting for '", colName, "':\n")

  # convert factors to integers
  if (is.factor(data)) data <- as.integer(data)

  if (FALSE) {
    dataDist <- fitdist(data, "gamma")
    dataBoot <- bootdist(dataDist, niter=51) #default niter=1001
    print(dataBoot)
    #plot(dataBoot)
    print(summary(dataBoot))
    print(quantile(dataBoot))
    message("")
  }
  
  if (is.numeric(data) || is.factor(data)) {
    fitgmme <- fitdist(data, "gamma", method="mme")
    print(summary(fitgmme))
    message("")
  }
  
  if (is.numeric(data) || is.factor(data)) {
    ##fitg <- fitdist(data, "gamma")
    ##print(summary(fitg))
    #plot(fitg)
    #plot(fitg, demp = TRUE)
    #plot(fitg, histo = FALSE, demp = TRUE)
    ##cdfcomp(fitg, addlegend=FALSE)
    ##denscomp(fitg, addlegend=FALSE)
    ##ppcomp(fitg, addlegend=FALSE)
    ##qqcomp(fitg, addlegend=FALSE)
    ##message("")
  }
}


fitDistNonParam <- function (df, var, colName, extraFun) {
  
  data <- df[[colName]]
  data <- na.omit(data)
  
  message("\nNon-parametric distribution fitting for '", colName, "':\n")
  
  # convert factors to integers
  if (is.factor(data)) data <- as.integer(data)
  
  # perform non-parametric mixture distribution fitting
  if (is.numeric(data)) {

    num.components <- 3 # can determine automatically?
    
    mixDistInfo <- fitMixDist(data, num.components)
    g <- plotMixedDist(data, mixDistInfo, num.components)
    
    if (.Platform$GUI == "RStudio") {print(g)}
    
    #TODO: consider moving to main
    edaFile <- str_replace_all(string=colName, pattern=" ", repl="")
    edaFile <- file.path(EDA_RESULTS_DIR, paste0(edaFile, "-DistFitMix", ".svg"))
    suppressMessages(ggsave(file=edaFile, plot=g, width=8.5, height=11))
    
    allPlots <<- c(allPlots, list(g))
    message("")
  }
  
}


fitDistREBMIX <- function (df, var, colName, extraFun) {
  
  data <- df[[colName]]
  df <- na.omit(df)
  
  # convert factors to integers
  if (is.factor(data)) data <- as.integer(data)
  
  # perform parametric mixture distribution fitting
  if (is.numeric(data)) {
    set.seed(123)
    
    message("\nREBMIX: Parametric distribution fitting for '", colName, "':\n")
    
    boot.param <- boot.REBMIX(x = list(data), pos = 1, Bootstrap = "p",
                              B = 100, n = NULL, replace = TRUE, prob = NULL)
    summary(boot.param)
    
    message("\nREBMIX: Non-parametric distribution fitting for '", colName, "':\n")
    
    boot.nonparam <- boot.REBMIX(x = list(data), pos = 1, Bootstrap = "n",
                                 B = 100, n = NULL, replace = TRUE, prob = NULL)
    summary(boot.nonparam)
    
    message("\nREBMIX: Poisson distribution fitting for '", colName, "':\n")

    poissonest <- REBMIX(Dataset = list(data), Preprocessing = "histogram",
                         cmax = 6, Criterion = "MDL5",
                         Variables = rep("discrete", 2),
                         pdf = rep("Poisson", 2), K = 1)
    c <- as.numeric(poissonest$summary$c)
    summary(c)
    message("")
  }
}


multiDescriptiveEDA <- function (df, var, colNames, extraFun) {
  
  message("\nDecriptive statistics for '", colNames, "':\n")
  
  # suppress "NAs introduced by coercion" warnings
  suppressWarnings(describe(flossData))
}


multiVisualEDA <- function (df, var, colName, extraFun) {
  
}


performEDA <- function (dataSource, analysis,
                        indicator, colName, extraFun) {

  fileName <- paste0(indicator, RDS_EXT)
  rdataFile <- file.path(TRANSFORM_DIR, dataSource, fileName)
  if (file.exists(rdataFile)) {
    data <- readRDS(rdataFile)
  }
  else {
    stop("RDS file for \'", indicator, "\' not found! ",
          "Run 'make' first.")
  }
  
  if (identical(analysis, "univariate")) {
    
    uniDescriptiveEDA(data, indicator, colName, extraFun)
    uniVisualEDA(data, indicator, colName, extraFun)
    #fitDistParam(data, indicator, colName, extraFun)
    fitDistNonParam(data, indicator, colName, extraFun)
    #fitDistREBMIX(data, indicator, colName, extraFun)
    
  } else if (identical(analysis, "multivariate")) {
   
    colNames <- names(data)
    colNames <- colNames[-1] # delete Project ID
    multiDescriptiveEDA(data, indicator, colNames, extraFun)
    multiVisualEDA(data, indicator, colNames, extraFun)
    
  } else {
    error("Unknown type of EDA analysis - ",
          "accepted values are 'univariate' and 'multivariate'")
  }
  
  rm(data)
}


##### VISUAL EDA #####


# Plot distribution of a continuous variable "colName"
plotHistogram <- function (df, colName, print = TRUE) {
  
  df <- df
  df$var <- df[[colName]]
  df <- na.omit(df)
  
  title <- paste("Projects distribution across", colName, "range")
  xLabel <- colName
  
  if (identical(colName, "Project Age"))
    xLabel <- paste(colName, "(months)")
  
  g <- ggplot(df, aes(x=var)) +
    scale_fill_continuous("Number of\nprojects") + 
    #scale_x_continuous(xLabel) +
    #scale_y_continuous("Number of projects") +
    scale_x_log10(xLabel) +
    scale_y_log10("Number of projects") +
    ggtitle(label=title)
  
  breaks <- pretty(range(df$var), n = nclass.FD(df$var), min.n = 1)
  bwidth <- breaks[2] - breaks[1]
  
  g <- g + geom_histogram(aes(fill = ..count..),
                          binwidth = 0.01, #bwidth, #0.1
                          position = "identity")
  
  # Overlay with transparent density plot
  #g <- g + geom_density(alpha=.2, fill="#FF6666")
  
  # Ignore NA values for mean
  g <- g + geom_vline(aes(xintercept=mean(var, na.rm=T)),
                      color="red", size=1)
  
  if (.Platform$GUI == "RStudio") {print(g)}
  
  #TODO: consider moving to main
  edaFile <- str_replace_all(string=colName, pattern=" ", repl="")
  edaFile <- file.path(EDA_RESULTS_DIR, paste0(edaFile, ".svg"))
  suppressMessages(ggsave(file=edaFile, plot=g, width=8.5, height=11))

  return (g)
}


# Plot distribution of a continuous variable "colName" by category
plotDensity <- function (df, colName) {
  
  df <- df
  df$var <- df[[colName]]
  df$category <- factor(df[[colName]])
  df <- na.omit(df)
  
  title <- paste("Projects distribution across", colName,
                 "range (by category)")
  xLabel <- colName
  
#  g <- ggplot(df, aes(x = var)) +  
#    geom_bar(aes(y = (..count..)/sum(..count..)), binwidth = 25) + 
#    scale_y_continuous(labels = percent_format())
  
breaks <- pretty(range(df$var), n = nclass.FD(df$var), min.n = 1)
bwidth <- breaks[2] - breaks[1]

g <- ggplot(df, aes(x=var, fill=var)) +
    geom_density(aes(y=..count..), 
                 binwidth=bwidth, position="identity")
  
#  g <- ggplot(df, aes(var, ..density.., colour = category)) +
#    scale_fill_continuous("Number of\nprojects") + 
#    scale_x_continuous(xLabel) +
#    scale_y_continuous("Number of projects") +
#    ggtitle(label=title)
  
#  g <- g + geom_freqpoly(binwidth = 1)
  
  if (.Platform$GUI == "RStudio") {print(g)}

  #TODO: consider moving to main
  edaFile <- str_replace_all(string=colName, pattern=" ", repl="")
  edaFile <- file.path(EDA_RESULTS_DIR, paste0(edaFile, ".svg"))
  suppressMessages(ggsave(file=edaFile, plot=g, width=8.5, height=11))
  
  return (g)
}


# Plot distribution of a categorical variable "colName"
plotBarGraph <- function (df, colName) {
  
  SHOW_LEVELS <- 10
  df <- df
  df$var <- factor(df[[colName]])
  df <- na.omit(df)
  
  # sort factor levels by the frequency of levels
  df$var <- reorder(df$var, df$var, function(x) -length(x))
  df$var <- topFactors(df$var, SHOW_LEVELS, o="The Rest")
  
  title <- paste("Projects distribution across", colName, "range")

  # prepare to place percentage on top of bars, using geom_text()
  if (FALSE) {
    dfTab <- as.data.frame(table(df))
    colnames(dfTab)[1] <- "x"
    dfTab$lab <- as.character(100 * dfTab$Freq / sum(dfTab$Freq))
  }
  
  # df[!is.na(df$var), ]
  g <- ggplot(df, aes(x=var, fill=var)) +
    geom_bar(stat="bin", position="identity") +
    scale_fill_discrete(colName) + 
    xlab(colName) +
    ylab("Number of projects") +
    ggtitle(label=title)

  # display pre-calculated percentage on top of bars
  if (FALSE) {
    g <- g +
      geom_text(data=dfTab,aes(x=x,y=Freq,label=lab),vjust=0) +
      theme(axis.text.x=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),legend.title=element_blank(),
            axis.title.y=element_blank())  
  }
  
  if (.Platform$GUI == "RStudio") {print(g)}
  
  #TODO: consider moving to main
  edaFile <- str_replace_all(string=colName, pattern=" ", repl="")
  edaFile <- file.path(EDA_RESULTS_DIR, paste0(edaFile, ".svg"))
  suppressMessages(ggsave(file=edaFile, plot=g, width=8.5, height=11))
  
  return (g)
}


ggQQplot <- function (vec, varName) # argument: vector of numbers
{
  
  title <- paste0("Q-Q plot for '", varName, "'")
  
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  d <- data.frame(resids = vec)
  
  g <- ggplot(d, aes(sample = resids)) +
    
    # Normal distribution function by default
    # suppress "Removed N rows containing missing values (stat_qq)"
    suppressWarnings(stat_qq()) +
    
    # Gamma distribution function
    #stat_qq(distribution = qgamma, dparams=list(shape=1)) +
    
    # Adaptive QQ plotting fuction
    #stat_function(fun=qq, args = list(x0 = sample, y0 = vec)) +
    
    geom_abline(slope = slope, intercept = int) +
    scale_x_continuous("Theoretical Quantiles") +
    scale_y_continuous("Sample Quantiles") +
    ggtitle(label=title)

  if (.Platform$GUI == "RStudio") {print(g)}
  
  #TODO: consider moving to main
  edaFile <- str_replace_all(string=varName, pattern=" ", repl="")
  edaFile <- file.path(EDA_RESULTS_DIR, paste0("QQ-", edaFile, ".svg"))
  suppressMessages(ggsave(file=edaFile, plot=g, width=8.5, height=11))
  
  return (g)
}

##### EDA MAIN #####


message("\n===== Starting Exploratory Data Analysis (EDA)...")

fileName <- paste0(READY4EDA_FILE, RDS_EXT)
ready4edaFile <- file.path(READY4EDA_DIR, fileName)

# load data
message("\nLoading data...")
flossData <- loadData(ready4edaFile)

# construct list of indicators & corresponding extra functions
sfIndicators <- c("prjAge", "devTeamSize",
                  "prjLicense", "prjMaturity")
sfColumnNames <- c("Project Age", "Development Team Size",
                   "Project License", "Project Maturity")
sfExtraFun <- list("projectAge", "devTeamSize",
                   "projectLicense", "projectMaturity")

# sequentially call EDA functions for all indicators in data source
silent <- lapply(seq_along(sfIndicators), function(i) {
  performEDA("SourceForge", analysis="univariate",
             sfIndicators[[i]], sfColumnNames[[i]], sfExtraFun[[i]])
  })


edaFilePDF <- file.path(EDA_RESULTS_DIR, "eda-univar.pdf")
mg <- do.call(marrangeGrob, c(allPlots, list(nrow=2, ncol = 1)));
suppressMessages(ggsave(filename=edaFilePDF, mg, width=8.5, height=11))

message("\n===== EDA completed, results can be found ",
        "in directory \"", EDA_RESULTS_DIR, "\"\n")

stop('OK! Intentionally stopped to prevent code from running.')

# construct list of indicators & corresponding extra functions
sfMultiIndicators <- c("prjAge", "prjLicense")
sfMultiColumnNames <- c("Project Age", "Project License")
sfMultiExtraFun <- list("projectAge", "projectLicense")

multiPlots <- lapply(seq_along(sfMultiIndicators), function(i) {
  performEDA("SourceForge", analysis="multivariate",
             sfMultiIndicators[[i]], sfMultiColumnNames[[i]],
             sfMultiExtraFun[[i]])
  })

#edaFilePDF <- file.path(EDA_RESULTS_DIR, "eda-multivar.pdf")
#pdf(edaFilePDF)
#silent <- lapply(multiPlots, print)


##### "EXTRA" (CUSTOMIZATION) FUNCTIONS #####


projectAge <- function (df, var) {}

projectLicense <- function (df, var) {}

devTeamSize <- function (df, var) {}

projectMaturity <- function (df, var) {}
