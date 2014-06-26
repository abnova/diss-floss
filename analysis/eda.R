# Start with a clean environment
rm(list = ls(all.names = TRUE))

if (!suppressMessages(require(RCurl))) install.packages('RCurl')
if (!suppressMessages(require(stringr))) install.packages('stringr')
if (!suppressMessages(require(ggplot2))) install.packages('ggplot2')
if (!suppressMessages(require(gridExtra))) install.packages('gridExtra')

library(RCurl)
library(stringr)
library(ggplot2)
library(gridExtra)

source("../utils/factors.R")
source("../utils/qq.R")

TRANSFORM_DIR <- "../data/transform"
RDS_EXT <- ".rds"

EDA_RESULTS_DIR <- "../results/eda"

DEBUG <- TRUE # TODO: retrieve debug flag via CL arguments

allPlots <- list()


##### EDA CATEGORIES #####

uniDescriptiveEDA <- function (df, var, colName, extraFun) {
  
  data <- df[[colName]]
  
  if (is.numeric(data)) {
    message("\nDecriptive statistics for '", colName, "':\n")
    print(summary(data))
  }
  
  if (is.factor(data)) {
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


multiDescriptiveEDA <- function (df, var, colNames, extraFun) {
  
  message("\nDecriptive statistics for '", colNames, "':\n")
}


multiVisualEDA <- function (df, var, colName, extraFun) {
  
}


performEDA <- function (dataSource, analysis,
                        indicator, colName, extraFun) {

  fileDigest <- base64(indicator)
  rdataFile <- paste0(TRANSFORM_DIR, "/", dataSource, "/",
                      fileDigest, RDS_EXT)
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
    
  } else if (identical(analysis, "multivariate")) {
   
    colNames <- names(data)
    colNames <- colNames[-1] # delete Project ID
    #multiDescriptiveEDA(data, indicator, colNames, extraFun)
    multiVisualEDA(data, indicator, colNames, extraFun)
    
  } else {
    error("Unknown type of EDA analysis - ",
          "accepted values are 'univariate' and 'multivariate'")
  }
  
  rm(data)
}


##### VISUAL EDA #####


# Plot distribution of a continuous variable "colName"
plotHistogram <- function (df, colName) {
  
  df <- df
  df$var <- df[[colName]]
  
  title <- paste("Projects distribution across", colName, "range")
  xLabel <- colName
  
  if (identical(colName, "Project Age"))
    xLabel <- paste(colName, "(months)")
  
  g <- ggplot(data = df, aes(x=var)) +
    scale_fill_continuous("Number of\nprojects") + 
    scale_x_continuous(xLabel) +
    #scale_y_continuous("Number of projects") +
    #scale_x_log10(xLabel) +
    scale_y_log10("Number of projects") +
    ggtitle(label=title)
  
  g <- g + geom_histogram(aes(fill = ..count..), binwidth = 1,
                          position = "identity")
  
  # Overlay with transparent density plot
  #g <- g + geom_density(alpha=.2, fill="#FF6666")
  
  # Ignore NA values for mean
  g <- g + geom_vline(aes(xintercept=mean(var, na.rm=T)),
                      color="red", size=1)
  
  if (.Platform$GUI == "RStudio") {print(g); dev.off()}
  
  #TODO: consider moving to main
  edaFile <- str_replace_all(string=colName, pattern=" ", repl="")
  edaFile <- paste0(EDA_RESULTS_DIR, "/", edaFile, ".svg")
  suppressMessages(ggsave(file=edaFile, plot=g, width=8.5, height=11))

  return (g)
}


# Plot distribution of a continuous variable "colName" by category
plotDensity <- function (df, colName) {
  
  df <- df
  df$var <- df[[colName]]
  df$category <- factor(df[[colName]])
  
  title <- paste("Projects distribution across", colName,
                 "range (by category)")
  xLabel <- colName
  
#  g <- ggplot(df, aes(x = var)) +  
#    geom_bar(aes(y = (..count..)/sum(..count..)), binwidth = 25) + 
#    scale_y_continuous(labels = percent_format())
  
  g <- ggplot(df, aes(x=var, fill=var)) +
    geom_density(aes(y=..count..), binwidth=1, position="identity")
  
#  g <- ggplot(df, aes(var, ..density.., colour = category)) +
#    scale_fill_continuous("Number of\nprojects") + 
#    scale_x_continuous(xLabel) +
#    scale_y_continuous("Number of projects") +
#    ggtitle(label=title)
  
#  g <- g + geom_freqpoly(binwidth = 1)
  
  if (.Platform$GUI == "RStudio") {print(g); dev.off()}

  #TODO: consider moving to main
  edaFile <- str_replace_all(string=colName, pattern=" ", repl="")
  edaFile <- paste0(EDA_RESULTS_DIR, "/", edaFile, ".svg")
  suppressMessages(ggsave(file=edaFile, plot=g, width=8.5, height=11))
  
  return (g)
}


# Plot distribution of a categorical variable "colName"
plotBarGraph <- function (df, colName) {
  
  SHOW_LEVELS <- 10
  df <- df
  df$var <- factor(df[[colName]])
  
  # sort factor levels by the frequency of levels
  df$var <- reorder(df$var, df$var, function(x) -length(x))
  df$var <- topFactors(df$var, SHOW_LEVELS, o="The Rest")
  
  title <- paste("Projects distribution across", colName, "range")
  
  g <- ggplot(data=df, aes(x=var, fill=var)) +
    geom_bar(stat="bin", position="identity") +
    scale_fill_discrete(colName) + 
    xlab(colName) +
    ylab("Number of projects") +
    ggtitle(label=title)
  
  if (.Platform$GUI == "RStudio") {print(g); dev.off()}
  
  #TODO: consider moving to main
  edaFile <- str_replace_all(string=colName, pattern=" ", repl="")
  edaFile <- paste0(EDA_RESULTS_DIR, "/", edaFile, ".svg")
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
    stat_qq() +
    
    # Gamma distribution function
    #stat_qq(distribution = qgamma, dparams=list(shape=1)) +
    
    # Adaptive QQ plotting fuction
    #stat_function(fun=qq, args = list(x0 = sample, y0 = vec)) +
    
    geom_abline(slope = slope, intercept = int) +
    scale_x_continuous("Theoretical Quantiles") +
    scale_y_continuous("Sample Quantiles") +
    ggtitle(label=title)

  if (.Platform$GUI == "RStudio") {print(g); dev.off()}
  
  #TODO: consider moving to main
  edaFile <- str_replace_all(string=varName, pattern=" ", repl="")
  edaFile <- paste0(EDA_RESULTS_DIR, "/", "QQ-", edaFile, ".svg")
  suppressMessages(ggsave(file=edaFile, plot=g, width=8.5, height=11))
  
  return (g)
}

##### EDA MAIN #####


message("\n===== Starting Exploratory Data Analysis (EDA)...")

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

#dev.off() # to display graphics in RStudio plot window

edaFilePDF <- paste0(EDA_RESULTS_DIR, "/", "eda-univar.pdf")
mg <- do.call(marrangeGrob, c(allPlots, list(nrow=2, ncol = 1)));
suppressMessages(ggsave(filename=edaFilePDF, mg, width=8.5, height=11))

message("\n===== EDA completed, results can be found ",
        "in directory \"", EDA_RESULTS_DIR, "\"\n")

stop()

# construct list of indicators & corresponding extra functions
sfMultiIndicators <- c("prjAge", "prjLicense")
sfMultiColumnNames <- c("Project Age", "Project License")
sfMultiExtraFun <- list("projectAge", "projectLicense")

multiPlots <- lapply(seq_along(sfMultiIndicators), function(i) {
  performEDA("SourceForge", analysis="multivariate",
             sfMultiIndicators[[i]], sfMultiColumnNames[[i]],
             sfMultiExtraFun[[i]])
  })

#edaFilePDF <- paste0(EDA_RESULTS_DIR, "/", "eda-multivar.pdf")
#pdf(edaFilePDF)
#silent <- lapply(multiPlots, print)
#dev.off()


##### "EXTRA" (CUSTOMIZATION) FUNCTIONS #####


projectAge <- function (df, var) {}

projectLicense <- function (df, var) {}

devTeamSize <- function (df, var) {}

projectMaturity <- function (df, var) {}
