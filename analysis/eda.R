if (!suppressMessages(require(RCurl))) install.packages('RCurl')
if (!suppressMessages(require(stringr))) install.packages('stringr')
if (!suppressMessages(require(ggplot2))) install.packages('ggplot2')


CACHE_DIR <- "../cache"
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
    allPlots <- c(allPlots, plot)
  }
  
  if (is.factor(data)) {
    plot <- plotBarGraph(df, colName)
    allPlots <- c(allPlots, plot)
  }
  
  if (is.numeric(data)) {
    plot <- ggQQplot(data, colName)
    allPlots <- c(allPlots, plot)
  }
  
  return (allPlots)
}


multiDescriptiveEDA <- function (df, var, colNames, extraFun) {
  
  message("\nDecriptive statistics for '", colNames, "':\n")
}


multiVisualEDA <- function (df, var, colName, extraFun) {
  
  plot <- NULL
  return (plot)
}


performEDA <- function (dataSource, analysis,
                        indicator, colName, extraFun) {

  fileDigest <- base64(indicator)
  rdataFile <- paste0(CACHE_DIR, "/", dataSource, "/",
                      fileDigest, RDS_EXT)
  if (file.exists(rdataFile)) {
    data <- readRDS(rdataFile)
  }
  else {
    error("RDS file for \'", indicator, "\' not found! ",
          "Run 'make' first.")
  }
  
  if (identical(analysis, "univariate")) {
    
    uniDescriptiveEDA(data, indicator, colName, extraFun)
    plots <- uniVisualEDA(data, indicator, colName, extraFun)
    allPlots <- c(allPlots, plots)
    
  } else if (identical(analysis, "multivariate")) {
   
    colNames <- names(data)
    colNames <- colNames[-1] # delete Project ID
    #multiDescriptiveEDA(data, indicator, colNames, extraFun)
    plots <- multiVisualEDA(data, indicator, colNames, extraFun)
    allPlots <- c(allPlots, plots)
    
  } else {
    error("Unknown type of EDA analysis - ",
          "accepted values are 'univariate' and 'multivariate'")
  }
  
  rm(data)
  
  return (allPlots)
}


##### VISUAL EDA #####


# Plot distribution of a continuous variable "colName"
plotHistogram <- function (df, colName) {

  g <- qplot(df[[colName]], data = df, binwidth = 1) +
    scale_fill_continuous("Number of\nprojects") + 
    scale_x_continuous("Project Age (months)") +
    scale_y_continuous("Number of projects") +
    ggtitle(label="Projects distribution across age range")
  
  g <- g + geom_histogram(aes(fill = ..count..), binwidth = 1)

  if (.Platform$GUI == "RStudio") print(g)
  
  #TODO: consider moving to main
  edaFile <- str_replace_all(string=colName, pattern=" ", repl="")
  edaFile <- paste0(EDA_RESULTS_DIR, "/", edaFile, ".svg")
  suppressMessages(ggsave(file=edaFile, plot=g))

  return (g)
}


# Plot distribution of a continuous variable "colName" by category
plotDensity <- function (df, colName, categoryName) {
  
  df <- df
  df$var <- df[[colName]]
  df$category <- factor(df[[categoryName]])
  
  ggplot(df, aes(var, ..density.., colour = category)) +
    scale_fill_continuous("Number of\nprojects") + 
    scale_x_continuous("Project Age (months)") +
    scale_y_continuous("Number of projects") +
    ggtitle(label="Projects distribution across age range (by license)")
  
  g <- g + geom_freqpoly(binwidth = 1)
  
  if (.Platform$GUI == "RStudio") print(g)
  
  #TODO: consider moving to main
  edaFile <- str_replace_all(string=colName, pattern=" ", repl="")
  edaFile <- paste0(EDA_RESULTS_DIR, "/", edaFile, ".svg")
  suppressMessages(ggsave(file=edaFile, plot=g))
  
  return (g)
}


# Plot distribution of a categorical variable "colName"
plotBarGraph <- function (df, colName) {
  
  df <- df
  df$var <- factor(df[[colName]])
  
  g <- ggplot(data=df, aes(x=var, fill=var)) +
    geom_bar(stat="bin") +
    scale_fill_discrete("License") + 
    xlab(colName) +
    ylab("Number of projects") +
    ggtitle(label="Projects distribution across licenses range")
  
  if (.Platform$GUI == "RStudio") print(g)

  #TODO: consider moving to main
  edaFile <- str_replace_all(string=colName, pattern=" ", repl="")
  edaFile <- paste0(EDA_RESULTS_DIR, "/", edaFile, ".svg")
  suppressMessages(ggsave(file=edaFile, plot=g))
  
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
    stat_qq() + geom_abline(slope = slope, intercept = int) +
    scale_x_continuous("Theoretical Quantiles") +
    scale_y_continuous("Sample Quantiles") +
    ggtitle(label=title)

  if (.Platform$GUI == "RStudio") print(g)

  return (g)
}

##### EDA MAIN #####


# construct list of indicators & corresponding extra functions
sfIndicators <- c("prjAge", "prjLicense")
sfColumnNames <- c("Project Age", "Project License")
sfExtraFun <- list("projectAge", "projectLicense")

#browser()

# sequentially call EDA functions for all indicators in data source
uniPlots <- lapply(seq_along(sfIndicators), function(i) {
  performEDA("SourceForge", analysis="univariate",
             sfIndicators[[i]], sfColumnNames[[i]], sfExtraFun[[i]])
  })

edaFilePDF <- paste0(EDA_RESULTS_DIR, "/", "eda-univar.pdf")
pdf(edaFilePDF)
#silent <- lapply(uniPlots, print)

dev.off()


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
