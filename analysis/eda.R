if (!suppressMessages(require(RCurl))) install.packages('RCurl')
if (!suppressMessages(require(stringr))) install.packages('stringr')
if (!suppressMessages(require(ggplot2))) install.packages('ggplot2')
if (!suppressMessages(require(gridExtra))) install.packages('gridExtra')


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
    allPlots <<- c(allPlots, list(plot))
  }
  
  if (is.factor(data)) {
    plot <- plotBarGraph(df, colName)
    allPlots <<- c(allPlots, list(plot))
    
    #plot <- plotDensity(df, colName)
    #allPlots <<- c(allPlots, list(plot))
  }
  
  if (is.numeric(data)) {
    plot <- ggQQplot(data, colName)
    allPlots <<- c(allPlots, list(plot))
  }
}


multiDescriptiveEDA <- function (df, var, colNames, extraFun) {
  
  message("\nDecriptive statistics for '", colNames, "':\n")
}


multiVisualEDA <- function (df, var, colName, extraFun) {
  
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
  
  title <- paste("Projects distribution across", colName, "range")
  xLabel <- colName
  
  if (identical(colName, "Project Age"))
    xLabel <- paste(colName, "(months)")
  
  g <- qplot(df[[colName]], data = df, binwidth = 1) +
    scale_fill_continuous("Number of\nprojects") + 
    scale_x_continuous(xLabel) +
    scale_y_continuous("Number of projects") +
    ggtitle(label=title)
  
  g <- g + geom_histogram(aes(fill = ..count..), binwidth = 1)

  if (.Platform$GUI == "RStudio") {print(g); dev.off()}
  
  #TODO: consider moving to main
  edaFile <- str_replace_all(string=colName, pattern=" ", repl="")
  edaFile <- paste0(EDA_RESULTS_DIR, "/", edaFile, ".svg")
  suppressMessages(ggsave(file=edaFile, plot=g))

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
  suppressMessages(ggsave(file=edaFile, plot=g))
  
  return (g)
}


# Plot distribution of a categorical variable "colName"
plotBarGraph <- function (df, colName) {
  
  df <- df
  df$var <- factor(df[[colName]])
  
  title <- paste("Projects distribution across", colName, "range")
  
  g <- ggplot(data=df, aes(x=var, fill=var)) +
    geom_bar(stat="bin") +
    scale_fill_discrete(colName) + 
    xlab(colName) +
    ylab("Number of projects") +
    ggtitle(label=title)
  
  if (.Platform$GUI == "RStudio") {print(g); dev.off()}
  
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

  if (.Platform$GUI == "RStudio") {print(g); dev.off()}
  
  return (g)
}

##### EDA MAIN #####


message("\n===== Starting Exploratory Data Analysis (EDA)...")

# construct list of indicators & corresponding extra functions
sfIndicators <- c("prjAge", "devTeamSize", "prjLicense")
sfColumnNames <- c("Project Age", "Development Team Size",
                   "Project License")
sfExtraFun <- list("projectAge", "devTeamSize", "projectLicense")

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
