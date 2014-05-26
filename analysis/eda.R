if (!suppressMessages(require(RCurl))) install.packages('RCurl')
if (!suppressMessages(require(stringr))) install.packages('stringr')
if (!suppressMessages(require(ggplot2))) install.packages('ggplot2')


CACHE_DIR <- "../cache"
RDS_EXT <- ".rds"

DEBUG <- TRUE # TODO: retrieve debug flag via CL arguments


##### EDA CATEGORIES #####

uniDescriptiveEDA <- function (df, var, colName, extraFun) {
  
  data <- df[[colName]]
  
  #env <- new.env()
  #do.call(extraFun, list(df, var), envir = env)
  
  if (is.factor(data)) {
    print(summary(data))
    plotBarGraph(df, colName)
  }
}


uniVisualEDA <- function (df, var, colName, extraFun) {
  
  if (is.numeric(df[[colName]])) {
    plotHistogram(df, colName)
  }
}


multiDescriptiveEDA <- function (df, var, colName, extraFun) {
  
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
    error("RDS file for \'", indicator, "\' not found! ",
          "Run 'make' first.")
  }
  
  if (identical(analysis, "univariate")) {
    
    uniDescriptiveEDA(data, indicator, colName, extraFun)
    uniVisualEDA(data, indicator, colName, extraFun)
    
  } else if (identical(analysis, "multivariate")) {
    
    multiDescriptiveEDA(data, indicator, colName, extraFun)
    multiVisualEDA(data, indicator, colName, extraFun)
    
  } else {
    error("Unknown type of EDA analysis - ",
          "accepted values are 'univariate' and 'multivariate'")
  }
  
  rm(data)
}


##### VISUAL EDA #####


# Plot distribution of a continuous variable "colName"
plotHistogram <- function (df, colName) {

  g <- qplot(df[[colName]], data = df, binwidth = 1) +
    scale_size_area("Projects") + 
    scale_x_continuous("Project Age (in months)") +
    scale_y_continuous("Number of projects") +
    ggtitle(label="Projects distribution across their age")
  
  g <- g + geom_histogram(aes(fill = ..count..), binwidth = 1)

  if (.Platform$GUI == "RStudio")
    print(g)

  edaFile <- lapply(strsplit(colName, " "), str_trim)
  print(edaFile)
  edaFile <- lapply(edaFile, paste0)
  print(edaFile)
  edaFile <- paste0("./", edaFile, ".svg")
  print(edaFile)
  ggsave(file=edaFile, plot=g)
  
  dev.off()
  
  return (g)
}


# Plot distribution of a categorical variable "colName"
plotBarGraph <- function (df, colName) {
  
  df <- df
  df$var <- factor(df[[colName]])
  
  g <- ggplot(data=df, aes(x=var, fill=var)) +
    geom_bar(stat="bin")
  print(g)
  
  return (g)
}


##### EDA MAIN #####


# construct list of indicators & corresponding extra functions
sfIndicators <- c("prjAge", "prjLicense")
sfColumnNames <- c("Project Age", "Project License")
sfExtraFun <- list("projectAge", "projectLicense")

# sequentially call EDA functions for all indicators in data source
uniPlots <- lapply(seq_along(sfIndicators), function(i) {
  performEDA("SourceForge", analysis="univariate",
             sfIndicators[[i]], sfColumnNames[[i]], sfExtraFun[[i]])
  })

pdf("./eda-univar.pdf")
silent <- lapply(uniPlots, print)

multiPlots <- lapply(seq_along(sfIndicators), function(i) {
  performEDA("SourceForge", analysis="multivariate",
             sfIndicators[[i]], sfColumnNames[[i]], sfExtraFun[[i]])
})

pdf("./eda-multivar.pdf")
silent <- lapply(multiPlots, print)

dev.off()


##### "EXTRA" (CUSTOMIZATION) FUNCTIONS #####


projectAge <- function (df, var) {}

projectLicense <- function (df, var) {}


##### MISC #####

if (FALSE) {
  
  g <- qplot(data[["Project Age"]], data = data, binwidth = 1) +
    #scale_size_area("Number of projects") + 
    scale_x_continuous("Project Age (in months)") +
    scale_y_continuous("Number of projects") +
    ggtitle(label="Projects distribution across their age")
  
  g <- g + geom_histogram(aes(fill = ..count..), binwidth = 1)
  print(g)
  
  g <- g + geom_histogram(aes(y = ..density..), binwidth = 1) +
    geom_density()
  print(g)
}

#g <- qplot(data[["Project Age"]], data = data,
#           geom = "histogram", binwidth = 1)

#g <- g + geom_histogram(aes(y = ..count..), binwidth = 1)

#g <- g + geom_histogram(aes(fill = ..count..), binwidth = 1) +
#  scale_fill_gradient("Count", low = "green", high = "red")
