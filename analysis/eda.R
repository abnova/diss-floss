if (!suppressMessages(require(RCurl))) install.packages('RCurl')
if (!suppressMessages(require(ggplot2))) install.packages('ggplot2')


CACHE_DIR <- "../cache"
RDS_EXT <- ".rds"

DEBUG <- TRUE # TODO: retrieve debug flag via CL arguments


##### EDA CATEGORIES #####

uniDescriptiveEDA <- function (df, var, colName, extraFun) {
  
  data <- df[[colName]]
  
  #env <- new.env()
  #do.call(extraFun, list(df, var), envir = env)
  
  if (is.factor(data))
    print(summary(data))
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


indicatorEDA <- function (dataSource, indicator, colName, extraFun) {

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
  
  uniDescriptiveEDA(data, indicator, colName, extraFun)
  uniVisualEDA(data, indicator, colName, extraFun)
  
  multiDescriptiveEDA(data, indicator, colName, extraFun)
  multiVisualEDA(data, indicator, colName, extraFun)
  
  rm(data)
}


##### VISUAL EDA #####

plotHistogram <- function (df, colName) {

  g <- qplot(df[[colName]], data = df, binwidth = 1) +
    #scale_size_area("Number of projects") + 
    scale_x_continuous("Project Age (in months)") +
    scale_y_continuous("Number of projects") +
    ggtitle(label="Projects distribution across their age")
  
  g <- g + geom_histogram(aes(fill = ..count..), binwidth = 1)

  if (.Platform$GUI == "RStudio")
    print(g)
  else {
    ggsave(file="test.svg", plot=g)
    ggsave(file="test.pdf", plot=g)
  }
  
  g <- g + geom_histogram(aes(y = ..density..), binwidth = 1) +
    geom_density()
  print(g)
}


##### EDA MAIN #####


# construct list of indicators & corresponding transform. functions
sfIndicators <- c("prjAge", "prjLicense")
sfColumnNames <- c("Project Age", "Project License")
sfExtraFun <- list("projectAge", "projectLicense")

# sequentially call all previously defined transformation functions
lapply(seq_along(sfIndicators),
       function(i) {
         indicatorEDA("SourceForge", sfIndicators[[i]],
                      sfColumnNames[[i]], sfExtraFun[[i]])
       })


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
