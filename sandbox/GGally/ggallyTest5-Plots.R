# GGally @examples for correlation plots
 
rm(list = ls(all.names = TRUE))

library("GGally")

getData <- function () {
  
  # retrieve and prepare data
  testData <- "https://github.com/abnova/test/blob/master/flossData4eda.rds?raw=true"
  
  tmpFile <- tempfile()
  tmpDir <- tempdir()
  
  download.file(testData, tmpFile, method = 'curl',
                extra = '-L', quiet = TRUE)
  
  df <- readRDS(tmpFile)
  
  df$prjage   <- df[["Project Age"]]
  df$teamsize <- df[["Development Team Size"]]
  df$license  <- df[["Project License"]]
  df$prjmaturity <- df[["Project Maturity"]]
  df$maturity <- factor(df$prjmaturity)
  
  return (df)
}

# Basketball statistics provided by Nathan Yau at Flowing Data.
nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")

# tests with my data set
df <- getData()
df2 <- df[, c("prjage", "teamsize", "license", "prjmaturity")]
df3 <- df[, c("prjage", "teamsize", "license", "maturity")]


# This plot might be useful (but separate plots are better?)
ggpairs(df2, title = "Pairwise Scatterplots",
        lower=list(continuous = "smooth", params = c(colour = "blue")),
        upper=list(params = list(corSize = 6)),
        diag=list(continuous = "bar", params = c(colour = "blue")), 
        axisLabels = "show")

# Plot with 'prjmaturity' as color (should be a factor)
# (this plot doesn't seem to be very informational)
if (FALSE) {
  ggpairs(df3, title = "Pairwise Scatterplots",
          lower=list(continuous = "smooth", params = c(colour = "blue")),
          upper=list(params = list(corSize = 6)),
          diag=list(continuous = "bar", params = c(colour = "blue")), 
          axisLabels = "show",
          color = "maturity")
}
