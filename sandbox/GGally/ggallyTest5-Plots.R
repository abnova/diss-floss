#' GGally @examples for correlation plots
#' 
rm(list = ls(all.names = TRUE))

library("GGally")
#library(RColorBrewer)

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
  
  return (df)
}

# Basketball statistics provided by Nathan Yau at Flowing Data.
nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")

# tests with my data set
df <- getData()
df2 <- df[, c("prjage", "teamsize", "license", "prjmaturity")]

#brewer.pal(length(colnames(df)), "Set1")

#The diverging palettes are
#BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral

# Plot relationships with sex as color
ggpairs(df2, title = "Pairwise Scatterplots",
        diag = list(continuous = "density", discrete = "bar"),
        axisLabels = "none",
        lower = list(continuous = "smooth"),
        upper = list(combo = "box"),
        color = "prjmaturity")
