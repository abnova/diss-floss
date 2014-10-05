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
  df$prjmaturity <- df[["Project Stage"]]
  
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

# first corrgram example
library(corrgram)
corrgram(mtcars, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Car Milage Data in PC2/PC1 Order")

# second corrgram example
corrgram(mtcars, order=NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt,
         main="Car Milage Data (unsorted)")

# change color scheme
col.regions <- function(ncol){   
  colorRampPalette(c("darkgoldenrod4", "burlywood1",
                     "darkkhaki", "darkgreen"))(ncol)}

# third corrgram example (with changed colors)
corrgram(mtcars, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlogram of Car Mileage Data (PC2/PC1 Order)")
