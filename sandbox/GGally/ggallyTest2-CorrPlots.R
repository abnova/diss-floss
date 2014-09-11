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

if (FALSE) {
  # Default output.
  ggcorr(nba[, -1])
}

if (FALSE) {
  # Labelled output, with coefficient transparency.
  ggcorr(nba[, -1],
         label = TRUE,
         label_alpha = TRUE,
         name = "") +
    theme(legend.position = "bottom")
}

if (FALSE) {
  # Custom options.
  ggcorr(
    nba[, -1],
    geom = "circle",
    max_size = 6,
    size = 3,
    hjust = 0.75,
    angle = -45,
    palette = "PuOr" # colorblind safe, photocopy-able
  ) + labs(title = "Points Per Game")
}


# tests with my data set
df <- getData()
df <- df[, c("prjage", "teamsize", "license", "prjmaturity")]

#brewer.pal(length(colnames(df)), "Set1")

#The diverging palettes are
#BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral

# Default output.
ggcorr(df)

# Custom options.
# produces error: "Breaks and labels are different lengths"
if (FALSE) {
  ggcorr(df,
         geom = "point",
         max_size = 6,
         size = 3,
         hjust = 0.75,
         #angle = -45,
         palette = "Accent" # Paired / Spectral RdYlBu RdYlGn
  ) + labs(title = "Basic correlations plot")
}


ggcorr(df,
       #geom = "point",
       #max_size = 6,
       #size = 3,
       #hjust = 0.75,
       #angle = 45,
       palette = "Accent"
) + labs(title = "Basic correlations plot")