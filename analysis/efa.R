# Start session with a clean R environment
rm(list = ls(all.names = TRUE))

if (!suppressMessages(require(psych))) install.packages('psych')
if (!suppressMessages(require(GPArotation))) 
  install.packages('GPArotation')

library(psych)
library(GPArotation)

# load data (commented out, as data are loaded in "missing.R")
#source("../prepare/merge.R")

# handle missing values (temporarily here)
source("~/diss-floss/prepare/missing.R")

# determine number of factors to extract

# parallel analysis
#fa.parallel(flossData, n.obs = nrow(flossData), fm = "pa")
fa.parallel(flossData, fm = "pa")

# Velicer’s minimum average partial (MAP)
#VSS(flossData, n.obs = nrow(flossData))
VSS(flossData)
