# Start session with a clean R environment
rm(list = ls(all.names = TRUE))

if (!suppressMessages(require(psych))) install.packages('psych')
library(psych)

# load data
source("../prepare/merge.R")

print(class(flossData))

# determine number of factors to extract

# parallel analysis
fa.parallel(WASIWRIT.cor, n.obs=152, fm="pa")

# Velicer’s minimum average partial (MAP)
VSS(WASIWRIT.cor, n.obs=152)
