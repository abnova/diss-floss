# Start session with a clean R environment
rm(list = ls(all.names = TRUE))

if (!suppressMessages(require(psych))) install.packages('psych')
if (!suppressMessages(require(mvtnorm))) install.packages('mvtnorm')
#if (!suppressMessages(require(GPArotation))) 
#  install.packages('GPArotation')

library(psych)
library(mvtnorm)
#library(GPArotation)


PRJ_HOME <- getwd()

# load data (commented out, as data are loaded in "missing.R")
#source("../prepare/merge.R")

# handle missing values (temporarily here)
source(file.path(PRJ_HOME, "prepare/missing.R"))

message("\n\n===== PERFORMING CONFIRMATORY FACTOR ANALYSIS (CFA) =====")

message("\n\n*** Determining correlations between items...\n")

# estimate tetrachoric correlations between items
message("\nTetrachoric correlations:")
message("=========================\n")

message("Currently disabled.\n")
#tetrachoric(flossData)

# estimate tetrachoric correlations between items
message("\nPolychoric correlations:")
message("========================\n")

# "You have more than 8 categories for your items,
#  polychoric is probably not needed"
message("Currently disabled.\n")
#polychoric(flossData)

#biserial(x, y)
#polyserial(x, y)