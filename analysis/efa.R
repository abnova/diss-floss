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

message("\n\n===== PERFORMING EXPLORATORY FACTOR ANALYSIS (EFA) =====")

# determine number of factors to extract
message("\n\n*** Determining number of factors to extract...\n")

# parallel analysis
message("\nParallel analysis:")
message("==================\n")

#fa.parallel(flossData, n.obs = nrow(flossData), fm = "pa")
fa.parallel(flossData, fm = "pa")

message("\n\nVery Simple Structure (VSS) analysis:")
message("=====================================")

# Velicer’s minimum average partial (MAP)
#VSS(flossData, n.obs = nrow(flossData))
VSS(flossData)

# TODO: automate passing determined number of factors

message("\n\n*** Performing factor analysis (FA)...\n\n")

message("FA, using principal axis method:")
message("================================\n")

# perform FA, using principal axis method
fa(flossData, nfactors = 2, fm = "pa")

message("\n\nFA with 'varimax' rotation:")
message("===========================\n")

message("Currently disabled.")
# perform FA with 'varimax' rotation
#promax.fa <- fa(flossData, nfactors = 2, fm = "pa",
#                rotate = "promax")
#print(promax.fa$Structure)

message("\n\nFA with 'quartimin' rotation:")
message("=============================\n")

message("Currently disabled.\n")
# perform FA with 'quartimin' rotation
#quartimin <- factanal(flossData, factors = 2,
#                      rotation = "cfQ",
#                      control = list(rotate = list(kappa = 0)))
#print(quartimin, cutoff = 0.001)

message("\n\nFA, using Schmid-Leiman transformation:")
message("=======================================\n")

schmid(flossData, nfactors = 2)

message("\n\nFA with 'bi-factor' rotation:")
message("=============================\n")

fa(flossData, nfactors = 3, fm="pa",
   rotate = "bifactor", max.iter = 500)
