# compares results of SEM-PLS analysis

# Start session with a clean R environment
## @knitr CleanEnv
rm(list = ls(all.names = TRUE))

## @knitr LoadPackages
if (!suppressMessages(require(plspm))) install.packages('plspm')
if (!suppressMessages(require(mice))) install.packages('mice')
if (!suppressMessages(require(ggplot2))) install.packages('ggplot2')
if (!suppressMessages(require(RColorBrewer)))
  install.packages('RColorBrewer')
if (!suppressMessages(require(reshape))) install.packages('reshape')
if (!suppressMessages(require(pander))) install.packages('pander')

library(plspm)
library(mice)
library(ggplot2)
library(RColorBrewer)
library(reshape)
library(pander)


##### PREPARATION & DEFINITIONS #####

## @knitr PrepareSEMcompare

PRJ_HOME <- Sys.getenv("DISS_FLOSS_HOME")

source(file.path(PRJ_HOME, "config/diss-floss-config.R"))
source(file.path(PRJ_HOME, "utils/data.R"))


models <- c("directEffects.rds", "mediation.rds")  # , "moderation"
names(models) <- c("directEffects", "mediation")

directEffFile <- file.path(SEM_RESULTS_DIR, models[["directEffects"]])
mediationFile <- file.path(SEM_RESULTS_DIR, models[["mediation"]])
  
directEffResults <- loadData(directEffFile)
mediationResults <- loadData(mediationFile)

# compare GoF and R2
message("GoF for model with direct effects: ",
        round(directEffResults$gof, DIGITS))

message("GoF for model with mediation: ",
        round(mediationResults$gof, DIGITS))
