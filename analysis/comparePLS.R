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


models <- c("directEffects.rds", "mediation.rds", "moderation.rds")
names(models) <- c("directEffects", "mediation", "moderation")

directEffFile <- file.path(SEM_RESULTS_DIR, models[["directEffects"]])
mediationFile <- file.path(SEM_RESULTS_DIR, models[["mediation"]])
moderationFile <- file.path(SEM_RESULTS_DIR, models[["moderation"]])

directEffResults <- loadData(directEffFile)
mediationResults <- loadData(mediationFile)
moderationResults <- loadData(moderationFile)

# present summary statistics (GoF, R2, etc.) for models comparison
message("Summary statistics for model with direct effects:\n",
        "-------------------------------------------------\n\n",
        "GoF: ", sprintf(FMT_FP_DECIMAL, directEffResults$gof), "\n",
        "R2:  ", sprintf(FMT_FP_DECIMAL,
                         directEffResults$inner_summary$R2), "\n")

message("Summary statistics for model with mediation:\n",
        "--------------------------------------------\n\n",
        "GoF: ", sprintf(FMT_FP_DECIMAL, mediationResults$gof), "\n",
        "R2:  ", sprintf(FMT_FP_DECIMAL,
                         mediationResults$inner_summary$R2), "\n")

message("Summary statistics for model with moderation:\n",
        "--------------------------------------------\n\n",
        "GoF: ", sprintf(FMT_FP_DECIMAL, moderationResults$gof), "\n",
        "R2:  ", sprintf(FMT_FP_DECIMAL,
                         moderationResults$inner_summary$R2), "\n")


# bootstrapping validation statistics

if (DO_SEM_BOOT) {
  
  models <- c("directEffects-boot.rds", "mediation-boot.rds")
  names(models) <- c("directEffects", "mediation")
  
  directEffFile <- file.path(SEM_RESULTS_DIR, models[["directEffects"]])
  mediationFile <- file.path(SEM_RESULTS_DIR, models[["mediation"]])
  
  directEffResults <- loadData(directEffFile)
  mediationResults <- loadData(mediationFile)
  
  # present summary statistics (GoF, R2, etc.) for models comparison
  message("Summary statistics for model with direct effects:\n",
          "-------------------------------------------------\n\n",
          "Outer weights: ",
          sprintf(FMT_FP_DECIMAL, directEffResults$boot$weights), "\n",
          "Loadings: ",
          sprintf(FMT_FP_DECIMAL, directEffResults$boot$loadings), "\n",
          "Path coefficients: ",
          sprintf(FMT_FP_DECIMAL, directEffResults$boot$paths), "\n",
          "R2:  ",
          sprintf(FMT_FP_DECIMAL, directEffResults$boot$rsq), "\n",
          "Total effects: ",
          sprintf(FMT_FP_DECIMAL, directEffResults$boot$total.efs), "\n")
  
  message("Summary statistics for model with mediation:\n",
          "--------------------------------------------\n\n",
          "Outer weights: ",
          sprintf(FMT_FP_DECIMAL, mediationResults$boot$weights), "\n",
          "Loadings: ",
          sprintf(FMT_FP_DECIMAL, mediationResults$boot$loadings), "\n",
          "Path coefficients: ",
          sprintf(FMT_FP_DECIMAL, mediationResults$boot$paths), "\n",
          "R2:  ",
          sprintf(FMT_FP_DECIMAL, mediationResults$boot$rsq), "\n",
          "Total effects: ",
          sprintf(FMT_FP_DECIMAL, mediationResults$boot$total.efs), "\n")
}
