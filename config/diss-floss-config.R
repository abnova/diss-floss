# ---------------------------------------------------------------------------
# Project-wide configuration file for general options (On/Off, single values)
# ---------------------------------------------------------------------------

# project's directories
PRJ_HOME <- Sys.getenv("DISS_FLOSS_HOME")

CACHE_DIR       <- file.path(PRJ_HOME, "cache")
TRANSFORMED_DIR <- file.path(PRJ_HOME, "data/transformed")

READY4EDA_DIR  <- file.path(PRJ_HOME, "data/ready4eda")
READY4EDA_FILE <- "flossData"

# Initially file was copied manually from "merged/SourceForge".
# Currently it is copied as a part of Makefile's merge rule.
# Implementing automatic data merging across all data sources
# should take care of this step (TODO).
MERGED_DIR  <- file.path(PRJ_HOME, "data/merged")
MERGED_FILE <- "flossData"

IMPUTED_DIR  <- file.path(PRJ_HOME, "data/imputed")
IMPUTED_FILE <- "flossDataImputed"

READY4EFA_DIR  <- file.path(PRJ_HOME, "data/ready4efa")
READY4EFA_FILE <- "flossData"

READY4CFA_DIR  <- file.path(PRJ_HOME, "data/ready4cfa")
READY4CFA_FILE <- "flossData"

READY4SEM_DIR  <- file.path(PRJ_HOME, "data/ready4sem")
READY4SEM_FILE <- "flossData"

EDA_RESULTS_DIR <- file.path(PRJ_HOME, "results/eda")
EFA_RESULTS_DIR <- file.path(PRJ_HOME, "results/efa")
CFA_RESULTS_DIR <- file.path(PRJ_HOME, "results/cfa")
SEM_RESULTS_DIR <- file.path(PRJ_HOME, "results/sem")


# domain-specific constants

# condition for selecting projects (after merging),
# based on amount of missing data
MIN_NUM_INDICATORS <- 10
MIN_NUM_PROJECTS   <- 50000


# manuscript's body vs. appendix control
DO_APPENDIX = FALSE


# general statistical control
DIGITS <- 3

# construct format string, based on the configuration setting
FMT_FP_DECIMAL <- paste0("%.", DIGITS, "f  ")


# nissing data analysis control
NUM_IMPUTATIONS <- 5  # minimum recommended number of imputations
NUM_IMP_EXTRACT <- 1  # number of imputed data sets to extract

DO_MISSING_ANALYSIS <- TRUE
DO_MISSING_MAIN_MI  <- FALSE
DO_MISSING_VIZ_MI   <- TRUE

# EDA analysis control
DO_MIX_ANALYSIS <- FALSE
DO_MULTI_VISUAL <- TRUE


# SEM analysis control
DO_SEM_DIRECT_EFF <- TRUE
DO_SEM_MEDIATION  <- TRUE
DO_SEM_MODERATION <- TRUE

DO_SEM_BOOT <- FALSE


# reproducibility
RNG_SEED <- 100
set.seed(RNG_SEED)

# debugging settings
DEBUG <- TRUE    # some debug information
DEBUG2 <- FALSE  # more detailed debug information

# environment settings
R_ENV_FILE <- "~/.Renviron"

# file-related settings
RDATA_EXT <- ".RData"
RDS_EXT <- ".rds"

GRAPHICS_EXT <- ".svg"  # default format for saving plots

# knitr-related
KNITR <<- isTRUE(getOption("knitr.in.progress"))

myDPI  <<- 300
myScale <<- 1
