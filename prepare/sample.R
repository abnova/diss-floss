# Start session with a clean R environment
rm(list = ls(all.names = TRUE))

if (!suppressMessages(require(lavaan))) install.packages('lavaan')
library(lavaan)

PRJ_HOME <- Sys.getenv("DISS_FLOSS_HOME") # getwd()

source(file.path(PRJ_HOME, "utils/data.R"))

MERGED_DIR <- file.path(PRJ_HOME, "data/merged")
MERGED_FILE <- "flossData" # default
RDS_EXT <- ".rds"

# choose parameters for estimating statistical power
ALPHA <- 0.10
BETA  <- 0.20
POWER <- 1 - BETA

# prepare
fileName <- paste0(MERGED_FILE, RDS_EXT)
mergedFile <- file.path(MERGED_DIR, fileName)

# load data
message("\nLoading data...")
flossData <- loadData(mergedFile)

# analysis
message("\nAnalyzing data...")

# the following calculation an effect size for correlational designs
# is based based on this paper:
# http://jpepsy.oxfordjournals.org/content/34/9/917.full

# computing r from an independent t-test:
# r = sqrt(t^2 / (t^2 + df))

# below we calculate Cohen's f^2 statistic as a measure
# of effect size for cases of multiple regression
# (http://www.saylor.org/site/wp-content/uploads/2011/08/PSYCH202A-6.1.4-Effect-size.pdf)
# f^2 = R^2 / 1 - R^2
# By convention, f^2 effect sizes of 0.02, 0.15, and 0.35 are termed
# small, medium, and large, respectively (Cohen, 1988)
f2 <- MBESS::ss.aipe.R2