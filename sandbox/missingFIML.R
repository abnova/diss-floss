# Start session with a clean R environment
rm(list = ls(all.names = TRUE))

if (!suppressMessages(require(lavaan))) install.packages('lavaan')
library(lavaan)

source("../utils/data.R")

MERGED_DIR <- "~/diss-floss/data/merged"
MERGED_FILE <- "flossData" # default
RDS_EXT <- ".rds"


# prepare
fileName <- paste0(MERGED_FILE, RDS_EXT)
mergedFile <- file.path(MERGED_DIR, fileName)

# load data
message("\nLoading data...")
flossData <- loadData(mergedFile)

# analysis
message("\nAnalyzing data...")

# select columns for analysis by their names
#columns4analysis <- c(8, 11, 12, 13, 15, 16, 17)
columns4analysis <- c("Development Team Size",
                      "Project Age",
                      "Project License",
                      "License Restrictiveness",
                      "Project Maturity",
                      "Software Type",
                      "User Community Size")

cols4analysisNew <- c("team.size", "prj.age", "license", "restrict",
                      "maturity", "soft.type", "commsize")

# delete the rest of the columns
flossData[, setdiff(names(flossData), columns4analysis)] <- list(NULL)

# rename working columns for convenience
names(flossData)[sapply(colnames(flossData),
                        grep, names(flossData))] <- cols4analysisNew

my.model.1 <- '
commsize =~ a*license + b*restrict
'

my.model.2 <- '

# means
commsize ~ 1
license  ~ 1
restrict ~ 1

# variances
commsize ~~ commsize
license  ~~ license
restrict ~~ restrict

# covariances/correlations
license  ~~ restrict
'

mcarFIML.fit <- sem(my.model.2, data=flossData, missing="fiml")

message("\nAnalysis results:\n")
summary(mcarFIML.fit, rsquare=TRUE, standardized=TRUE)

message("")
