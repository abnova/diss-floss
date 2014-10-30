# Start session with a clean R environment
## @knitr CleanEnv
rm(list = ls(all.names = TRUE))

##### PACKAGES #####

## @knitr LoadPackages
if (!suppressMessages(require(polycor))) install.packages('polycor')
if (!suppressMessages(require(lavaan))) install.packages('lavaan')

library(polycor)
library(lavaan)


##### SETUP #####

## @knitr PrepareCFA
set.seed(100)

PRJ_HOME <- Sys.getenv("DISS_FLOSS_HOME")

KNITR <<- isTRUE(getOption("knitr.in.progress"))

source(file.path(PRJ_HOME, "utils/data.R"))
source(file.path(PRJ_HOME, "utils/platform.R"))

READY4CFA_DIR  <- file.path(PRJ_HOME, "data/ready4cfa")
READY4CFA_FILE <- "flossData" # default

CFA_RESULTS_DIR <- file.path(PRJ_HOME, "results/cfa")

RDS_EXT      <- ".rds"
GRAPHICS_EXT <- ".svg"

DEBUG <- TRUE


##### ANALYSIS #####

message("\n\n===== PERFORMING CONFIRMATORY FACTOR ANALYSIS (CFA) =====")

fileName <- paste0(READY4CFA_FILE, RDS_EXT)
ready4cfaFile <- file.path(READY4CFA_DIR, fileName)

# load data
message("\n\n*** Loading data...")
flossData <- loadData(ready4cfaFile)

# due to very small amount of projects with "Non-OSI" licesnse
# and their disapperance due to calculating correlations,
# we don't include "License Category" into EFA (consider analyzing it
# at later phases with inclusion of imputed data)

# we also remove "Repo URL" due to injection of large # of NAs
# due to limiting conditionsat the end of the merge process

factors4Analysis <- c("Development Team Size", "Project Age",
                      "License Restrictiveness", "Project Stage",
                      "Software Type")
flossData <- flossData[factors4Analysis]

# convert names (temp)
names(flossData) <- make.names(names(flossData))

# sample the sample (use 1%) to reduce processing time
#flossData <- sampleDF(flossData, nrow(flossData) / 100)

# save name of the data set (seems redundant, but it will be useful,
# when there will be more than one data set, i.e. 'pilot' and 'main')
# [currently used for KNITR only]
datasetName <- deparse(substitute(flossData))

# log transform continuous data
flossData["Project.Age"] <- log(flossData["Project.Age"])
flossData["Development.Team.Size"] <- log(flossData["Development.Team.Size"])

# ===

# calculate standard deviation for each variable in the data set
# (was needed for cor2cov(), left for informative purposes)
#sdValues <- unlist(lapply(seq(length(names(flossData))),
#                          function(i) sd(flossData[[i]], na.rm = TRUE)))


# specify the latent variable model

model <- "

# factor structure (fix factors' loadings to 1)

f1 =~ 1 * Development.Team.Size
f2 =~ 1 * License.Restrictiveness
f3 =~ 1 * Project.Age + Software.Type

# fix some variances/residual variances to zero
# because < 3 indicators per latent variables

Development.Team.Size ~~ 0 * Development.Team.Size
License.Restrictiveness ~~ 0 * License.Restrictiveness
Project.Age ~~ 0 * Project.Age
Software.Type ~~ 0 * Software.Type

# covariances between the latent variables

f1 ~~ f2
f1 ~~ f3
f2 ~~ f3
"

# perform CFA
message("\n*** Performing CFA of the model...")

fit <- cfa(model, data = flossData, meanstructure = TRUE,
           missing = "pairwise", estimator = "DWLS") # WLSMV

# output results
summary(fit, fit.measures = TRUE, standardize = TRUE)
