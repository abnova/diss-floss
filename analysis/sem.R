# Start session with a clean R environment
rm(list = ls(all.names = TRUE))

if (!suppressMessages(require(plspm))) install.packages('plspm')
library(plspm)

set.seed(100)

PRJ_HOME <- Sys.getenv("DISS_FLOSS_HOME")

KNITR <<- isTRUE(getOption("knitr.in.progress"))

source(file.path(PRJ_HOME, "utils/data.R"))
source(file.path(PRJ_HOME, "utils/platform.R"))
source(file.path(PRJ_HOME, "utils/qgraphtikz.R")) # fix for TikZ device
source(file.path(PRJ_HOME, "utils/factors.R"))

READY4CFA_DIR  <- file.path(PRJ_HOME, "data/ready4cfa")
READY4CFA_FILE <- "flossData"

CFA_RESULTS_DIR <- file.path(PRJ_HOME, "results/cfa")

RDS_EXT      <- ".rds"
GRAPHICS_EXT <- ".svg"

DEBUG <- FALSE


## @knitr PerformCFA

##### ANALYSIS #####

message("\n\n===== PERFORMING STRUCTURED EQUATION MODELING (SEM-PLS) =====")

fileName <- paste0(READY4CFA_FILE, RDS_EXT)
ready4cfaFile <- file.path(READY4CFA_DIR, fileName)

# load data
message("\n\n*** Loading data...")
flossData <- loadData(ready4cfaFile)

# handle missing data
flossData <- na.omit(flossData)

# temp
names(flossData) <- make.names(names(flossData))

# due to very small amount of projects with "Non-OSI" licesnse
# and their disapperance due to calculating correlations,
# we don't include "License Category" into EFA (consider analyzing it
# at later phases with inclusion of imputed data)

# we also remove "Repo URL" due to injection of large # of NAs
# due to limiting conditionsat the end of the merge process

factors4analysis <- c("Development.Team.Size", # "Project Age",
                      "License.Restrictiveness", "Project.Stage",
                      "Software.Type")
flossData <- flossData[, factors4analysis]

# convert names (temp)
#names(flossData) <- make.names(names(flossData)) #TODO: test & remove

# sample the sample (use 1%) to reduce processing time
#flossData <- sampleDF(flossData, nrow(flossData) / 100)

# save name of the data set (seems redundant, but it will be useful,
# when there will be more than one data set, i.e. 'pilot' and 'main')
# [currently used for KNITR only]
datasetName <- deparse(substitute(flossData))

# log transform continuous data
#flossData["Project.Age"] <- log(flossData["Project.Age"])
flossData["Development.Team.Size"] <- log(flossData["Development.Team.Size"])


###

# rows of the path matrix
Governance  <- c(0, 0) # 0, 0, 0
#Sponsorship <- c(0, 0, 0)
Success     <- c(1, 0) # 1, 1, 0

# inner model matrix
successPath <- rbind(Governance, Success) # Sponsorship, 

# add column names
colnames(successPath) <- rownames(successPath)

# blocks of indicators (outer model)
#successBlocks <- list(2:3, 4) # 5:8, 9:12

depVar <- "Development.Team.Size"  # "User.Community.Size"

# new list of blocks (with names of variables)
successBlocks <- list(setdiff(factors4analysis, depVar), c(depVar))

# vector of modes (reflective)
successModes <- rep("A", 2) # 3

# keep only the necessary columns, dropping the rest
#keepCols <- c("Repo URL", "Project License",
#              "License Restrictiveness", "User Community Size")
#flossData <- flossData[, keepCols, drop = FALSE]

# convert factors to numeric values
# convert factors to integers via as.numeric.factor() ["factors.R"]
# the above doesn't work - however, as.integer() works just fine

if (FALSE)
for (x in factors4analysis)
  flossData[[x]] <- as.integer(flossData[[x]])

for (x in factors4analysis)
  flossData[[x]] <- as.numeric.factor(flossData[[x]])

print(str(flossData))


# specify measurement scale for manifest variables
successScales <- list(c("ord", "ord", "ord"), c("num"))


# run plspm analysis
successPLS <- plspm(flossData,
                    successPath,
                    successBlocks,
                    modes = successModes,
                    scaling = successScales)

# 4.2. Handling PLS-PM Results

# what's in foot_pls?
print(successPLS)

# summarized results
print(summary(successPLS))

# 4.3. Measurement Model Assessment: Reflective Indicators

# plotting loadings
gLoadings <- plot(successPLS, what = "loadings")
print(gLoadings)

# outer model results (in a matrix way, unlike tabular in summary())
print(successPLS$outer_model)

# Defense outer model results
print(subset(successPLS$outer_model, block == "Defense"))

# plotting weights
gWeights <- plot(successPLS, what = "weights")
print(gWeights)

# add two more columns NGCH and NGCA
#spainfoot$NGCH = -1 * spainfoot$GCH
#spainfoot$NGCA = -1 * spainfoot$GCA

# check column names
print(names(flossData))

# new list of blocks (with column positions of variables)
newBlocksPos <- list(1:4, c(15,16,7,8), 9:12)

# new list of blocks (with names of variables)
newBlocksStr <- list(
  c("GSH", "GSA", "SSH", "SSA"),
  c("NGCH", "NGCA", "CSH", "CSA"),
  c("WMH", "WMA", "LWR", "LRWL"))

# re-apply plspm
#successPLS <- plspm(flossData,
#                    successPath, newBlocksStr,
#                    modes = successModes)

# plot loadings
gLoadings2 <- plot(successPLS, "loadings")
print(gLoadings2)

# unidimensionality - better results
print(successPLS$unidim)

# loadings and communalities
print(successPLS$outer_model)

# cross-loadings
print(successPLS$crossloadings)

# load ggplot2 and reshape
library(ggplot2)
library(reshape)

# reshape crossloadings data.frame for ggplot
xloads = melt(successPLS$crossloadings, id.vars = c("name", "block"),
              variable_name = "LV")

# bar-charts of crossloadings by block
ggplot(data = xloads,
       aes(x = name, y = value, fill = block)) +
  
  # add horizontal reference lines
  geom_hline(yintercept = 0, color = "gray75") +
  geom_hline(yintercept = 0.5, color = "gray70", linetype = 2) +
  
  # indicate the use of car-charts
  geom_bar(stat = 'identity', position = 'dodge') +
  
  # panel display (i.e. faceting)
  facet_wrap(block ~ LV) +
  
  # tweaking some grahical elements
  theme(axis.text.x = element_text(angle = 90),
        line = element_blank(),
        plot.title = element_text(size = 12)) +
  
  # add title
  ggtitle("Crossloadings")


# 4.4. Measurement Model Assessment: Formative Indicators


# 4.5. Structural Model Assessment

# inner model
print(successPLS$inner_model)

# inner model summary
print(successPLS$inner_summary)

# select R2
print(successPLS$inner_summary[, "R2", drop = FALSE])

# GoF index
print(successPLS$gof)

# 4.6. Validation

# running bootstrap validation (200 samples)
successVal <- plspm(flossData, successPath, newBlocksStr,
                    modes = successModes,
                    boot.val = TRUE, br = 200)


# bootstrap results
print(successVal$boot)
