# Start session with a clean R environment
rm(list = ls(all.names = TRUE))

if (!suppressMessages(require(lavaan))) install.packages('lavaan')
library(lavaan)

PRJ_HOME <- Sys.getenv("DISS_FLOSS_HOME") # getwd()

source(file.path(PRJ_HOME, "utils/data.R"))

MERGED_DIR <- file.path(PRJ_HOME, "data/merged")
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
columns4analysis <- c("Development.Team.Size",
                      "Project.Age",
                      "Project.License",
                      "License.Restrictiveness",
                      "Project.Maturity",
                      #"Software.Type",
                      "User.Community.Size")

cols4analysisNew <- c("team.size", "prj.age", "license", "restrict",
                      "maturity",
                      #"soft.type",
                      "commsize")

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


my.model.3 <- '

# regression
commsize ~ a1*team.size + a2*prj.age + a3*license +
           a4*restrict + a5*maturity
# + a6*soft.type

# variances
team.size ~~ team.size
prj.age   ~~ prj.age
license   ~~ license
restrict  ~~ restrict
maturity  ~~ maturity
#soft.type ~~ soft.type

# covariances/correlations (diag "+ soft.type" temp deleted)
team.size ~~ prj.age  + license  + restrict + maturity
prj.age   ~~ license  + restrict + maturity
license   ~~ restrict + maturity
restrict  ~~ maturity
#maturity  ~~ soft.type
'

if (FALSE) {
  
  mcarFIML.fit <- sem(my.model.3, data=flossData, missing="fiml",
                      fixed.x = FALSE)
  
  message("\nModel 3 - Analysis results 1:\n")
  summary(mcarFIML.fit, rsquare=TRUE, standardized=TRUE)
  
  message("\nModel 3 - Analysis results 2:\n")
  summary(mcarFIML.fit, fit.measures=TRUE, rsquare=TRUE, standardize=TRUE)
  
  # Wald test
  lavTestWald(mcarFIML.fit,
              constraints ='a1 == 0
                            a2 == 0
                            a3 == 0
                            a4 == 0
                            a5 == 0')
}


my.model.4 <- '

# regression
commsize  ~ 1
team.size ~ 1
prj.age   ~ 1
license   ~ 1
restrict  ~ 1
maturity  ~ 1
#soft.type ~ 1

# variances
team.size ~~ team.size
prj.age   ~~ prj.age
license   ~~ license
restrict  ~~ restrict
maturity  ~~ maturity
#soft.type ~~ soft.type

# covariances/correlations (diag "+ soft.type" temp deleted)
team.size ~~ prj.age  + license  + restrict + maturity
prj.age   ~~ license  + restrict + maturity
license   ~~ restrict + maturity
restrict  ~~ maturity
#maturity  ~~ soft.type
'

mcarFIML.fit <- sem(my.model.4, data=flossData, missing="fiml")
# alternatively, clear model's "means" section & use "meanstructure=TRUE"

message("\nModel 4 - Analysis results:\n")
summary(mcarFIML.fit, fit.measures=TRUE, standardize=TRUE)
# may add "rsquare=TRUE" in the future

# Missing data patterns and covariance coverage
inspect(mcarFIML.fit, 'patterns') 
inspect(mcarFIML.fit, 'coverage')

message("")
