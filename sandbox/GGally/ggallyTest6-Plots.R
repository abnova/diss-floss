# GGally @examples for correlation plots
 
rm(list = ls(all.names = TRUE))

library("GGally")

PRJ_HOME <- Sys.getenv("DISS_FLOSS_HOME")

source(file.path(PRJ_HOME, "utils/data.R"))

MERGED_DIR <- file.path(PRJ_HOME, "data/merged")
MERGED_FILE <- "flossData" # default
RDS_EXT <- ".rds"

fileName <- paste0(MERGED_FILE, RDS_EXT)
mergedFile <- file.path(MERGED_DIR, fileName)

# load data
message("\nLoading data...")
df <- loadData(mergedFile)

# make column names syntactically valid
names(df) <- make.names(names(df))

# select columns
df2 <- df[, c("Project.Age", "Development.Team.Size", "User.Community.Size",
              "License.Restrictiveness", "Project.Maturity")]
df3 <- df[, c("Project.Age", "Development.Team.Size", "User.Community.Size",
              "License.Restrictiveness", "Project.Maturity")]


# This plot might be useful (but separate plots are better?)
g1 <- ggpairs(df2, title = "Pairwise Scatterplots",
        lower=list(continuous = "smooth", params = c(colour = "blue")),
        upper=list(params = list(corSize = 6)),
        diag=list(continuous = "bar", params = c(colour = "blue")), 
        axisLabels = "show")
print(g1)

# Plot with 'prjmaturity' as color (should be a factor)
# (this plot doesn't seem to be very informative)
if (TRUE) {
  g2 <- ggpairs(df3, title = "Pairwise Scatterplots",
          lower=list(continuous = "smooth", params = c(colour = "blue")),
          upper=list(params = list(corSize = 6)),
          diag=list(continuous = "bar", params = c(colour = "blue")), 
          axisLabels = "show",
          color = "Project.Maturity")
  print(g2)
}
