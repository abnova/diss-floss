# Execute global R profile first...
source("~/.Rprofile")

# ...then local project R setup
#r <- getOption("repos")
#r["CRAN"] <- "http://cran.rstudio.com"
#options(repos = r)

#DISS_FLOSS_HOME <<- Sys.getenv("DISS_FLOSS_HOME")

# Set the project-wide working directory, based on a user-defined
# environment variable. Now use 'getwd()' to build any full path.
# In our case, a generally preferred practice to only use
# relative paths is not a good idea due to need of running
# specific R scripts anywhere in the project directory tree.
setwd(Sys.getenv("DISS_FLOSS_HOME"))
