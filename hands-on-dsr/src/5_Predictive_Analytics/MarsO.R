
## ----module, echo=FALSE, results="asis"----------------------------------
Module <- "MarsO"
cat(paste0("\\newcommand{\\Module}{", Module, "}"))


## ----setup, child="mycourse.Rnw"-----------------------------------------

## ----setup_options, include=FALSE----------------------------------------
library(knitr)
library(xtable)

opts_chunk$set(cache=FALSE)

opts_chunk$set(out.width='0.8\\textwidth')
opts_chunk$set(fig.align='center')

opts_chunk$set(src.top=NULL)
opts_chunk$set(src.bot=NULL)
opts_chunk$set(out.lines=4)
opts_chunk$set(out.truncate=80)

opts_chunk$set(fig.path=sprintf("figures/%s/", Module))
opts_chunk$set(cache.path=sprintf("cache/%s/", Module))
opts_chunk$set(bib.file=paste0(Module, ".bib"))

# Leave code as I have formatted it.

opts_chunk$set(tidy=FALSE)

# Hooks

# Allow auto crop of base graphics plots when crop=TRUE.

knit_hooks$set(crop=hook_pdfcrop)

# Truncate long lines and long output

hook_output <- knit_hooks$get("output")
hook_source <- knit_hooks$get("source")
knit_hooks$set(output=function(x, options) 
{
  if (options$results != "asis")
  {
    # Split string into separate lines.
    x <- unlist(stringr::str_split(x, "\n"))
    # Trim to the number of lines specified.
    if (!is.null(n <- options$out.lines)) 
    {
      if (length(x) > n) 
      {
        # Truncate the output.
        x <- c(head(x, n), "....\n")
      }
    }
    # Truncate each line to length specified.
    if (!is.null(m <- options$out.truncate))
    {
      len <- nchar(x)
      x[len>m] <- paste0(substr(x[len>m], 0, m-3), "...")
    }
    # Paste lines back together.
    x <- paste(x, collapse="\n")
    # Replace ' = ' with '=' - my preference. Hopefully won't 
    # affect things inappropriately.
    x <- gsub(" = ", "=", x)
  }
  hook_output(x, options)
},
source=function(x, options)
{
  # Split string into separate lines.
  x <- unlist(stringr::str_split(x, "\n"))
  # Trim to the number of lines specified.
  if (!is.null(n <- options$src.top)) 
  {
    if (length(x) > n) 
    {
      # Truncate the output.
      if (is.null(m <-options$src.bot)) m <- 0
      x <- c(head(x, n+1), "\n....\n", tail(x, m+2)) 
   }
  }
  # Paste lines back together.
  x <- paste(x, collapse="\n")
  hook_source(x, options)
})

# Optionally allow R Code chunks to be environments so we can refer to them.

knit_hooks$set(rcode=function(before, options, envir) 
{
  if (before)
    sprintf('\\begin{rcode}\\label{%s}\\hfill{}', options$label)
  else
    '\\end{rcode}'
})



## ----load_packages, message=FALSE----------------------------------------
library(rattle)         # The weather dataset and normVarNames().
library(randomForest)   # Impute missing values using na.roughfix().
library(dplyr)          # Data munging: tbl_df(), %>%.
library(ROCR)           # Use prediction() to convert to measures.
library(earth)          # An implementation of mars.


## ----additional_dependent_pacakges, echo=FALSE, message=FALSE------------
# These are dependencies that would otherwise be loaded as required.

library(stringr)
library(ggplot2)


## ----common_intro, child='documentation.Rnw', eval=TRUE------------------


## ----help_library, eval=FALSE, tidy=FALSE--------------------------------
## ?read.csv


## ----help_package, eval=FALSE--------------------------------------------
## library(help=rattle)


## ----record_start_time, echo=FALSE---------------------------------------
start.time <- proc.time()


## ----generate_bib, echo=FALSE, message=FALSE, warning=FALSE--------------
# Write all packages in the current session to a bib file
if (is.null(opts_chunk$get("bib.file"))) opts_chunk$set(bib.file="Course.bib")
write_bib(sub("^.*/", "", grep("^/", searchpaths(), value=TRUE)),
          file=opts_chunk$get("bib.file"))
system(paste("cat extra.bib >>", opts_chunk$get("bib.file")))
# Fix up specific issues.
# R-earth
system(paste("perl -pi -e 's|. Derived from .*$|},|'",
             opts_chunk$get("bib.file")))
# R-randomForest
system(paste("perl -pi -e 's|Fortran original by Leo Breiman",
             "and Adele Cutler and R port by|Leo Breiman and",
             "Adele Cutler and|'", opts_chunk$get("bib.file")))
# R-C50
system(paste("perl -pi -e 's|. C code for C5.0 by R. Quinlan|",
             " and J. Ross Quinlan|'", opts_chunk$get("bib.file")))
# R-caret
system(paste("perl -pi -e 's|. Contributions from|",
             " and|'", opts_chunk$get("bib.file")))
# Me
system(paste("perl -pi -e 's|Graham Williams|",
             "Graham J Williams|'", opts_chunk$get("bib.file")))




## ------------------------------------------------------------------------
library(rattle)         # Provides weather and normVarNames().
library(dplyr)          # Provides %>% and tbl_df().

dsname     <- "weather"
ds         <- get(dsname) %>% tbl_df()
names(ds)  <- normVarNames(names(ds))
vars       <- names(ds)
target     <- "rain_tomorrow"
risk       <- "risk_mm"
id         <- c("date", "location")


## ----out.lines=NULL------------------------------------------------------
ds


## ----review_ignore-------------------------------------------------------
# Ignore the IDs and the risk variable.
ignore     <- union(id, if (exists("risk")) risk)

# Ignore variables that look like identifiers.
ids        <- which(sapply(ds, function(x) length(unique(x))) == nrow(ds))
ignore     <- union(ignore, names(ids))

# Ignore variables which are completely missing.
mvc        <- sapply(ds[vars], function(x) sum(is.na(x))) # Missing value count.
mvn        <- names(ds)[(which(mvc == nrow(ds)))]         # Missing var names.
ignore     <- union(ignore, mvn)

# Ignore variables that are mostly missing - e.g., 70% or more missing
mvn        <- names(ds)[(which(mvc >= 0.7*nrow(ds)))]
ignore     <- union(ignore, mvn)

# Ignore variables with many levels.
factors    <- which(sapply(ds[vars], is.factor))
lvls       <- sapply(factors, function(x) length(levels(ds[[x]])))
many       <- names(which(lvls > 20))	# Factors with too many levels.
ignore     <- union(ignore, many)

# Ignore constants.
constants  <- names(which(sapply(ds[vars], function(x) all(x == x[1L]))))
ignore     <- union(ignore, constants)

# Initialise the variables
vars       <- setdiff(vars, ignore)


## ----out.lines=NULL------------------------------------------------------
vars
ignore


## ------------------------------------------------------------------------
ds[vars] <- na.roughfix(ds[vars])


## ----review_finalise-----------------------------------------------------
# Variable roles.
inputc     <- setdiff(vars, target)
inputi     <- sapply(inputc, function(x) which(x == names(ds)), USE.NAMES=FALSE)
numi       <- intersect(inputi, which(sapply(ds, is.numeric)))
numc       <- names(numi)
cati       <- intersect(inputi, which(sapply(ds, is.factor)))
catc       <- names(cati)

# Remove all observations with a missing target.
ds         <- ds[!is.na(ds[target]),]

# Normalise factors.
factors    <- which(sapply(ds[vars], is.factor))
for (f in factors) levels(ds[[f]]) <- normVarNames(levels(ds[[f]]))

# Ensure the target is categoric.
ds[target] <- as.factor(ds[[target]])

# Number of observations.
nobs       <- nrow(ds)


## ----out.lines=NULL------------------------------------------------------
library(earth)          # Model builder

# Formula for modelling.
form       <- formula(paste(target, "~ ."))

# Training and test datasets.
seed       <- sample(1:1000000, 1)
set.seed(seed)
train      <- sample(nobs, 0.7*nobs)
test       <- setdiff(seq_len(nobs), train)
actual     <- ds[test, target]
risks      <- ds[test, risk]

# Build model.
m.earth    <- earth(form, data=ds[train, vars])
mtype      <- "earth"
model      <- m.earth

model


## ------------------------------------------------------------------------
library(ROCR)           # prediction()

classes    <- predict(model, ds[test, vars], type="class")
acc        <- sum(classes == actual, na.rm=TRUE)/length(actual)
err        <- sum(classes != actual, na.rm=TRUE)/length(actual)
predicted  <- predict(model, ds[test, vars], type="response")
predicted  <- rescale(predicted, 0:1) # TRY THIS THEN READ DOCS
pred       <- prediction(predicted, ds[test, target])
ate        <- attr(performance(pred, "auc"), "y.values")[[1]]


## ----out.lines=NULL------------------------------------------------------
round(table(actual, classes, dnn=c("Actual", "Predicted"))/length(actual), 2)


## ----earth_weather_riskchart, echo=FALSE---------------------------------
library(rattle)         # riskchart()

riskchart(predicted, actual, risks)


## ----earth_weather_riskchart, eval=FALSE---------------------------------
## library(rattle)         # riskchart()
## 
## riskchart(predicted, actual, risks)


## ----common_outtro, child="finale.Rnw", eval=TRUE------------------------


## ----syinfo, child="sysinfo.Rnw", eval=TRUE------------------------------

## ----echo=FALSE, message=FALSE-------------------------------------------
require(Hmisc)
pkg <- "knitr"
pkg.version <- installed.packages()[pkg, 'Version']
pkg.date <- installed.packages(fields="Date")[pkg, 'Date']
pkg.info <- paste(pkg, pkg.version, pkg.date)

rev <- system("bzr revno", intern=TRUE)
cpu <- system(paste("cat /proc/cpuinfo | grep 'model name' |",
                    "head -n 1 | cut -d':' -f2"), intern=TRUE)
ram <- system("cat /proc/meminfo | grep MemTotal: | awk '{print $2}'",
              intern=TRUE)
ram <- paste0(round(as.integer(ram)/1e6, 1), "GB")
user <- Sys.getenv("LOGNAME")
node <- Sys.info()[["nodename"]]
user.node <- paste0(user, "@", node)
gcc.version <- system("g++ -v 2>&1 | grep 'gcc version' | cut -d' ' -f1-3",
                      intern=TRUE)
os <- system("lsb_release -d | cut -d: -f2 | sed 's/^[ \t]*//'", intern=TRUE)





