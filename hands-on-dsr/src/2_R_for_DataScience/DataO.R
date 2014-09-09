
## ----module, echo=FALSE, results="asis"----------------------------------
Module <- "DataO"
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

opts_chunk$set(background='#E7E7E7')

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
library(rattle)          # The weather dataset and normVarNames().
library(randomForest)    # Impute missing values using na.roughfix().
library(tidyr)           # Tidy the dataset.
library(ggplot2)         # Visualise data.
library(dplyr)           # Data preparation and pipes %>%.
library(lubridate)       # Handle dates.
library(FSelector)       # Feature selection.


## ----additional_dependent_pacakges, echo=FALSE, message=FALSE------------
# These are dependencies that would otherwise be loaded as required.

library(stringr)


## ----common_intro, child='documentation.Rnw', eval=TRUE------------------


## ----help_library, eval=FALSE, tidy=FALSE--------------------------------
## ?read.csv


## ----help_package, eval=FALSE--------------------------------------------
## library(help=rattle)


## ----child-bib, child='generatebib.Rnw', eval=TRUE-----------------------

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





## ----eval=FALSE----------------------------------------------------------
## dspath <- "http://rattle.togaware.com/weather.csv"


## ------------------------------------------------------------------------
dspath <- system.file("csv", "weather.csv", package="rattle")


## ----dataset_load--------------------------------------------------------
weather <- read.csv(dspath)


## ------------------------------------------------------------------------
library(rattle)         # weather, normVarNames().


## ----basic_summary_weather-----------------------------------------------
dim(weather)
names(weather)
str(weather)


## ----prepare_the_dataset, out.lines=7------------------------------------
dsname <- "weather"
ds     <- get(dsname)
dim(ds)
names(ds)


## ----alternative_dataset_assignment, eval=FALSE--------------------------
## ds <- weather


## ----using_tbl_df--------------------------------------------------------
class(ds)
ds <- tbl_df(ds)
class(ds)


## ----tbl_df_print, out.lines=NULL----------------------------------------
ds


## ----dataset_head, out.lines=10------------------------------------------
head(ds)


## ----dataset_tail, out.lines=10------------------------------------------
tail(ds)


## ----dataset_sample, out.lines=10----------------------------------------
ds[sample(nrow(ds), 6),]


## ----dataset_structure, out.lines=30-------------------------------------
str(ds)


## ----dataset_summary, out.lines=43---------------------------------------
summary(ds)


## ----message=FALSE-------------------------------------------------------
names(ds)
names(ds) <- normVarNames(names(ds))
names(ds)


## ------------------------------------------------------------------------
sapply(ds, class)


## ----convert_date, message=FALSE-----------------------------------------
library(lubridate)     # ymd()

head(ds$date)
ds$date <- ymd(as.character(ds$date))
head(ds$date)


## ------------------------------------------------------------------------
sapply(ds, class)


## ----variable_roles, out.lines=NULL--------------------------------------
(vars  <- names(ds))
target <- "rain_tomorrow"
risk   <- "risk_mm"
id     <- c("date", "location")


## ------------------------------------------------------------------------
ignore <- union(id, if (exists("risk")) risk)


## ------------------------------------------------------------------------
(ids   <- which(sapply(ds, function(x) length(unique(x))) == nrow(ds)))
ignore <- union(ignore, names(ids))


## ----ignore_missing_variables--------------------------------------------
mvc <- sapply(ds[vars], function(x) sum(is.na(x)))
mvn <- names(which(mvc == nrow(ds)))
ignore <- union(ignore, mvn)


## ----ignore_mostly_missing_variables-------------------------------------
mvn <- names(which(mvc >= 0.7*nrow(ds)))
ignore <- union(ignore, mvn)


## ----ignore_factors_with_many_levels-------------------------------------
factors <- which(sapply(ds[vars], is.factor))
lvls    <- sapply(factors, function(x) length(levels(ds[[x]])))
(many   <- names(which(lvls > 20)))
ignore  <- union(ignore, many)


## ----ignore_variables_constant_values------------------------------------
(constants <- names(which(sapply(ds[vars], function(x) all(x == x[1L])))))
ignore     <- union(ignore, constants)


## ------------------------------------------------------------------------
mc <- cor(ds[which(sapply(ds, is.numeric))], use="complete.obs")
mc[upper.tri(mc, diag=TRUE)] <- NA
mc <-
  mc                                                                  %>% 
  abs()                                                               %>% 
  data.frame()                                                        %>% 
  mutate(var1=row.names(mc))                                          %>% 
  gather(var2, cor, -var1)                                            %>% 
  na.omit()
mc <- mc[order(-abs(mc$cor)),]
mc


## ------------------------------------------------------------------------
ignore <- union(ignore, c("temp_3pm", "pressure_9am", "temp_9am"))


## ------------------------------------------------------------------------
length(vars)
vars <- setdiff(vars, ignore)
length(vars)


## ----out.lines=NULL------------------------------------------------------
library(FSelector)      # information.gain()

form <- formula(paste(target, "~ ."))
cfs(form, ds[vars])
information.gain(form, ds[vars])


## ----remove_missing_target-----------------------------------------------
dim(ds)
sum(is.na(ds[target]))
ds <- ds[!is.na(ds[target]),]
sum(is.na(ds[target]))
dim(ds)


## ------------------------------------------------------------------------
ods <- ds


## ----impute_missing_values-----------------------------------------------
dim(ds[vars])
sum(is.na(ds[vars]))
ds[vars] <- na.roughfix(ds[vars])
sum(is.na(ds[vars]))
dim(ds[vars])


## ------------------------------------------------------------------------
ds <- ods


## ------------------------------------------------------------------------
ods <- ds
omit <- NULL


## ----remove_missing_values-----------------------------------------------
dim(ds[vars])
sum(is.na(ds[vars]))
mo <- attr(na.omit(ds[vars]), "na.action")
omit <- union(omit, mo)
if (length(omit)) ds <- ds[-omit,]
sum(is.na(ds[vars]))
dim(ds[vars])


## ------------------------------------------------------------------------
ds <- ods


## ----normalise_factors---------------------------------------------------
factors <- which(sapply(ds[vars], is.factor))
for (f in factors) levels(ds[[f]]) <- normVarNames(levels(ds[[f]]))


## ----ensure_target_is_categoric------------------------------------------
ds[target] <- as.factor(ds[[target]])
table(ds[target])


## ----fig.height=4--------------------------------------------------------
p <- ggplot(ds, aes_string(x=target))
p <- p + geom_bar(width=0.2)
print(p)


## ----identify_variables_inputc-------------------------------------------
inputc  <- setdiff(vars, target)
inputc


## ----identify_variables_inputi-------------------------------------------
inputi  <- sapply(inputc, function(x) which(x == names(ds)), USE.NAMES=FALSE)
inputi


## ----number_of_observations----------------------------------------------
nobs    <- nrow(ds)
nobs


## ----dimensions----------------------------------------------------------
dim(ds)
dim(ds[vars])
dim(ds[inputc])
dim(ds[inputi])


## ----identify_variable_types---------------------------------------------
numi       <- intersect(inputi, which(sapply(ds, is.numeric)))
numi
numc       <- names(ds)[numi]
numc
cati       <- intersect(inputi, which(sapply(ds, is.factor)))
cati
catc       <- names(ds)[cati]
catc


## ----eval=FALSE----------------------------------------------------------
## dsdate  <- paste0("_", format(Sys.Date(), "%y%m%d"))
## dsrdata <- paste0(dsname, dsdate, ".RData")
## save(ds, dsname, dspath, dsdate, target, risk, id, ignore, vars,
##      nobs, omit, inputi, inputc, numi, numc, cati, catc, file=dsrdata)


## ----echo=FALSE----------------------------------------------------------
# Do this so we know what to load into ModelsO.Rnw
dsdate  <- paste0("_", "130704")
dsrdata <- paste0(dsname, dsdate, ".RData")
save(ds, dsname, dspath, dsdate, target, risk, id, ignore, vars,
     nobs, omit, inputi, inputc, numi, numc, cati, catc, file=dsrdata)


## ------------------------------------------------------------------------
(load(dsrdata))
dsname
dspath
dim(ds)
id
target
risk
ignore
vars


## ----review_load, eval=FALSE---------------------------------------------
## # Required packages
## library(rattle)		# The weather dataset and normVarNames().
## library(randomForest)	# Impute missing values using na.roughfix().
## 
## # Identify the dataset.
## dsname      <- "weather"
## dspath      <- system.file("csv", "weather.csv", package="rattle")
## weather     <- read.csv(dspath)
## ds          <- get(dsname) %>% tbl_df()
## names(ds)   <- normVarNames(names(ds)) # Optional lower case variable names.
## vars        <- names(ds)
## target      <- "rain_tomorrow"
## risk        <- "risk_mm"
## id          <- c("date", "location")
## 
## # Summarise
## ds
## dim(ds)
## names(ds)
## str(ds)
## summary(ds)


## ----review_ignore, eval=FALSE-------------------------------------------
## # Ignore the IDs and the risk variable.
## ignore      <- union(id, if (exists("risk")) risk)
## 
## # Ignore variables that look like identifiers.
## ids         <- which(sapply(ds, function(x) length(unique(x))) == nrow(ds))
## ignore      <- union(ignore, names(ids))
## 
## # Ignore variables which are completely missing.
## mvc         <- sapply(ds[vars], function(x) sum(is.na(x))) # Missing value count.
## mvn         <- names(ds)[(which(mvc == nrow(ds)))]         # Missing var names.
## ignore      <- union(ignore, mvn)
## 
## # Ignore variables that are mostly missing - e.g., 70% or more missing
## mvn         <- names(ds)[(which(mvc >= 0.7*nrow(ds)))]
## ignore      <- union(ignore, mvn)
## 
## # Ignore variables with many levels.
## factors     <- which(sapply(ds[vars], is.factor))
## lvls        <- sapply(factors, function(x) length(levels(ds[[x]])))
## many        <- names(which(lvls > 20))	# Factors with too many levels.
## ignore      <- union(ignore, many)
## 
## # Ignore constants.
## constants   <- names(which(sapply(ds[vars], function(x) all(x == x[1L]))))
## ignore      <- union(ignore, constants)
## 
## # Initialise the variables
## vars        <- setdiff(vars, ignore)
## 


## ----review_finalise, eval=FALSE-----------------------------------------
## # Variable roles.
## inputc      <- setdiff(vars, target)
## inputi      <- sapply(inputc, function(x) which(x == names(ds)), USE.NAMES=FALSE)
## numi        <- intersect(inputi, which(sapply(ds, is.numeric)))
## numc        <- names(numi)
## cati        <- intersect(inputi, which(sapply(ds, is.factor)))
## catc        <- names(cati)
## 
## # Remove all observations with a missing target.
## ds          <- ds[!is.na(ds[target]),]
## 
## # Impute missing values, but do this wisely - understand why missing.
## if (sum(is.na(ds[vars]))) ds[vars] <- na.roughfix(ds[vars])
## 
## # Omit observations with missing values.
## omit        <- NULL
## mo          <- attr(na.omit(ds[vars]), "na.action")
## omit        <- union(omit, mo)
## if (length(omit)) ds <- ds[-omit,]	# Remove ommited observations.
## 
## # Normalise factors.
## factors     <- which(sapply(ds[vars], is.factor))
## for (f in factors) levels(ds[[f]]) <- normVarNames(levels(ds[[f]]))
## 
## # Ensure the target is categoric.
## ds[target]  <- as.factor(ds[[target]])
## 
## # Number of observations.
## nobs        <- nrow(ds)
## 
## # Save the dataset
## dsdate      <- paste0("_", format(Sys.Date(), "%y%m%d"))
## dsrdata     <- paste0(dsname, dsdate, ".RData")
## save(ds, dsname, dspath, dsdate, target, risk, id, ignore, vars,
##      nobs, omit, inputi, inputc, numi, numc, cati, catc, file=dsrdata)


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





