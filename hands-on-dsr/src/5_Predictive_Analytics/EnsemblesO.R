
## ----module, echo=FALSE, results="asis"----------------------------------
Module <- "EnsemblesO"
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
library(rattle)         # The weather dataset.
library(ada)            # Build boosted trees model with ada().
library(randomForest)   # Impute missing values with na.roughfix().
library(wsrpart)        # Weighted subspace using RPart.
library(wsrf)           # Weighted subspace implemented in Cpp.
library(party)          # Conditional random forest cforest().


## ----documentation, child='documentation.Rnw', eval=TRUE-----------------


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




## ----prepare_data_weather------------------------------------------------
set.seed(1426)
library(rattle)
data(weather)
dsname       <- "weather"
ds           <- get(dsname)
id           <- c("Date", "Location")
target       <- "RainTomorrow"
risk         <- "RISK_MM"
ignore       <- c(id, if (exists("risk")) risk) 
(vars        <- setdiff(names(ds), ignore))
inputs       <- setdiff(vars, target)
ds[vars]     <- na.roughfix(ds[vars]) # Impute missing values, roughly.
(nobs        <- nrow(ds))
(numerics    <- intersect(inputs, names(ds)[which(sapply(ds[vars], is.numeric))]))
(categorics  <- intersect(inputs, names(ds)[which(sapply(ds[vars], is.factor))]))
(form        <- formula(paste(target, "~ .")))
length(train <- sample(nobs, 0.7*nobs))
length(test  <- setdiff(seq_len(nobs), train))
actual       <- ds[test, target]


## ----check_dataset, out.lines=5------------------------------------------
dim(ds)
names(ds)
head(ds)
tail(ds)
str(ds)
summary(ds)


## ----rpart_model---------------------------------------------------------
model <- m.rp <- rpart(form, ds[train, vars])


## ----rp_print, out.lines=NULL--------------------------------------------
model


## ----rp_plot, message=FALSE, out.width="0.6\\textwidth"------------------
fancyRpartPlot(model)


## ----rp_performance, message=FALSE---------------------------------------
predicted <- predict(model, ds[test, vars], type="prob")[,2]
riskchart(predicted, actual)


## ----rf_model------------------------------------------------------------
model <- m.rf <- randomForest(form, ds[train, vars])


## ----rf_print, out.lines=NULL--------------------------------------------
model


## ------------------------------------------------------------------------
predicted <- predict(model, ds[test, vars])
sum(actual != predicted)/length(predicted) # Overall error rate
round(100*table(actual, predicted, dnn=c("Actual", "Predicted"))/length(predicted))


## ----rf_performance------------------------------------------------------
predicted <- predict(model, ds[test, vars], type="prob")[,2]
riskchart(predicted, actual)


## ------------------------------------------------------------------------
model <- m.cf <- cforest(form, ds[train, vars])
model


## ------------------------------------------------------------------------
model <- m.cf <- cforest(form, ds[train, vars], 
                         controls=cforest_control(ntree=500,
                             mtry=2,
                             replace=FALSE, 
                             teststat="quad", 
                             testtype = "Univ", 
                             mincriterion=0, 
                             fraction = 0.632,
                             minsplit=2,
                             minbucket=1))
model


## ----wsrpart_model-------------------------------------------------------
model <- m.wsrp <- wsrpart(form, ds[train, vars], ntrees=100)


## ----wsrpart_print, out.lines=15-----------------------------------------
model


## ----wsrpart_plot, message=FALSE, out.width="0.6\\textwidth"-------------
fancyRpartPlot(model[[1]]$model)


## ----wsrpart_performance-------------------------------------------------
predicted <- predict(model, ds[test, vars], type="prob")[,2]
riskchart(predicted, actual)


## ----wsrf_model----------------------------------------------------------
model <- m.wsrf <- wsrf(form, ds[train, vars], ntrees=500, nvars=20)


## ----wsrf_print, out.lines=40--------------------------------------------
model


## ----wsrf_performance----------------------------------------------------
predicted <- predict(model, ds[test, vars], type="prob")[,2]
riskchart(predicted, actual)


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





