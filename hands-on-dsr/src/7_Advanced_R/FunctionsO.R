
## ----module, echo=FALSE, results="asis"----------------------------------
Module <- "Functions"
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
library(rattle)


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




## ----load_wsrpart, echo=FALSE, message=FALSE-----------------------------
# And load this package for the exercises.
library(wsrpart)


## ----sample_function-----------------------------------------------------
mult10 <- function(x) 
{
  if (is.character(x))
  {
    result <- apply(sapply(x, rep, 10), 2, paste, collapse="")
    names(result) <- NULL
  }
  else
  {
    result <- x * 10
  }
  
  return(result)
}


## ----plus_is_a_function--------------------------------------------------
4 + 5
"+"(4, 5)


## ----multiple_apply_function---------------------------------------------
1 + 2 + 3 + 4 + 5
Reduce("+", 1:5)


## ----eval_a_function_as_string-------------------------------------------
cmd <- "1 + 2 + 3 + 4 + 5"
eval(parse(text=cmd))


## ----nested_for_loops----------------------------------------------------
for (i in 0:4)
  for (j in 5:9)
    print(paste0(i, j))


## ----load_weatherAUS-----------------------------------------------------
ds <- read.csv(file="data/weatherAUS.csv")


## ----observe_the_dataset, out.lines=7------------------------------------
dim(ds)
head(ds)
tail(ds)
str(ds)
summary(ds)


## ------------------------------------------------------------------------
target <- "RainTomorrow"
risk   <- "RISK_MM"
dsname <- "weather"


## ----ensure_target_is_categoric------------------------------------------
ds[target] <- as.factor(ds[[target]])
summary(ds[target])


## ----identify_variables--------------------------------------------------
vars    <- colnames(ds)
ignore  <- vars[c(1, 2, if (exists("risk")) which(risk==vars))]
vars    <- setdiff(vars, ignore)
(inputs <- setdiff(vars, target))
nobs   <- nrow(ds)
dim(ds[vars])


## ----construct_the_formula-----------------------------------------------
(form <- formula(paste(target, "~ .")))


## ----set_seed------------------------------------------------------------
set.seed(142)


## ----build_training_testing_datasets-------------------------------------
length(train <- sample(nobs, 0.7*nobs))
length(test  <- setdiff(seq_len(nobs), train))


## ----exercise_varweights_demo, eval=FALSE--------------------------------
## varw <- varWeights(form, ds)


## ----exercise_varweihts_sample_correlation_functions---------------------
n1 <- ds[["Temp3pm"]]
c1 <- ds[["WindGustDir"]]
t1 <- ds[[target]]

cor(as.numeric(n1), as.numeric(t1), use="pairwise.complete.obs")
cor(as.numeric(c1), as.numeric(t1), use="pairwise.complete.obs")


## ----exercise_varweights_template, eval=FALSE----------------------------
## varWeights <- function(formula, data)
## {
##   ...
## }


## ----exercise_varweights_output------------------------------------------
varWeights(form, ds)


## ----exercise_selectvars, eval=FALSE-------------------------------------
## vars <- selectVars(form, ds, 3)


## ----exercise_selectvars_template, eval=FALSE----------------------------
## selectVars <- function(formula, data, n)
## {
##   ...
## }


## ----exercise_selectvars_output------------------------------------------
selectVars(form, ds, 3)
selectVars(form, ds, 3)
selectVars(form, ds, 3)
selectVars(form, ds, 3)


## ----exercise_wsrpart_demo, eval=FALSE-----------------------------------
## dt <- wsrpart(form, data)


## ----exercise_wsrpart_template, eval=FALSE-------------------------------
## wsrpart <- function(formula, data, nvars, ...)
## {
##   ...
## }


## ----set_random_seed, echo=FALSE-----------------------------------------
set.seed(142)


## ----exercise_wsrpart_example, eval=c(2,3), echo=c(1,3)------------------
system.time(model <- wsrpart(form, ds[train, vars]))
system.time(model <- wsrpart(form, ds[train, vars], ntrees=1))
model


## ----exercise_wsrpart_ntrees_example, out.lines=20-----------------------
system.time(model <- wsrpart(form, ds[train, vars], 4))
class(model)
length(model)
class(model[[1]]$model)
model[[1]]$model


## ----exercise_predict_mrpart_examples------------------------------------
predict(model, ds[test,vars])


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






