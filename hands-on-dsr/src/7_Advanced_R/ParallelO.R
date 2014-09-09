
## ----module, echo=FALSE, results="asis"----------------------------------
Module <- "GeneticProgrammingO"
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
library(parallel)
library(rpart)


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


## ----list_csv_files------------------------------------------------------
dir(path="data", pattern="*.csv")


## ----read_csv_weather----------------------------------------------------
ds <- read.csv(file="data/weatherAUS.csv")


## ----check_weather_dataset, out.lines=5----------------------------------
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
nobs    <- nrow(ds)
dim(ds[vars])


## ----construct_the_formula-----------------------------------------------
(form <- formula(paste(target, "~ .")))


## ----set_seed------------------------------------------------------------
(seed <- sample(1:1000000, 1))
set.seed(seed)


## ----build_training_testing_datasets-------------------------------------
length(train <- sample(nobs, 0.7*nobs))
length(test  <- setdiff(seq_len(nobs), train))


## ----example_call_to_wsrpart---------------------------------------------
set.seed(42)
system.time(model <- wsrpart(form, ds[train, vars], ntrees=1))
model[[1]]$model
model[[1]]$vars
model[[1]]$accuracy


## ----example_call_to_wsrpart_repeat--------------------------------------
set.seed(84)
system.time(model <- wsrpart(form, ds[train, vars], ntrees=1))
model[[1]]$model
model[[1]]$vars
model[[1]]$oob.error


## ----detect_number_of_cores----------------------------------------------
cores <- detectCores()
cores


## ----run_mcparallel------------------------------------------------------
jobs <- lapply(1:cores, 
               function(x) mcparallel(wsrpart(form, ds[train,vars], ntrees=1), 
                                      name=sprintf("dt%02d", x)))


## ----inspect_processes, out.lines=NULL-----------------------------------
jobs[1:2]


## ----wait_finish---------------------------------------------------------
system.time(model <- mccollect(jobs, wait=TRUE))


## ----access_forest, out.lines=10-----------------------------------------
length(model)
model[[1]][[1]]$model
model[[2]][[1]]$model


## ----exercise_mwsrpart_cores_examples------------------------------------
system.time(model <- wsrpart(form, ds, ntrees=4, parallel=2))
num.trees <- cores
set.seed(42)
system.time(model <- wsrpart(form, ds, ntrees=num.trees, parallel=2))
model[[1]]$model


## ----exercise_mcwsrpart_timings------------------------------------------
set.seed(42)
system.time(model <- wsrpart(form, ds, ntrees=num.trees, parallel=2))
set.seed(42)
system.time(model <- wsrpart(form, ds, ntrees=num.trees, parallel=2))


## ----make_local_cluster--------------------------------------------------
cl <- makeCluster(rep("localhost", cores))
cl


## ----local_cluster_add_3-------------------------------------------------
clusterApply(cl, 1:2, get("+"), 3)


## ----local_cluster_evalq_getwd-------------------------------------------
clusterEvalQ(cl, getwd())


## ----local_cluster_close-------------------------------------------------
stopCluster(cl)


## ----make_local_cluster_rpart--------------------------------------------
cl <- makeCluster(rep("localhost", cores))
cl


## ----local_cluster_evalq_library-----------------------------------------
clusterEvalQ(cl, {library(parallel); library(rpart); library(rattle)})


## ----local_cluster_load_dataset------------------------------------------
clusterExport(cl, c("ds", "form", "train", "vars"))


## ----local_cluster_build_rpart_model-------------------------------------
clusterExport(cl, c("varWeights", "selectVars", "wsrpart"))
system.time(model <- clusterCall(cl, wsrpart, form, ds[train, vars], ntrees=4))
length(model)


## ----local_cluster_close_rpart-------------------------------------------
stopCluster(cl)


## ----make_cluster_rpart_ms, eval=FALSE-----------------------------------
## nodes <- paste("node", 1:10, sep="")
## cl <- makeCluster(nodes)
## cl


## ----multiple_cluster_build_forest_ms, eval=FALSE------------------------
## clusterEvalQ(cl, {library(parallel); library(rpart); library(rattle)})
## clusterExport(cl,  c("varWeights", "selectVars", "wsrpart"))
## clusterExport(cl, c("varWeights", "selectVars", "wsrpart"))
## system.time(model <- clusterCall(cl, wsrpart, form, ds[train, vars], ntrees=4))


## ----multiple_cluster_close_ms, eval=FALSE-------------------------------
## stopCluster(cl)


## ----make_cluster_rpart_msmc, eval=FALSE---------------------------------
## nodes <- paste("node", 1:10, sep="")
## cl <- makeCluster(nodes)
## cl


## ----multiple_cluster_build_forest_msmc, eval=FALSE----------------------
## clusterEvalQ(cl, {library(rpart); library(rattle)})
## clusterExport(cl, "weatherDS")
## clusterExport(cl, c("varWeights", "selectVars", "wsrpart", "mcwsrpart"))
## system.time(forest <- clusterCall(cl, mcwsrpart, weatherDS, 8))


## ----multiple_cluster_close_msmc, eval=FALSE-----------------------------
## stopCluster(cl)


## ----eval=FALSE----------------------------------------------------------
## nodes <- paste("node", 2:10, sep="")
## cl <- makeCluster(nodes)
## clusterEvalQ(cl,
##              install.packages("rattle",
##                               lib="/usr/local/lib/R/site-library",
##                               repos="http://rattle.togaware.com"))
## stopCluster(cl)


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






