
## ----module, echo=FALSE, results="asis"----------------------------------
Module <- "ModelsO"
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
library(rpart)          # Build decision tree model with rpart().
library(randomForest)   # Build model with randomForest().
library(ada)            # Build boosted trees model with ada().
library(rattle)         # Display tree model with fancyRpartPlot().
library(ROCR)           # Use prediction() for evaluation.
library(party)          # Build conditional tree models with ctree() and cforest().
library(ggplot2)        # Display evaluations.


## ----echo=FALSE, message=FALSE-------------------------------------------
library(Hmisc)


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





## ----out.lines=NULL------------------------------------------------------
(load("weather_130704.RData"))
dsname
dspath
dsdate
dim(ds)
id
target
risk
ignore
vars


## ----construct_the_formula-----------------------------------------------
(form <- formula(paste(target, "~ .")))


## ----set_seed------------------------------------------------------------
(seed <- sample(1:1000000, 1))
seed <- 123
set.seed(seed)


## ----build_training_testing_datasets-------------------------------------
length(train <- sample(nobs, 0.7*nobs))
length(test  <- setdiff(seq_len(nobs), train))


## ------------------------------------------------------------------------
actual.train <- ds[train, target]
actual       <- ds[test, target]
risks        <- ds[test, risk]


## ----build_model, out.lines=NULL-----------------------------------------
ctrl <- rpart.control(maxdepth=3)
system.time(model <- m.rp <- rpart(form, ds[train, vars], control=ctrl))
model
mtype <- "rpart" # Record the type of the model for later use.


## ----echo=FALSE, message=FALSE-------------------------------------------
# Do this to avoid the messages taking up space on the
# page so we can fit the plot.
library(rpart.plot)
library(RColorBrewer)


## ----plot_model, out.width='0.5\\textwidth'------------------------------
fancyRpartPlot(model)


## ----evaluate_train_accuracy---------------------------------------------
head(cl <- predict(model, ds[train, vars], type="class"))


## ------------------------------------------------------------------------
head(actual.train)


## ------------------------------------------------------------------------
(acc <- sum(cl == actual.train, na.rm=TRUE)/length(actual.train))


## ------------------------------------------------------------------------
(err <- sum(cl != actual.train, na.rm=TRUE)/length(actual.train))


## ----evaluate_train_auc--------------------------------------------------
pr <- predict(model, ds[train, vars], type="prob")[,2]
pred <- prediction(pr, ds[train, target])
(atr <- attr(performance(pred, "auc"), "y.values")[[1]])


## ----evaluate_test_accuracy----------------------------------------------
cl <- predict(model, ds[test, vars], type="class")
(acc <- sum(cl == actual, na.rm=TRUE)/length(actual))


## ------------------------------------------------------------------------
(err <- sum(cl != actual, na.rm=TRUE)/length(actual))


## ----evaluate_test_auc---------------------------------------------------
pr <- predict(model, ds[test, vars], type="prob")[,2]
pred <- prediction(pr, ds[test, target])
(ate <- attr(performance(pred, "auc"), "y.values")[[1]])


## ------------------------------------------------------------------------
cl <- predict(model, ds[train, vars], type="class")
round(100*table(actual.train, cl, dnn=c("Actual", "Predicted"))/length(actual.train))


## ------------------------------------------------------------------------
cl <- predict(model, ds[test, vars], type="class")
round(100*table(actual, cl, dnn=c("Actual", "Predicted"))/length(actual))


## ----riskchart, message=FALSE--------------------------------------------
riskchart(pr, ds[test, target], ds[test, risk])


## ----experimental_results_child, child='experi.Rnw', eval=TRUE-----------

## ----echo=FALSE, eval=FALSE----------------------------------------------
## #library(rattle)
## #form <- RainTomorrow ~ .
## #ds <- weather
## #target <- "RainTomorrow"
## #modeller <- "ada"
## #details <- "50"
## #n <- 10
## #control <- list(rpart.control(maxdepth=10, # Not Working
## #                              cp=0.010000,
## #                              minsplit=20,
## #                              xval=10),
## #                na.action=na.omit)
## #keep.pr <- TRUE
## #prob <- "prob"
## #class <- "class"


## ----src.top=25, src.bot=14----------------------------------------------
experi <- function(form, ds, dsname, target, modeller, details="", 
                   n=100, control=NULL,
                   keep=FALSE, # Keep the last model built.
                   prob="prob",
                   class="class",
                   log="experi.log")
{
  suppressPackageStartupMessages(require(pROC))

  user <- Sys.getenv("LOGNAME")
  node <- Sys.info()[["nodename"]]

  wsrpart.model <- modeller=="wsrpart"
  
  numclass <- length(levels(ds[,target]))

  start.time <- proc.time()
  
  seeds <- cors <- strs <- aucs <- accs <- NULL
  for (i in seq_len(n))
  {
    loop.time <- proc.time()

    seeds  <- c(seeds, seed <- sample(1:1000000, 1))
    set.seed(seed)
    
    train  <- sample(nrow(ds), 0.7*nrow(ds))
    test   <- setdiff(1:nrow(ds), train)
    actual <- ds[test, target]
    
    args   <- append(list(form, data=ds[train,]), control)
    model  <- do.call(modeller, args)
    
    if (numclass==2)
    {
      if (modeller %in% c("ctree", "cforest"))
        pr <- do.call("rbind", predict(model, newdata=ds[test,], type=prob))[,2]
      else
        pr <- predict(model, newdata=ds[test,], type=prob)[,2]
      # For small samples we can get all the same class...
      # Should really ensure we sample both classes
      if (length(unique(actual)) == 1)
        aucs <- c(0.0, aucs)
      else
        aucs <- c(auc(actual, pr), aucs)
    }
    if ("ada" %in% class(model)) class <- "vector"
    if (modeller %in% c("ctree", "cforest")) class <- "response"

    #compute cirrelation and strength for mrpart
    if (wsrpart.model)
    {
      cors <- c(correlation(model, ds, form), cors)
      strs <- c(strength(model, ds, form), strs)
    }
    
    cl <- predict(model, newdata=ds[test,], type=class)
    accs <- c(sum(cl==actual, na.rm=TRUE)/length(actual), accs)

    if (! is.null(log))
    {
      require(lubridate)
      if (! file.exists(log))
        write.table(data.frame(user=NA, node=NA, ts=NA, ds=NA, model=NA,
                               acc=NA, auc=NA, cor=NA, str=NA,
                               user=NA, elapsed=NA),
                    file=log, sep=",", row.names=FALSE)
      write.table(data.frame(user=user, node=node, ts=now(),
                           ds=dsname, model=modeller,
                           acc=round(accs[1], 4),
                           auc=round(aucs[1], 4),
                           cor=ifelse(wsrpart.model, round(cors[1], 4), NA),
                           str=ifelse(wsrpart.model, round(strs[1], 4), NA),
                           user=round((proc.time()-loop.time)['user.self'], 2),
                           elapsed=round((proc.time()-loop.time)['elapsed'],2)),
                file=log, sep=",", append=TRUE, col.names=FALSE, row.names=FALSE)
    }
  }
  
  result <- data.frame(modeller=paste0(modeller, "_", details),
                       auc=ifelse(numclass==2, mean(aucs), NA),
                       auc.sd=ifelse(numclass==2, sd(aucs), NA),
                       cor=ifelse(wsrpart.model, mean(cors), NA),
                       cor.sd=ifelse(wsrpart.model , sd(cors), NA),
                       str=ifelse(wsrpart.model, mean(strs), NA),
                       str.sd=ifelse(wsrpart.model , sd(strs), NA),
                       acc=mean(accs), acc.sd=sd(accs), n=n,
                       user=(proc.time()-start.time)['user.self'],
                       elapsed=(proc.time()-start.time)['elapsed'])
  if (wsrpart.model)
    if (numclass==2)
      result[-1]   <- round(result[-1], 2)
  else
    result[-c(1:3)]   <- round(result[-c(1:3)], 2)
  else
    if (numclass==2)
      result[-c(1,4:7)]   <- round(result[-c(1,4:7)], 2)
  else
    result[-c(1:7)]   <- round(result[-c(1:7)], 2)
  
  row.names(result) <- NULL

  if (keep)
  {
    if (numclass==2) 
    {
      attr(result, "pr") <- pr
      attr(result, "test") <- test
    }
    attr(result, "model") <- model
  }

  return(result)
}



## ----eval=FALSE----------------------------------------------------------
## source("http://onepager.togaware.com/experi.R")


## ----run_experiments-----------------------------------------------------
n <- 10


## ----run_rp, message=FALSE-----------------------------------------------
ex.rp <- experi(form, ds[vars], dsname, target, "rpart", "1", n=n, keep=TRUE)


## ----run_rf--------------------------------------------------------------
ex.rf <- experi(form, ds[vars], dsname, target, "randomForest", "500", n=n, keep=TRUE,
                control=list(na.action=na.omit))


## ----run_ad--------------------------------------------------------------
ex.ad <- experi(form, ds[vars], dsname, target, "ada", "50", n=n, keep=TRUE)


## ----run_ct--------------------------------------------------------------
ex.ct <- experi(form, ds[vars], dsname, target, "ctree", "1", n=n, keep=TRUE)


## ----run_cf, eval=FALSE--------------------------------------------------
## # Generates: error code 1 from Lapack routine 'dgesdd'
## ex.cf <- experi(form, ds[vars], dsname, target, "cforest", "500", n=n, keep=TRUE)


## ----show_results, out.lines=NULL----------------------------------------
results <- rbind(ex.rp, ex.rf, ex.ad, ex.ct)
rownames(results) <- results$modeller
results$modeller <- NULL
results


## ----experi_riskchart_rp-------------------------------------------------
ex <- ex.rp
pr <- attr(ex, "pr")
test <- attr(ex, "test")
riskchart(pr, ds[test, target], ds[test, risk])


## ----experi_riskchart_rf-------------------------------------------------
ex <- ex.rf
pr <- attr(ex, "pr")
test <- attr(ex, "test")
riskchart(pr, ds[test, target], ds[test, risk])


## ----experi_riskchart_ad-------------------------------------------------
ex <- ex.ad
pr <- attr(ex, "pr")
test <- attr(ex, "test")
riskchart(pr, ds[test, target], ds[test, risk])


## ----experi_riskchart_ct-------------------------------------------------
ex <- ex.ct
pr <- attr(ex, "pr")
test <- attr(ex, "test")
riskchart(pr, ds[test, target], ds[test, risk])


## ----save_model----------------------------------------------------------
dname <- "models"
if (! file.exists(dname)) dir.create(dname)
time.stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
fstem <- paste(dsname, mtype, time.stamp, sep="_")
(fname <- file.path(dname, sprintf("%s.RData", fstem)))
save(ds, dsname, vars, target, risk, ignore,
     form, nobs, seed, train, test, model, mtype, pr,
     file=fname)


## ----load_model----------------------------------------------------------
(load(fname))


## ----build_model_rf, out.lines=NULL--------------------------------------
ctrl <- rpart.control(maxdepth=3)
system.time(model <- m.rf <- rpart(form, ds[train, vars], control=ctrl))
model


## ----eval=FALSE----------------------------------------------------------
## # Required packages
## library(rpart)   # Model builder
## library(rattle)  # riskchart()
## library(ROCR)    # prediction()
## 
## # Load dataset.
## load("weather_130704.RData")
## 
## # Formula for modelling.
## form       <- formula(paste(target, "~ ."))
## 
## # Training and test datasets.
## seed       <- sample(1:1000000, 1)
## set.seed(seed)
## train      <- sample(nobs, 0.7*nobs)
## test       <- setdiff(seq_len(nobs), train)
## actual     <- ds[test, target]
## risks      <- ds[test, risk]
## 
## # Build model.
## ctrl       <- rpart.control(maxdepth=3)
## m.rp       <- rpart(form, data=ds[train, vars], control=ctrl)
## mtype      <- "rpart"
## model      <- m.rp
## 
## # Review model.
## fancyRpartPlot(model)
## 
## # Evaluate the model.
## classes    <- predict(model, ds[test, vars], type="class")
## acc        <- sum(classes == actual, na.rm=TRUE)/length(actual)
## err        <- sum(classes != actual, na.rm=TRUE)/length(actual)
## predicted  <- predict(model, ds[test, vars], type="prob")[,2]
## pred       <- prediction(predicted, ds[test, target])
## ate        <- attr(performance(pred, "auc"), "y.values")[[1]]
## riskchart(predicted, actual, risks)
## psfchart(predicted, actual)
## round(table(actual, classes, dnn=c("Actual", "Predicted"))/length(actual), 2)


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





