
## ----module, echo=FALSE, results="asis"----------------------------------
Module <- "EvaluateO"
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
library(rattle)         # Weather, riskchart() and psfchart().
library(dplyr)          # Use tbl_df().
library(e1071)          # naiveBayes().
library(rpart)          # Decision tree model rpart().
library(randomForest)   # Impute missing na.roughfix(), and randonmForest().
library(wsrpart)        # Build weighted subspart rpart wsrpart().
library(wsrf)           # Build weighted subspace random forest wsrf().
library(gmodels)	# Generate cross-tabulation using CrossTable().
library(ROCR)		# Plot ROC curves.
library(ggplot2)        # Plot ROC curves.


## ----echo=FALSE, message=FALSE-------------------------------------------
library(Hmisc)
library(stringr)	# Used by rattle's normVarNames().


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




## ----prepare_data--------------------------------------------------------
# Identify the dataset
dsname     <- "weatherAUS"
ds         <- tbl_df(get(dsname))
names(ds)  <- normVarNames(names(ds)) # Lower case variable names.
vars       <- names(ds)
target     <- "rain_tomorrow"
risk       <- "risk_mm"
id         <- c("date", "location")

# Ignore the IDs and the risk variable.
ignore     <- c(id, if (exists("risk")) risk)

# Ignore variables which are completely missing.
mvc        <- sapply(ds[vars], function(x) sum(is.na(x)))
mvn        <- names(which(mvc == nrow(ds)))
ignore     <- union(ignore, mvn)

# Initialise the variables
vars       <- setdiff(vars, ignore)

# Variable roles.
inputs     <- setdiff(vars, target)
numi       <- which(sapply(ds[inputs], is.numeric))
numc       <- names(numi)
cati       <- which(sapply(ds[inputs], is.factor))
catc       <- names(cati)

# Remove all observations with a missing target.
ds         <- ds[!is.na(ds[target]),]

# Impute missing values needed for randomForest().
if (sum(is.na(ds[vars]))) ds[vars] <- na.roughfix(ds[vars])

# Ensure the target is categoric.
ds[target] <- as.factor(ds[[target]])

# Number of observations.
nobs       <- nrow(ds)

# Prepare for model building.
form       <- formula(paste(target, "~ ."))
seed       <- 328058
train      <- sample(nobs, 0.7*nobs)
test       <- setdiff(seq_len(nobs), train)
actual     <- ds[test, target]
risks      <- ds[test, risk]


## ----build_models--------------------------------------------------------
# Naive Bayes
library(e1071)
model      <- m.nb <- naiveBayes(form, ds[train, vars])      #  1s
cl.nb      <- predict(model, ds[test, vars], type="class")   # 20s
pr.nb      <- predict(model, ds[test, vars], type="raw")[,2] # 20s

# Decision tree
library(rpart)
model      <- m.rp <- rpart(form, ds[train, vars])           # 6s
cl.rp      <- predict(model, ds[test, vars], type="class")
pr.rp      <- predict(model, ds[test, vars], type="prob")[,2]

# Random forest
library(randomForest)
model      <- m.rf <- randomForest(form, ds[train, vars], ntree=100) # 20s
cl.rf      <- predict(model, ds[test, vars], type="class")
pr.rf      <- predict(model, ds[test, vars], type="prob")[,2]

# Weighted subspace rpart
library(wsrpart)
model      <- m.wsrp <- wsrpart(form, ds[train, vars], ntree=10) # 30s
cl.wsrp    <- predict(model, ds[test, vars], type="class")
pr.wsrp    <- predict(model, ds[test, vars], type="prob")[,2]

# Weighted subspace random forest
library(wsrf)
model      <- m.wsrf <- wsrf(form, ds[train, vars], ntree=10) # 30s
cl.wsrf    <- predict(model, ds[test, vars], type="class")
pr.wsrf    <- predict(model, ds[test, vars], type="prob")[,2]


## ----table_confusion_matrix_raw, out.lines=NULL--------------------------
table(actual, cl.nb, dnn=c("Actual", "Predicted"))
table(actual, cl.rp, dnn=c("Actual", "Predicted"))
table(actual, cl.rf, dnn=c("Actual", "Predicted"))
table(actual, cl.wsrp, dnn=c("Actual", "Predicted"))
table(actual, cl.wsrf, dnn=c("Actual", "Predicted"))


## ----table_confusion_matrix, out.lines=NULL------------------------------
pcme <- function(actual, cl) 
{
  x <- table(actual, cl)
  tbl <- cbind(round(x/length(actual), 2), 
               Error=round(c(x[1,2]/sum(x[1,]), x[2,1]/sum(x[2,])), 2))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  tbl
}
pcme(actual, cl.nb)
pcme(actual, cl.rp)
pcme(actual, cl.rf)
pcme(actual, cl.wsrp)
pcme(actual, cl.wsrf)


## ------------------------------------------------------------------------
overall <- function(x) round((x[1,2] + x[2,1]) / sum(x), 2)
overall(table(actual, cl.nb)/length(actual))
overall(table(actual, cl.rp)/length(actual))
overall(table(actual, cl.rf)/length(actual))
overall(table(actual, cl.wsrp)/length(actual))
overall(table(actual, cl.wsrf)/length(actual))


## ------------------------------------------------------------------------
avgerr <- function(x) round(mean(c(x[1,2], x[2,1]) / apply(x, 1, sum)), 2)
avgerr(table(actual, cl.nb)/length(actual))
avgerr(table(actual, cl.rp)/length(actual))
avgerr(table(actual, cl.rf)/length(actual))
avgerr(table(actual, cl.wsrp)/length(actual))
avgerr(table(actual, cl.wsrf)/length(actual))


## ----out.lines=NULL------------------------------------------------------
library(gmodels)
CrossTable(actual, cl.nb)


## ----out.lines=NULL------------------------------------------------------
CrossTable(actual, cl.rf)


## ----roc_curve, message=FALSE--------------------------------------------
library(ROCR)
pr <- prediction(pr.rf, actual)
pe <- performance(pr, "tpr", "fpr")
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))

p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p


## ----fig.width=11, out.width="\\textwidth"-------------------------------
library(ROCR)
library(gridExtra)

proc <- function(pr.m)
{
  pr <- prediction(pr.m, actual)
  pe <- performance(pr, "tpr", "fpr")
  au <- performance(pr, "auc")@y.values[[1]]
  pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
  
  p <- ggplot(pd, aes(x=fpr, y=tpr))
  p <- p + geom_line(colour="red")
  p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
  p <- p + ggtitle(deparse(substitute(pr.m)))
  p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                    label=paste("AUC =", round(au, 2)))
  return(p)
}
grid.arrange(proc(pr.nb), proc(pr.rp), proc(pr.rf), 
             proc(pr.wsrp), proc(pr.wsrf), ncol=3)


## ----risk_chart_rf, echo=FALSE, message=FALSE----------------------------
riskchart(pr.rf, actual, risks, "Random Forest")


## ----risk_chart_rf, eval=FALSE-------------------------------------------
## riskchart(pr.rf, actual, risks, "Random Forest")


## ----risk_chart_nb, echo=FALSE, message=FALSE----------------------------
riskchart(pr.nb, actual, risks, "Naive Bayes")


## ----risk_chart_rf, eval=FALSE-------------------------------------------
## riskchart(pr.rf, actual, risks, "Random Forest")


## ----eval=FALSE, echo=FALSE, out.height='0.2\\textheight'----------------
## library(gridExtra)
## rc.rf <- riskchart(pr.rf, actual, risks, "randomForest")
## rc.wsrf <- riskchart(pr.wsrf, actual, risks, "wsrf")
## grid.arrange(rc.rf, rc.wsrf, ncol=2)


## ----fig.width=10--------------------------------------------------------
print(riskchart(pr.rf, actual, risks, "Random Forest"))


## ----rc_all_models, fig.width=10, out.width="\\textwidth"----------------
library(gridExtra)
rc.nb <- riskchart(pr.nb, actual, risks, "naive bayes")
rc.rp <- riskchart(pr.rp, actual, risks, "rpart")
rc.rf <- riskchart(pr.rf, actual, risks, "randomForest")
rc.wsrp <- riskchart(pr.wsrp, actual, risks, "wsrpart")
rc.wsrf <- riskchart(pr.wsrf, actual, risks, "wsrf")
grid.arrange(rc.nb, rc.rp, rc.rf, rc.wsrp, rc.wsrf, ncol=3)


## ----fig.width=13--------------------------------------------------------
pi <- which(pr.rf > 0.5)
riskchart(pr.rf[pi], actual[pi], risks[pi], "randomForest - Evaluation")


## ----fig.width=13--------------------------------------------------------
actual0 <- actual; risks0 <- risks; actual0[-pi] <- "No"; risks0[-pi] <- 0
riskchart(pr.rf, actual0, risks0, "randomForest - Evaluation with Dummies")


## ----psfchart, message=FALSE---------------------------------------------
print(psfchart(pr.rf, actual))


## ----fig.width=10, fig.height=10-----------------------------------------
psf.rp   <- psfchart(pr.rp, actual, bins=10)
psf.rf   <- psfchart(pr.rf, actual)
psf.wsrp <- psfchart(pr.wsrp, actual)
psf.wsrf <- psfchart(pr.wsrf, actual)
grid.arrange(psf.rp, psf.rf, psf.wsrp, psf.wsrf)


## ----fig.width=10, fig.height=10-----------------------------------------
psf.rf1   <- psfchart(pr.rf, actual, bins=10)
psf.rf2   <- psfchart(pr.rf, actual, bins=50)
psf.wsrf1 <- psfchart(pr.wsrf, actual, bins=5, threshold=0.3)
psf.wsrf2 <- psfchart(pr.wsrf, actual, bins=100, threshold=0.3)
grid.arrange(psf.rf1, psf.rf2, psf.wsrf1, psf.wsrf2)


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





