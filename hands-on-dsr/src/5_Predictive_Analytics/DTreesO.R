
## ----module, echo=FALSE, results="asis"----------------------------------
Module <- "DTreesO"
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
library(rattle)         # GUI for building trees and fancy tree plot
library(rpart)          # Popular decision tree algorithm
library(rpart.plot)     # Enhanced tree plots
library(party)          # Alternative decision tree algorithm
library(partykit)       # Convert rpart object to BinaryTree
library(RWeka)          # Weka decision tree J48.
library(C50)            # Original C5.0 implementation.


## ----echo=FALSE----------------------------------------------------------
library(RColorBrewer)


## ----documentation, child="documentation.Rnw", eval=TRUE-----------------


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




## ----start_rattle, eval=FALSE--------------------------------------------
## library(rattle)
## rattle()


## ----rattle_commands, eval=FALSE-----------------------------------------
## set.seed(42)
## library(rattle)
## library(rpart)
## ds     <- weather
## target <- "RainTomorrow"
## nobs   <- nrow(ds)
## form   <- formula(paste(target, "~ ."))
## train  <- sample(nobs, 0.70 * nobs)
## vars   <- -c(1,2,23)
## model  <- rpart(form, ds[train, vars], parms=list(split="information"))


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
(nobs        <- nrow(ds))
(numerics    <- intersect(inputs, names(ds)[which(sapply(ds[vars], is.numeric))]))
(categorics  <- intersect(inputs, names(ds)[which(sapply(ds[vars], is.factor))]))
(form        <- formula(paste(target, "~ .")))
length(train <- sample(nobs, 0.7*nobs))
length(test  <- setdiff(seq_len(nobs), train))
actual       <- ds[test, target]
risks        <- ds[test, risk]


## ----check_dataset, out.lines=5------------------------------------------
dim(ds)
names(ds)
head(ds)
tail(ds)
str(ds)
summary(ds)


## ----dt_model------------------------------------------------------------
model <- rpart(formula=form, data=ds[train, vars])


## ----dt_model_no_argument_names------------------------------------------
str(rpart)
model <- rpart(form, ds[train, vars])


## ----display_model, out.lines=NULL---------------------------------------
model


## ----dt_model_summary, out.lines=40--------------------------------------
summary(model)


## ----dt_model_printcp, out.lines=NULL------------------------------------
printcp(model)


## ----dt_model_plotcp,----------------------------------------------------
plotcp(model)


## ----tmodel_weatherAUS_for_cp--------------------------------------------
tmodel <- rpart(form, weatherAUS[vars])
plotcp(tmodel)


## ----tmodel_weatherAUS_cp_0----------------------------------------------
tmodel <- rpart(form, weatherAUS[vars], control=rpart.control(cp=0))
plotcp(tmodel)


## ----out.lines=NULL------------------------------------------------------
tmodel$cptable[c(1:5,22:29, 80:83),]


## ------------------------------------------------------------------------
model$variable.importance


## ----message=FALSE-------------------------------------------------------
predicted <- predict(model, ds[test, vars], type="prob")[,2]
riskchart(predicted, actual, risks)


## ------------------------------------------------------------------------
predicted <- predict(model, ds[test, vars], type="class")
sum(actual != predicted)/length(predicted) # Overall error rate
round(100*table(actual, predicted, dnn=c("Actual", "Predicted"))/length(predicted))


## ------------------------------------------------------------------------
asRules.rpart <- function(model)
{
  if (!inherits(model, "rpart")) stop("Not a legitimate rpart tree")
  #
  # Get some information.
  #
  frm     <- model$frame
  names   <- row.names(frm)
  ylevels <- attr(model, "ylevels")
  ds.size <- model$frame[1,]$n
  #
  # Print each leaf node as a rule.
  #
  for (i in 1:nrow(frm))
  {
    if (frm[i,1] == "<leaf>")
    {
      # The following [,5] is hardwired - needs work!
      cat("\n")
      cat(sprintf(" Rule number: %s ", names[i]))
      cat(sprintf("[yval=%s cover=%d (%.0f%%) prob=%0.2f]\n",
                  ylevels[frm[i,]$yval], frm[i,]$n,
                  round(100*frm[i,]$n/ds.size), frm[i,]$yval2[,5]))
      pth <- path.rpart(model, nodes=as.numeric(names[i]), print.it=FALSE)
      cat(sprintf("   %s\n", unlist(pth)[-1]), sep="")
    }
  }
}


## ----out.lines=14--------------------------------------------------------
asRules(model)


## ----basic_plot, echo=FALSE----------------------------------------------
plot(model)
text(model)


## ----basic_plot, eval=FALSE----------------------------------------------
## plot(model)
## text(model)


## ----basic_plot_uniform, echo=FALSE--------------------------------------
plot(model, uniform=TRUE)
text(model)


## ----basic_plot_uniform, eval=FALSE--------------------------------------
## plot(model, uniform=TRUE)
## text(model)


## ----basic_plot_extra, echo=FALSE----------------------------------------
plot(model, uniform=TRUE)
text(model, use.n=TRUE, all=TRUE, cex=.8)


## ----basic_plot_extra, eval=FALSE----------------------------------------
## plot(model, uniform=TRUE)
## text(model, use.n=TRUE, all=TRUE, cex=.8)


## ----fancy_plot, message=FALSE, echo=FALSE-------------------------------
fancyRpartPlot(model)


## ----fancy_plot, message=FALSE, eval=FALSE-------------------------------
## fancyRpartPlot(model)


## ----prp_default, echo=FALSE---------------------------------------------
prp(model)


## ----prp_default, eval=FALSE---------------------------------------------
## prp(model)


## ----prp_fav, echo=FALSE-------------------------------------------------
prp(model, type=2, extra=104, nn=TRUE, fallen.leaves=TRUE, 
    faclen=0, varlen=0, shadow.col="grey", branch.lty=3)


## ----prp_fav, eval=FALSE-------------------------------------------------
## prp(model, type=2, extra=104, nn=TRUE, fallen.leaves=TRUE,
##     faclen=0, varlen=0, shadow.col="grey", branch.lty=3)


## ----prp_colour, echo=FALSE----------------------------------------------
col <- c("#FD8D3C", "#FD8D3C", "#FD8D3C", "#BCBDDC",
         "#FDD0A2", "#FD8D3C", "#BCBDDC")
prp(model, type=2, extra=104, nn=TRUE, fallen.leaves=TRUE, 
    faclen=0, varlen=0, shadow.col="grey", branch.lty=3, box.col=col)


## ----prp_colour, eval=FALSE----------------------------------------------
## col <- c("#FD8D3C", "#FD8D3C", "#FD8D3C", "#BCBDDC",
##          "#FDD0A2", "#FD8D3C", "#BCBDDC")
## prp(model, type=2, extra=104, nn=TRUE, fallen.leaves=TRUE,
##     faclen=0, varlen=0, shadow.col="grey", branch.lty=3, box.col=col)


## ----prp_label_nodes, echo=FALSE-----------------------------------------
prp(model, type=1)


## ----prp_label_nodes, eval=FALSE-----------------------------------------
## prp(model, type=1)


## ----prp_label_below, echo=FALSE-----------------------------------------
prp(model, type=2)


## ----prp_label_below, eval=FALSE-----------------------------------------
## prp(model, type=2)


## ----prp_split_labels, echo=FALSE----------------------------------------
prp(model, type=3)


## ----prp_split_labels, eval=FALSE----------------------------------------
## prp(model, type=3)


## ----prp_interior_lables, echo=FALSE-------------------------------------
prp(model, type=4)


## ----prp_interior_lables, eval=FALSE-------------------------------------
## prp(model, type=4)


## ----prp_num_obs, echo=FALSE---------------------------------------------
prp(model, type=2, extra=1)


## ----prp_num_obs, eval=FALSE---------------------------------------------
## prp(model, type=2, extra=1)


## ----prp_per_obs, echo=FALSE---------------------------------------------
prp(model, type=2, extra=101)


## ----prp_per_obs, eval=FALSE---------------------------------------------
## prp(model, type=2, extra=101)


## ----prp_class_rate, echo=FALSE------------------------------------------
prp(model, type=2, extra=2)


## ----prp_class_rate, eval=FALSE------------------------------------------
## prp(model, type=2, extra=2)


## ----prp_add_per_obs, echo=FALSE-----------------------------------------
prp(model, type=2, extra=102)


## ----prp_add_per_obs, eval=FALSE-----------------------------------------
## prp(model, type=2, extra=102)


## ----prp_miss_rate, echo=FALSE-------------------------------------------
prp(model, type=2, extra=3)


## ----prp_miss_rate, eval=FALSE-------------------------------------------
## prp(model, type=2, extra=3)


## ----prp_prob_class, echo=FALSE------------------------------------------
prp(model, type=2, extra=4)


## ----prp_prob_class, eval=FALSE------------------------------------------
## prp(model, type=2, extra=4)


## ----prp_prob_class_per_obs, echo=FALSE----------------------------------
prp(model, type=2, extra=104)


## ----prp_prob_class_per_obs, eval=FALSE----------------------------------
## prp(model, type=2, extra=104)


## ----prp_only_prob, echo=FALSE-------------------------------------------
prp(model, type=2, extra=5)


## ----prp_only_prob, eval=FALSE-------------------------------------------
## prp(model, type=2, extra=5)


## ----prp_second_class, echo=FALSE----------------------------------------
prp(model, type=2, extra=6)


## ----prp_second_class, eval=FALSE----------------------------------------
## prp(model, type=2, extra=6)


## ----prp_second_class_per_obs, echo=FALSE--------------------------------
prp(model, type=2, extra=106)


## ----prp_second_class_per_obs, eval=FALSE--------------------------------
## prp(model, type=2, extra=106)


## ----prp_second_class_only_prob, echo=FALSE------------------------------
prp(model, type=2, extra=7)


## ----prp_second_class_only_prob, eval=FALSE------------------------------
## prp(model, type=2, extra=7)


## ----prp_extra_8, echo=FALSE---------------------------------------------
prp(model, type=2, extra=8)


## ----prp_extra_8, eval=FALSE---------------------------------------------
## prp(model, type=2, extra=8)


## ----prp_extra_9, echo=FALSE---------------------------------------------
prp(model, type=2, extra=9)


## ----prp_extra_9, eval=FALSE---------------------------------------------
## prp(model, type=2, extra=9)


## ----prp_extra_100, echo=FALSE-------------------------------------------
prp(model, type=2, extra=100)


## ----prp_extra_100, eval=FALSE-------------------------------------------
## prp(model, type=2, extra=100)


## ----prp_extra_106, echo=FALSE-------------------------------------------
prp(model, type=2, extra=106, nn=TRUE)


## ----prp_extra_106, eval=FALSE-------------------------------------------
## prp(model, type=2, extra=106, nn=TRUE)


## ----prp_extra_106_ni, echo=FALSE----------------------------------------
prp(model, type=2, extra=106, nn=TRUE, ni=TRUE)


## ----prp_extra_106_ni, eval=FALSE----------------------------------------
## prp(model, type=2, extra=106, nn=TRUE, ni=TRUE)


## ----prp_extra_106_fallen, echo=FALSE------------------------------------
prp(model, type=2, extra=106, nn=TRUE, fallen.leaves=TRUE)


## ----prp_extra_106_fallen, eval=FALSE------------------------------------
## prp(model, type=2, extra=106, nn=TRUE, fallen.leaves=TRUE)


## ----prp_extra_106_fallen_branch, echo=FALSE-----------------------------
prp(model, type=2, extra=106, nn=TRUE, fallen.leaves=TRUE, 
    branch=0.5)


## ----prp_extra_106_fallen_branch, eval=FALSE-----------------------------
## prp(model, type=2, extra=106, nn=TRUE, fallen.leaves=TRUE,
##     branch=0.5)


## ----prp_extra_106_faclen, echo=FALSE------------------------------------
prp(model, type=2, extra=106, nn=TRUE, fallen.leaves=TRUE, 
    faclen=0)


## ----prp_extra_106_faclen, eval=FALSE------------------------------------
## prp(model, type=2, extra=106, nn=TRUE, fallen.leaves=TRUE,
##     faclen=0)


## ----prp_axtra_106_shadow, echo=FALSE------------------------------------
prp(model, type=2, extra=106, nn=TRUE, fallen.leaves=TRUE, 
    shadow.col="grey")


## ----prp_axtra_106_shadow, eval=FALSE------------------------------------
## prp(model, type=2, extra=106, nn=TRUE, fallen.leaves=TRUE,
##     shadow.col="grey")


## ----prp_extra_106_branch, echo=FALSE------------------------------------
prp(model, type=2, extra=106, nn=TRUE, fallen.leaves=TRUE, 
    branch.lty=3)


## ----prp_extra_106_branch, eval=FALSE------------------------------------
## prp(model, type=2, extra=106, nn=TRUE, fallen.leaves=TRUE,
##     branch.lty=3)


## ----eval=FALSE----------------------------------------------------------
## plot(c(0,1), c(0,0), type="l", axes=FALSE, xlab=NA, ylab=NA, lty=2)
## plot(c(0,1), c(0,0), type="l", axes=FALSE, xlab=NA, ylab=NA, lty="dashed")
## plot(c(0,1), c(0,0), type="l", axes=FALSE, xlab=NA, ylab=NA, lty="44")


## ----eval=FALSE----------------------------------------------------------
## install.packages("partykit", repos="http://R-Forge.R-project.org")
## library(partykit)


## ----fig.width=14, out.width="\\textwidth"-------------------------------
class(model)
plot(as.party(model))


## ----out.lines=15--------------------------------------------------------
print(as.party(model))


## ----message=FALSE-------------------------------------------------------
library(partykit)
model <- ctree(formula=form, data=ds[train, vars])


## ----out.lines=NULL------------------------------------------------------
model


## ----message=FALSE-------------------------------------------------------
predicted <- predict(model, ds[test, vars], type="prob")[,2]
riskchart(predicted, actual, risks)


## ------------------------------------------------------------------------
predicted <- predict(model, ds[test, vars], type="response")
sum(actual != predicted)/length(predicted) # Overall error rate
round(100*table(actual, predicted, dnn=c("Actual", "Predicted"))/length(predicted))


## ------------------------------------------------------------------------
plot(model)


## ------------------------------------------------------------------------
library(RWeka)
model <- J48(formula=form, data=ds[train, vars])


## ----out.lines=NULL------------------------------------------------------
model


## ----message=FALSE-------------------------------------------------------
predicted <- predict(model, ds[test, vars], type="prob")[,2]
riskchart(predicted, actual, risks)


## ------------------------------------------------------------------------
predicted <- predict(model, ds[test, vars], type="class")
sum(actual != predicted)/length(predicted) # Overall error rate
round(100*table(actual, predicted, dnn=c("Actual", "Predicted"))/length(predicted))


## ------------------------------------------------------------------------
plot(as.party(model))


## ----out.lines=12--------------------------------------------------------
print(as.party(model))


## ----c50-----------------------------------------------------------------
library(C50)
model <- C5.0(form, ds[train, vars])


## ----c50_print, out.lines=NULL-------------------------------------------
model


## ----out.lines=NULL------------------------------------------------------
C5imp(model)


## ----c50_summary, out.lines=40-------------------------------------------
summary(model)


## ----message=FALSE-------------------------------------------------------
predicted <- predict(model, ds[test, vars], type="prob")[,2]
riskchart(predicted, actual, risks)


## ------------------------------------------------------------------------
predicted <- predict(model, ds[test, vars], type="class")
sum(actual != predicted)/length(predicted) # Overall error rate
round(100*table(actual, predicted, dnn=c("Actual", "Predicted"))/length(predicted))


## ----c50_rules-----------------------------------------------------------
library(C50)
model <- C5.0(form, ds[train, vars], rules=TRUE)


## ----c50_rules_print, out.lines=NULL-------------------------------------
model


## ----out.lines=NULL------------------------------------------------------
C5imp(model)


## ----c50_rules_summary, out.lines=40-------------------------------------
summary(model)


## ----message=FALSE-------------------------------------------------------
predicted <- predict(model, ds[test, vars], type="prob")[,2]
riskchart(predicted, actual, risks)


## ------------------------------------------------------------------------
predicted <- predict(model, ds[test, vars], type="class")
sum(ds[test, target] != predicted)/length(predicted) # Overall error rate
round(100*table(ds[test, target], predicted, dnn=c("Actual", "Predicted"))/length(predicted))


## ----build_regression_model, out.lines=NULL------------------------------
target <- "RISK_MM"
vars <- c(inputs, target)
form <- formula(paste(target, "~ ."))
(model <- rpart(formula=form, data=ds[train, vars]))


## ----basic_plot_regression-----------------------------------------------
plot(model)
text(model)


## ----basic_plot_uniform_regression---------------------------------------
plot(model, uniform=TRUE)
text(model)


## ----basic_plot_extra_regressoin-----------------------------------------
plot(model, uniform=TRUE)
text(model, use.n=TRUE, all=TRUE, cex=.8)


## ----fancy_plot_regression-----------------------------------------------
fancyRpartPlot(model)


## ------------------------------------------------------------------------
prp(model)


## ------------------------------------------------------------------------
prp(model, type=2, extra=101, nn=TRUE, fallen.leaves=TRUE, 
    faclen=0, varlen=0, shadow.col="grey", branch.lty=3)


## ----fig.width=14, out.width="\\textwidth"-------------------------------
class(model)
plot(as.party(model))


## ------------------------------------------------------------------------
model <- ctree(formula=form, data=ds[train, vars])


## ----out.lines=45--------------------------------------------------------
model


## ------------------------------------------------------------------------
plot(model)


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





