
## ----module, echo=FALSE, results="asis"----------------------------------
Module <- "StyleO"
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



## ----echo=FALSE, message=FALSE-------------------------------------------
library(caret) # For Max Kuhn citation.
library(rattle)


## ----child-demo, child="documentation.Rnw", eval=TRUE--------------------


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




## ----eval=FALSE----------------------------------------------------------
## generatePlots.R


## ----eval=FALSE----------------------------------------------------------
## generatePlots.r


## ----eval=FALSE----------------------------------------------------------
## fancyPlot.R


## ----eval=FALSE----------------------------------------------------------
## fancy_plot.R
## fancy.plot.R
## fancy_plot.r


## ----eval=FALSE----------------------------------------------------------
## weather.RData


## ----eval=FALSE----------------------------------------------------------
## weather.rdata
## weather.Rdata


## ----eval=FALSE----------------------------------------------------------
## weather.csv


## ----eval=FALSE----------------------------------------------------------
## weather.CSV


## ----eval=FALSE----------------------------------------------------------
## displayPlotAgain


## ----eval=FALSE----------------------------------------------------------
## DisplayPlotAgain
## displayplotagain


## ----eval=FALSE----------------------------------------------------------
## list.of.frames
## lib.cmd


## ----eval=FALSE----------------------------------------------------------
## list_of_frames


## ----eval=FALSE----------------------------------------------------------
## MAX.LINES


## ----eval=FALSE----------------------------------------------------------
## const.max.lines


## ----eval=FALSE----------------------------------------------------------
## min_temp
## wind_gust_speed


## ----eval=FALSE----------------------------------------------------------
## max.pressure
## wind.dir
## WindSpeed


## ----eval=FALSE----------------------------------------------------------
## read.csv(file="data.csv", sep=";", na.strings=".")


## ----eval=FALSE----------------------------------------------------------
## read.csv(file = "data.csv", sep =
##          ";", na.strings
##          = ".")


## ----eval=FALSE----------------------------------------------------------
## while (blueSky())
## {
##   openTheWindows()
##   doSomeResearch()
## }
## retireForTheDay()


## ----eval=FALSE----------------------------------------------------------
## while (blueSky()) {
##   openTheWindows()
##   doSomeResearch()
## }
## retireForTheDay()


## ----eval=FALSE----------------------------------------------------------
## dial.plot <- function(label="UseR!"
##                       , value=78
##                       , dial.radius=1
##                       , value.cex=3
##                       , value.color="black"
##                       , label.cex=3
##                       , label.color="black")
## {
##   ...
## }


## ----eval=FALSE----------------------------------------------------------
## dial.plot <- function(label="UseR!", value=78, dial.radius=1, value.cex=3,
##                       value.color="black", label.cex=3, label.color="black")
## {
##   ...
## }
## 
## dial.plot <- function(label="UseR!",
##                       value=78,
##                       dial.radius=1,
##                       value.cex=3,
##                       value.color="black",
##                       label.cex=3,
##                       label.color="black")
## {
##   ...
## }


## ----eval=FALSE----------------------------------------------------------
## dial.plot(label="UseR!"
##           , value=78
##           , dial.radius=1
##           , value.cex=3
##           , value.color="black"
##           , label.cex=3
##           , label.color="black")


## ----eval=FALSE----------------------------------------------------------
## dial.plot(label="UseR!", value=78, dial.radius=1, value.cex=3,
##           value.color="black", label.cex=3, label.color="black")
## 
## dial.plot(label="UseR!",
##           value=78,
##           dial.radius=1,
##           value.cex=3,
##           value.color="black",
##           label.cex=3,
##           label.color="black")


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





