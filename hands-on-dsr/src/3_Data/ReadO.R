
## ----module, echo=FALSE, results="asis"----------------------------------
Module <- "ReadO"
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
library(RCurl)
library(foreign)
library(xlsx)
library(openxlsx)


## ----child-demo, child='documentation.Rnw', eval=TRUE--------------------


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




## ----checkwd-------------------------------------------------------------
getwd()


## ----list_csv_files------------------------------------------------------
dir(path="data", pattern="*.csv")


## ----read_csv_heart------------------------------------------------------
heart <- read.csv(file=file.path("data", "heart.csv"))


## ----check_csv_heart-----------------------------------------------------
dim(x=heart)
head(x=heart)
tail(x=heart)
str(object=heart)


## ----read_csv_attempt1---------------------------------------------------
stroke <- read.csv(file.path("data", "stroke.csv"))


## ------------------------------------------------------------------------
str(read.csv)


## ----check_csv_attemp1---------------------------------------------------
dim(stroke)
head(stroke)
tail(stroke)
str(stroke)
summary(stroke)


## ----read_csv_attempt2---------------------------------------------------
stroke <- read.csv(file.path("data", "stroke.csv"), sep=";")
dim(stroke)
head(stroke)
str(stroke)


## ------------------------------------------------------------------------
str(read.csv)


## ------------------------------------------------------------------------
stroke <- read.csv(file.path("data", "stroke.csv"), sep=";")
stroke <- read.csv(file.path("data", "stroke.csv"), header=TRUE, sep=";")
stroke <- read.csv(file.path("data", "stroke.csv"), TRUE, ";")


## ------------------------------------------------------------------------
stroke <- read.csv(file.path("data", "stroke.csv"), ";")


## ----read_csv2-----------------------------------------------------------
stroke <- read.csv2(file.path("data", "stroke.csv"))


## ----review_data_stroke_2------------------------------------------------
dim(stroke)
head(stroke)
tail(stroke)
str(stroke)


## ----eval=FALSE----------------------------------------------------------
## ?read.csv


## ------------------------------------------------------------------------
read.csv


## ----eval=FALSE----------------------------------------------------------
## ds <- read.csv("stroke.csv", stringsAsFactors=FALSE)


## ----eval=FALSE----------------------------------------------------------
## View(stroke)


## ----eval=FALSE----------------------------------------------------------
## library(RGtk2Extras)
## dfedit(stroke)


## ----eval=FALSE----------------------------------------------------------
## library(Deducer)
## date.viewer()


## ------------------------------------------------------------------------
tail(stroke)


## ----read_csv_stroke_nastrings-------------------------------------------
stroke <- read.csv2(file.path("data", "stroke.csv"), na.strings=".")


## ------------------------------------------------------------------------
dim(stroke)
head(stroke)
tail(stroke)
str(stroke)


## ------------------------------------------------------------------------
stroke <- read.csv2(file.path("data", "stroke.csv"), na.strings=c(".", "?", " "))


## ------------------------------------------------------------------------
sapply(stroke, class)


## ------------------------------------------------------------------------
classes <- c("factor", "character", "character", "integer", "factor", 
             "factor", "factor", "factor", "factor")
stroke <- read.csv2(file.path("data", "stroke.csv"), na.strings=".", 
                    colClasses=classes)
sapply(stroke, class)


## ----eval=FALSE----------------------------------------------------------
## write(weather, file=file.path("data", "myweather.csv"), row.names=FALSE)


## ------------------------------------------------------------------------
save(stroke, file=file.path("data", "stroke.RData"))


## ----eval=FALSE----------------------------------------------------------
## read.fwf()


## ----read_table_url------------------------------------------------------
addr <- file.path("http://www.ats.ucla.edu/stat/r/examples/alda/data",
                  "tolerance1_pp.txt")
tolerance <- read.csv(addr)


## ------------------------------------------------------------------------
dim(tolerance)
head(tolerance)
tail(tolerance)
str(tolerance)
summary(tolerance)


## ----save_tolerance, echo=3----------------------------------------------
# Save tolerance backup in case it goes missing
save(tolerance, file=file.path("data", sprintf(".tolerance_%s.RData", format(Sys.time(), "%y%m%d"))))
save(tolerance, file=file.path("data", "tolerance.RData"))


## ----query_key-----------------------------------------------------------
key <- "0Aonsf4v9iDjGdHRaWWRFbXdQN1ZvbGx0LWVCeVd0T1E"


## ------------------------------------------------------------------------
tt <- getForm("https://spreadsheets.google.com/spreadsheet/pub", 
              hl="en_US", key=key, output="csv")
tt


## ------------------------------------------------------------------------
ds <- read.csv(textConnection(tt))
dim(ds)
head(ds)


## ------------------------------------------------------------------------
library(RCurl)
key   <- "0AmbQbL4Lrd61dER5Qnl3bHo4MkVNRlZ1OVdicnZnTHc"
query <- curlEscape("select *")
addr  <- paste0("http://spreadsheets.google.com/tq?",
                "key=", key, 
                "&tq=", query, 
                "&tqx=out:csv")
addr


## ------------------------------------------------------------------------
ds <- read.csv(addr)


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





