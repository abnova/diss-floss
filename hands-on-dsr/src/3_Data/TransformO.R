
## ----module, echo=FALSE, results="asis"----------------------------------
Module <- "TransformO"
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
library(rattle)		# The weatherAUS datasets and normVarNames()
library(ggplot2)	# Visualise the transforms.
library(plyr)		# Transform using ddplyr()
library(dplyr)		# Transform using ddplyr()
library(reshape2)	# melt() and dcast()


## ----echo=FALSE, message=FALSE-------------------------------------------
library(Rcpp)


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




## ----weather_dataset, message=FALSE--------------------------------------
library(rattle)
ds         <- weatherAUS
names(ds)  <- normVarNames(names(ds))	# Lower case variable names.
str(ds)


## ------------------------------------------------------------------------
cities <- c("Adelaide", "Brisbane", "Canberra", "Darwin")
levels(ds$location)
summary(ds$location)

dss    <- subset(ds, location %in% cities)
levels(dss$location)
summary(dss$location)


## ------------------------------------------------------------------------
dss$location <- factor(dss$location)
levels(dss$location)
summary(dss$location)


## ------------------------------------------------------------------------
levels(dss$location)
summary(dss$location)

dss$location <- factor(dss$location, levels=rev(levels(dss$location)))

levels(dss$location)
summary(dss$location)


## ----add_column----------------------------------------------------------
ds$temp_range <- ds$max_temp - ds$min_temp
str(ds)
p <- ggplot(ds, aes(x=temp_range))
p <- p + geom_bar(binwidth=1)
p


## ----transform_add_column------------------------------------------------
ds <- transform(ds, 
                temp_range=max_temp-min_temp,
                excess=rainfall-evaporation)
sum(ds$excess, na.rm=TRUE)
str(ds)
ggplot(ds, aes(x=excess)) + geom_bar(binwidth=1)


## ----dplyr_group_by, out.lines=NULL--------------------------------------
weatherAUS %>%
  group_by(Location) %>%
  summarise(total = sum(Rainfall)) %>%
  arrange(desc(total)) %>%
  head(5)


## ----remove_column, out.lines=NULL---------------------------------------
tail(ds$excess)
names(ds)
ds$excess <- NULL
tail(ds$excess)
names(ds)


## ------------------------------------------------------------------------
dss <- subset(ds, date==max(date))
dim(dss)
head(dss)


## ------------------------------------------------------------------------
library(reshape2)
dssm <- melt(dss, c("date", "location"))
dim(dssm)
head(dssm)
tail(dssm)
dssm[sample(nrow(dssm), 6),]


## ----out.lines=10--------------------------------------------------------
dssmc <- dcast(dssm, date + location ~ variable)
dim(dss)
dim(dssmc)
head(dss)
head(dssmc)


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





