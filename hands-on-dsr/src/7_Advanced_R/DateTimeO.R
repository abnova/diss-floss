
## ----module, echo=FALSE, results="asis"----------------------------------
Module <- "DateTimeO"
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
library(lubridate)       # Simplified date/time handling.
library(ggplot2)         # Visualise data.
library(tidyr)           # Tidy the dataset.
library(rattle)          # The weatherAUS dataset.
library(scales)          # Rescaling axes in ggplot2.
library(WDI)             # World bank data.
library(countrycode)
library(plyr)            # Transform data.
library(gridExtra)       # Multiple plots on a grid.


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




## ----read_stroke_data----------------------------------------------------
stroke <- read.csv(file.path("data", "stroke.csv"), sep=";", na.strings=".")
dds <- stroke
head(dds$DIED)
head(dds$DSTR)


## ----dates_as_factors----------------------------------------------------
class(dds$DIED)
class(dds$DSTR)


## ----stroke_dataset_convert_date_type------------------------------------
class(dds$DIED <- as.Date(dds$DIED, format="%d.%m.%Y"))
class(dds$DSTR <- as.Date(dds$DSTR, format="%d.%m.%Y"))


## ------------------------------------------------------------------------
head(dds$DIED)
head(dds$DSTR)


## ----lubridate_read_csv--------------------------------------------------
ds <- stroke


## ----transform_dates_again-----------------------------------------------
library(lubridate)
class(ds$DIED <- dmy(ds$DIED))
class(ds$DSTR <- dmy(ds$DSTR))


## ------------------------------------------------------------------------
head(ds$DIED)
head(ds$DSTR)


## ------------------------------------------------------------------------
(ct <- Sys.time())
class(ct)
str(ct)
unclass(ct)


## ----out.lines=12--------------------------------------------------------
(ct <- as.POSIXlt(ct))
class(ct)
str(ct)
unclass(ct)


## ----simple_string_formats-----------------------------------------------
format(Sys.time(), "%a %d %b %Y %H:%M:%S %Z")


## ----compute_on_dates----------------------------------------------------
dds$LIVED <- dds$DIED - dds$DSTR
head(dds$LIVED)
class(dds$LIVED)


## ----compute_on_dates_2--------------------------------------------------
ds$LIVED <- ds$DIED - ds$DSTR
head(ds$LIVED)
class(ds$LIVED)


## ------------------------------------------------------------------------
as.integer(ds$LIVED[1])/60/60/24


## ------------------------------------------------------------------------
units(ds$LIVED)
units(ds$LIVED) <- "days"
units(ds$LIVED)
head(ds$LIVED)


## ------------------------------------------------------------------------
ds$INTERVAL <- with(ds, interval(DSTR, DIED))
head(ds$INTERVAL)
class(ds$INTERVAL)
max(ds$INTERVAL, na.rm=TRUE)
min(ds$INTERVAL, na.rm=TRUE)


## ------------------------------------------------------------------------
head(duration(ds$INTERVAL))


## ----fig.width=14, out.width="\\textwidth"-------------------------------
ds <- stroke
ds$DSTR <- dmy(ds$DSTR)
g <- ggplot(data=ds, aes(wday(DSTR, label=TRUE, abbr=FALSE)))
g <- g + geom_histogram(colour="white", fill="lightblue")
g <- g + ggtitle("Day of Week of Incidence of Stroke")
g <- g + xlab("Weekday")
print(g)


## ----fig.width=14, out.width="\\textwidth"-------------------------------
g <- ggplot(data=ds, aes(mday(DSTR)))
g <- g + geom_histogram(binwidth=1, colour="white", fill="orange")
g <- g + ggtitle("Day of Month of Incidence of Stroke")
g <- g + xlab("Day of Month")
print(g)


## ----fig.width=14, out.width="\\textwidth"-------------------------------
library("reshape2") # melt()
library("ggplot2")  # ggplot()

ds <- data.frame(L1=100+c(0, cumsum(runif(99, -20, 20))),
                 L2=150+c(0, cumsum(runif(99, -10, 10))),
                 Date=seq.Date(as.Date("2000-01-01"), 
                   by="1 month", length.out=100))

dsm <- melt(ds, id="Date")

g <- ggplot(data=dsm, aes(x=Date, y=value, colour=variable))
g <- g + geom_line()
g <- g + ylab("Observation")
g <- g + labs(colour="Location")
print(g)


## ------------------------------------------------------------------------
library(WDI)
library(ggplot2)
library(countrycode)


## ------------------------------------------------------------------------
(meta.data <- WDIsearch("Fertility rate", field="name", short=FALSE))
(indicators <- meta.data[1:2, 1])
countries <- c("United States", "Britain", "India", "China", "Australia")
(iso2char <- countrycode(countries, "country.name", "iso2c"))
(wdids <- WDI(iso2char, meta.data[1:2,1], start=2001, end=2011))


## ------------------------------------------------------------------------
plots <- lapply(indicators, function(nm)
{ 
  p <- ggplot(wdids, aes(x=year, y=wdids[,nm], group=country, color=country),
              environment=environment())
  p <- p + geom_line(size=1)
  p <- p + scale_x_continuous(name="Year", breaks=c(unique(wdids[,"year"])))
  p <- p + scale_y_continuous(name=nm)
  p <- p + scale_linetype_discrete(name="Country")
  p <- p + theme(legend.title=element_blank())
  p <- p + ggtitle(paste(meta.data[meta.data[,1]==nm, "name"], "\n"))
})


## ----fig.width=7.5-------------------------------------------------------
do.call(grid.arrange, plots)


## ------------------------------------------------------------------------
vars <- c("Date", "MinTemp", "MaxTemp", "Sunshine", "Rainfall", "Evaporation")
ds <- weather[vars]


## ------------------------------------------------------------------------
ds$Sunshine <- ds$Sunshine * 60


## ------------------------------------------------------------------------
ds$CumRainfall <- cumsum(ds$Rainfall)
ds$CumEvaporation <- cumsum(ds$Evaporation)


## ------------------------------------------------------------------------
dsm <- melt(ds, id="Date")


## ----fig.width=14, out.width="\\textwidth"-------------------------------
g <- ggplot(dsm, aes(x=Date, y=value, colour=variable))
g <- g + geom_point()
print(g)            


## ----fig.width=14, out.width="\\textwidth"-------------------------------
g <- ggplot(dsm, aes(x=Date, y=value, colour=variable))
g <- g + geom_point()
g <- g + scale_y_log10()
print(g)            


## ----fig.width=14, out.width="\\textwidth", warning=FALSE----------------
asinh_trans <- function() trans_new(name="asinh",
                                    transform=asinh,
                                    inverse=sinh)
g <- ggplot(dsm, aes(x=Date, y=value, colour=variable))
g <- g + geom_point()
g <- g + scale_y_continuous(trans="asinh")
print(g)  


## ----fig.width=14, out.width="\\textwidth", warning=FALSE----------------
g <- ggplot(dsm, aes(x=Date, y=value, colour=variable))
g <- g + geom_point()
g <- g + scale_y_continuous(trans="asinh",
                            limits=c(-1e4, 1e4))
print(g)  


## ----fig.width=14, out.width="\\textwidth", warning=FALSE----------------
g <- ggplot(dsm, aes(x=Date, y=value, colour=variable))
g <- g + geom_point()
g <- g + scale_y_continuous(trans="asinh",
                            breaks=c(-10, 0, 10, 1e2, 1e3))
print(g)  


## ----fig.width=14, out.width="\\textwidth", warning=FALSE----------------
g <- ggplot(dsm, aes(x=Date, y=value, colour=variable))
g <- g + geom_point()
g <- g + scale_y_continuous(trans="asinh",
                            breaks=c(-10, 0, 10, 1e2, 1e3),
                            labels=c("-10", "0", "10", "100", "1K"))
print(g)  


## ----fig.width=14, out.width="\\textwidth", warning=FALSE----------------
g <- ggplot(dsm, aes(x=Date, y=value, colour=variable))
g <- g + geom_line()
g <- g + scale_y_continuous(trans="asinh",
                            breaks=c(-10, 0, 10, 1e2, 1e3),
                            labels=c("-10", "0", "10", "100", "1K"))
print(g)  


## ----fig.width=14, out.width="\\textwidth", warning=FALSE----------------
draw.lines <- c("CumRainfall", "CumEvaporation")

g <- ggplot(dsm, aes(x=Date, y=value, colour=variable))
g <- g + geom_point(data=subset(dsm, !variable %in% draw.lines))
g <- g + geom_line(data=subset(dsm, variable %in% draw.lines))
g <- g + scale_y_continuous(trans="asinh",
                            breaks=c(-10, 0, 10, 1e2, 1e3),
                            labels=c("-10", "0", "10", "100", "1K"))
print(g)  


## ----fig.width=14, out.width="\\textwidth", warning=FALSE----------------
events <- as.Date(c("2007-12-25", "2008-03-22"))

g <- ggplot(dsm, aes(x=Date, y=value, colour=variable))
g <- g + geom_point(data=subset(dsm, !variable %in% draw.lines))
g <- g + geom_line(data=subset(dsm, variable %in% draw.lines))
g <- g + scale_y_continuous(trans="asinh",
                            breaks=c(-10, 0, 10, 1e2, 1e3),
                            labels=c("-10", "0", "10", "100", "1K"))
g <- g + geom_vline(xintercept=as.numeric(events), linetype=3)
g <- g + annotate("text", events[1], -9, label="Christmas", size=3, colour="blue")
g <- g + annotate("text", events[2], -9, label="Easter", size=3, colour="purple")
print(g)  


## ----fig.width=14, out.width="\\textwidth", warning=FALSE----------------
g <- ggplot(dsm, aes(x=Date, y=value, colour=variable))
g <- g + geom_point(data=subset(dsm, !variable %in% draw.lines))
g <- g + geom_line(data=subset(dsm, variable %in% draw.lines))
g <- g + scale_y_continuous(trans="asinh",
                            breaks=c(-10, 0, 10, 1e2, 1e3),
                            labels=c("-10", "0", "10", "100", "1K"))
g <- g + geom_vline(xintercept=as.numeric(events), linetype=3)
g <- g + annotate("text", events[1], -9, label="Christmas", size=3, colour="blue")
g <- g + annotate("text", events[2], -9, label="Easter", size=3, colour="purple")
g <- g + ggtitle(sprintf("Weather pattern for %s", weather$Location[1]))
g <- g + theme(legend.direction="horizontal", legend.position="bottom")
print(g)  


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





