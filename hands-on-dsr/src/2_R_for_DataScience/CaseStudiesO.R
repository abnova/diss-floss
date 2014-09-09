
## ----module, echo=FALSE, results="asis"----------------------------------
Module <- "CaseStudiesO"
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
library(xlsx)           # Read Excel spreadsheets.
library(rattle)         # normVarNames().
library(stringr)        # String manpiulation.
library(tidyr)          # Tidy the dataset.
library(dplyr)          # Data manipulation.
library(ggplot2)        # Visualise data.
library(scales)         # Include commas in numbers.
library(directlabels)   # Dodging labels for ggplot2.


## ----additional_dependent_pacakges, echo=FALSE, message=FALSE------------
# These are dependencies that would otherwise be loaded as required.

library(magrittr)


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




## ------------------------------------------------------------------------
datagovau <- "http://data.gov.au/dataset/"
atogovau  <- file.path(datagovau, "9b57d00a-da75-4db9-8899-6537dd60eeba/resource")


## ----ato_web_period, echo=FALSE------------------------------------------
period <- " - July 2013 to April 2014.csv"
months <- c("Jul-13", "Aug-13", "Sep-13", "Oct-13", "Nov-13", 
            "Dec-13", "Jan-14", "Feb-14", "Mar-14", "Apr-14")


## ----ato_web_period, eval=FALSE------------------------------------------
## period <- " - July 2013 to April 2014.csv"
## months <- c("Jul-13", "Aug-13", "Sep-13", "Oct-13", "Nov-13",
##             "Dec-13", "Jan-14", "Feb-14", "Mar-14", "Apr-14")


## ----csv2zip, echo=FALSE-------------------------------------------------
csv2zip <- function(fname)
{
  fname %>% tolower() %>% str_replace_all(" |-|csv$", "") %>% str_c("zip")
}


## ----tefname, echo=FALSE, results="hide"---------------------------------
tefname <- str_c("A Sample File Of Data As CSV", period)
tefname


## ----tezname, echo=FALSE, results="hide"---------------------------------
tezname <- csv2zip(tefname)
tezname


## ----csv2zip, eval=FALSE-------------------------------------------------
## csv2zip <- function(fname)
## {
##   fname %>% tolower() %>% str_replace_all(" |-|csv$", "") %>% str_c("zip")
## }


## ----tefname-------------------------------------------------------------
tefname <- str_c("A Sample File Of Data As CSV", period)
tefname


## ----tezname-------------------------------------------------------------
tezname <- csv2zip(tefname)
tezname


## ------------------------------------------------------------------------
brfname <- str_c("Browser by month and traffic source", period)
brzname <- csv2zip(brfname)


## ------------------------------------------------------------------------
enfname <- str_c("Entry pages by month and traffic source", period)
enzname <- csv2zip(enfname)


## ------------------------------------------------------------------------
refname <- str_c("Entry referrers by month and traffic source", period)
rezname <- csv2zip(refname)


## ------------------------------------------------------------------------
exfname <- str_c("Exit pages by month and traffic source", period)
exzname <- csv2zip(exfname)


## ------------------------------------------------------------------------
kwfname <- str_c("Local keywords (top 100) by month and traffic source", period)
kwzname <- csv2zip(kwfname)


## ------------------------------------------------------------------------
osfname <- str_c("Operating System (platform) by month and traffic", period)
oszname <- csv2zip(osfname)


## ------------------------------------------------------------------------
vifname <- str_c("Pages by month and traffic source", period)
vizname <- csv2zip(vifname)


## ------------------------------------------------------------------------
fname <- file.path(atogovau, 
                   "121dfe58-044b-4f21-b4ee-6f9335a44413",
                   "download",
                   enzname)
fname


## ----eval=FALSE----------------------------------------------------------
## temp  <- tempfile(fileext=".zip")
## download.file(fname, temp)


## ----eval=FALSE----------------------------------------------------------
## entry <- temp %>% unz(enfname) %>% read.csv()
## unlink(temp)


## ----echo=FALSE----------------------------------------------------------
# Name of local copy of file so it is not always downloaded
lfname <- file.path("data", str_replace(enzname, ".zip$", ".RData"))


## ----echo=FALSE, eval=FALSE----------------------------------------------
## # Interactively save the downloaded file.
## save(entry, file=lfname)


## ----echo=FALSE----------------------------------------------------------
# Always load the cached copy of the dataset.
load(lfname)


## ------------------------------------------------------------------------
dsname <- "entry"
ds     <- dsname %>% get() %>% tbl_df()


## ------------------------------------------------------------------------
names(ds) <- normVarNames(names(ds))
names(ds)
vars <- names(ds)


## ------------------------------------------------------------------------
dso <- ds
rm(entry)


## ----out.lines=7---------------------------------------------------------
ds


## ------------------------------------------------------------------------
length(levels(ds$entry_page))


## ----out.lines=NULL------------------------------------------------------
summary(ds[-1])


## ------------------------------------------------------------------------
table(ds$month)


## ------------------------------------------------------------------------
which(ds$source=="4")


## ----eval=FALSE----------------------------------------------------------
## $ tail -n+198495 <filename>.csv | head -n10


## ------------------------------------------------------------------------
dim(ds)
issue <- which(ds$source=="4")
issue
dso <- ds <- ds[-issue,]
dim(ds)


## ------------------------------------------------------------------------
levels(ds$month)
ds$month <- factor(ds$month, levels=months)
levels(ds$month)


## ------------------------------------------------------------------------
format(sum(ds$views), big.mark=",")
format(sum(ds$visits), big.mark=",")


## ----ato_web_en_monthly, fig.height=4, out.width="\\textwidth", echo=FALSE----
ds                                                  %>% 
  group_by(month)                                   %>% 
  summarise(views=sum(views), visits=sum(visits))   %>%
  gather(type, count, -month)                       %>%
  ggplot(aes(x=month, y=count, fill=type))           +
  geom_bar(stat="identity", position="dodge")        +
  scale_y_continuous(labels=comma)                   +
  labs(fill="Type", x="Month", y="Count")            +
  theme(axis.text.x=element_text(angle=45, hjust=1))


## ----ato_web_en_monthly, fig.height=4, out.width="\\textwidth", eval=FALSE----
## ds                                                  %>%
##   group_by(month)                                   %>%
##   summarise(views=sum(views), visits=sum(visits))   %>%
##   gather(type, count, -month)                       %>%
##   ggplot(aes(x=month, y=count, fill=type))           +
##   geom_bar(stat="identity", position="dodge")        +
##   scale_y_continuous(labels=comma)                   +
##   labs(fill="Type", x="Month", y="Count")            +
##   theme(axis.text.x=element_text(angle=45, hjust=1))


## ----ato_web_en_monthly_facet, fig.height=5, fig.width=9, out.width="\\textwidth", echo=FALSE----
ds %>% 
  group_by(month, source)                           %>% 
  summarise(views=sum(views), visits=sum(visits))   %>%
  gather(type, count, -c(month, source))            %>%
  ggplot(aes(x=month, y=count, fill=type))           +
  geom_bar(stat="identity", position="dodge")        +
  scale_y_continuous(labels=comma)                   +
  labs(fill="Type", x="Month", y="Count")            +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  facet_wrap(~source)


## ----ato_web_en_monthly_facet, fig.height=5, fig.width=9, out.width="\\textwidth", eval=FALSE----
## ds %>%
##   group_by(month, source)                           %>%
##   summarise(views=sum(views), visits=sum(visits))   %>%
##   gather(type, count, -c(month, source))            %>%
##   ggplot(aes(x=month, y=count, fill=type))           +
##   geom_bar(stat="identity", position="dodge")        +
##   scale_y_continuous(labels=comma)                   +
##   labs(fill="Type", x="Month", y="Count")            +
##   theme(axis.text.x=element_text(angle=45, hjust=1)) +
##   facet_wrap(~source)


## ------------------------------------------------------------------------
ds <- ds %>% subset(source=="External")
dim(ds)
ds


## ------------------------------------------------------------------------
ds$entry_page[sample(nrow(ds), 10)]


## ------------------------------------------------------------------------
ds$entry_page %>% unique() %>% length()


## ------------------------------------------------------------------------
tbl <- ds$entry_page %>% table()
summary(tbl)


## ------------------------------------------------------------------------
dsa <- ds %>% group_by(entry_page) %>% summarise(total=sum(visits))
head(dsa$total)


## ----eval=FALSE----------------------------------------------------------
## fname <- file.path(atogovau,
##                    "08e33518-dd6f-42d8-a035-f4c0fc7f18f3",
##                    "download",
##                    brzname)
## temp  <- tempfile(fileext=".zip")
## download.file(fname, temp)
## browser <- read.csv(unz(temp, brfname))
## unlink(temp)


## ----echo=FALSE----------------------------------------------------------
# Name of local copy of file so it is not always downloaded
lfname <- file.path("data", str_replace(brzname, ".zip$", ".RData"))


## ----echo=FALSE, eval=FALSE----------------------------------------------
## # Interactively save the downloaded file.
## save(browser, file=lfname)


## ----echo=FALSE----------------------------------------------------------
# Always load the cached copy of the dataset.
load(lfname)


## ----out.lines=NULL------------------------------------------------------
dsname       <- "browser"
ds           <- dsname %>% get() %>% tbl_df()
names(ds)    <- normVarNames(names(ds))
names(ds)[3] <- "source"
names(ds)
vars         <- names(ds)
dso          <- ds
ds


## ------------------------------------------------------------------------
ds[sample(nrow(ds), 6),]


## ----out.lines=8---------------------------------------------------------
summary(ds)


## ------------------------------------------------------------------------
levels(ds$month)
ds$month <- factor(ds$month, levels=months)
levels(ds$month)


## ------------------------------------------------------------------------
ds$ratio <- ds$views/ds$visits
summary(ds$ratio)


## ----out.lines=NULL------------------------------------------------------
freq <- ds                      %>% 
  group_by(source)              %>% 
  summarise(total=sum(visits))
freq


## ----out.lines=NULL------------------------------------------------------
ib <- ds                       %>% 
  filter(source == "Internal") %>%
  group_by(browser)            %>% 
  summarise(total=sum(visits)) %>%
  arrange(desc(total))
ib


## ----ato_br_internal, echo=FALSE, fig.height=4, fig.width=8, out.width="\\textwidth"----
ds                                                   %>%
  filter(source=="Internal", visits > 1000)          %>%
  ggplot(aes(month, visits, fill=browser))            +
  geom_bar(stat="identity")                           +
  scale_y_continuous(labels=comma)                    +
  theme(axis.text.x=element_text(angle=45, hjust=1))


## ----ato_br_internal, eval=FALSE-----------------------------------------
## ds                                                   %>%
##   filter(source=="Internal", visits > 1000)          %>%
##   ggplot(aes(month, visits, fill=browser))            +
##   geom_bar(stat="identity")                           +
##   scale_y_continuous(labels=comma)                    +
##   theme(axis.text.x=element_text(angle=45, hjust=1))


## ----out.lines=NULL------------------------------------------------------
eb <- ds                       %>% 
  filter(source == "External") %>% 
  group_by(browser)            %>% 
  summarise(total=sum(visits)) %>% 
  arrange(desc(total))
head(eb, 10)


## ----out.lines=NULL------------------------------------------------------
tail(eb, 15)


## ----ato_browser_external, echo=FALSE, fig.height=4, fig.width=8, out.width="\\textwidth"----
ds                                                        %>%
  filter(source == "External", visits > 20000)            %>%
  ggplot(aes(month, visits, fill=browser))                 +
  geom_bar(stat="identity")                                +
  facet_wrap(~browser)                                     +
  scale_y_continuous(labels=comma)                         +
  theme(axis.text.x=element_text(angle=45, hjust=1))       +
  theme(legend.position="none")


## ----ato_browser_external, eval=FALSE------------------------------------
## ds                                                        %>%
##   filter(source == "External", visits > 20000)            %>%
##   ggplot(aes(month, visits, fill=browser))                 +
##   geom_bar(stat="identity")                                +
##   facet_wrap(~browser)                                     +
##   scale_y_continuous(labels=comma)                         +
##   theme(axis.text.x=element_text(angle=45, hjust=1))       +
##   theme(legend.position="none")


## ----eval=FALSE----------------------------------------------------------
## fname    <- file.path(atogovau,
##                       "c6b745ec-439c-4dc4-872a-f48f5368d58e",
##                       "download",
##                       kwzname)
## temp     <- tempfile(fileext=".zip")
## download.file(fname, temp)
## keywords <- read.csv(unz(temp, kwfname))
## unlink(temp)


## ----echo=FALSE----------------------------------------------------------
# Name of local copy of file so it is not always downloaded
lfname <- file.path("data", str_replace(kwzname, ".zip$", ".RData"))


## ----echo=FALSE, eval=FALSE----------------------------------------------
## # Interactively save the downloaded file.
## save(keywords, file=lfname)


## ----echo=FALSE----------------------------------------------------------
# Always load the cached copy of the dataset.
load(lfname)


## ------------------------------------------------------------------------
dsname       <- "keywords"
ds           <- dsname %>% get() %>% tbl_df()
names(ds)    <- normVarNames(names(ds))
names(ds)[1] <- "keyword"
names(ds)[3] <- "source"
ds$month     <- factor(ds$month, levels=months)
vars         <- names(ds)
dso          <- ds


## ----out.lines=7---------------------------------------------------------
ds


## ----out.lines=7---------------------------------------------------------
ds                                                  %>%
  group_by(keyword)                                 %>% 
  summarise(views=sum(views), visits=sum(visits))   %>%
  arrange(desc(views))                              %>%
  head(40)



## ----ato_kw_top40, echo=FALSE--------------------------------------------
ds                                                  %>% 
  group_by(keyword)                                 %>% 
  summarise(views=sum(views), visits=sum(visits))   %>%
  arrange(desc(views))                              %>%
  head(40)                                          %>%
  gather(type, count, -keyword)                     %>%
  ggplot(aes(x=keyword, y=count, fill=type))         +
  geom_bar(stat="identity", position="dodge")        +
  scale_y_continuous(labels=comma)                   +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme(legend.position="top")                       +
  labs(x="")                                         +
  coord_flip()


## ----ato_kw_top40, eval=FALSE--------------------------------------------
## ds                                                  %>%
##   group_by(keyword)                                 %>%
##   summarise(views=sum(views), visits=sum(visits))   %>%
##   arrange(desc(views))                              %>%
##   head(40)                                          %>%
##   gather(type, count, -keyword)                     %>%
##   ggplot(aes(x=keyword, y=count, fill=type))         +
##   geom_bar(stat="identity", position="dodge")        +
##   scale_y_continuous(labels=comma)                   +
##   theme(axis.text.x=element_text(angle=45, hjust=1)) +
##   theme(legend.position="top")                       +
##   labs(x="")                                         +
##   coord_flip()


## ----ato_kw_top40_internal, echo=FALSE-----------------------------------
ds                                                  %>% 
  subset(source=="Internal") %>%
  group_by(keyword)                                 %>% 
  summarise(views=sum(views), visits=sum(visits))   %>%
  arrange(desc(views))                              %>%
  head(40)                                          %>%
  gather(type, count, -keyword)                     %>%
  ggplot(aes(x=keyword, y=count, fill=type))         +
  geom_bar(stat="identity", position="dodge")        +
  scale_y_continuous(labels=comma)                   +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme(legend.position="top")                       +
  coord_flip()


## ----ato_kw_top40_internal, eval=FALSE-----------------------------------
## ds                                                  %>%
##   subset(source=="Internal") %>%
##   group_by(keyword)                                 %>%
##   summarise(views=sum(views), visits=sum(visits))   %>%
##   arrange(desc(views))                              %>%
##   head(40)                                          %>%
##   gather(type, count, -keyword)                     %>%
##   ggplot(aes(x=keyword, y=count, fill=type))         +
##   geom_bar(stat="identity", position="dodge")        +
##   scale_y_continuous(labels=comma)                   +
##   theme(axis.text.x=element_text(angle=45, hjust=1)) +
##   theme(legend.position="top")                       +
##   coord_flip()


## ----eval=FALSE----------------------------------------------------------
## library(gdata)
## 
## ds <- read.xls("results.xlsx")
## dim(ds)


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





