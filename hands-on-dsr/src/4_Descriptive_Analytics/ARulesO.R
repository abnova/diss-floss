
## ----module, echo=FALSE, results="asis"----------------------------------
Module <- "ARulesO"
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



## ----load_pacakges, message=FALSE----------------------------------------
library(arules)	        # Association rules.
library(dplyr)          # Data munging: tbl_df(), %>%.


## ----additional_dependent_pacakges, echo=FALSE, message=FALSE------------

# These are dependencies that would otherwise be loaded as required.

library(magrittr)


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




## ----eval=FALSE----------------------------------------------------------
## fname       <- "http://www.biz.uiowa.edu/faculty/jledolter/DataMining/lastfm.csv"
## lastfm      <- read.csv(fname, stringsAsFactors=FALSE)


## ----echo=FALSE, eval=FALSE----------------------------------------------
## save(lastfm, file="data/lastfm.RData")


## ----lastfm_load_dataset, echo=FALSE-------------------------------------
load("data/lastfm.RData")


## ----lastfm_summary, out.lines=NULL--------------------------------------
dsname      <- "lastfm"
ds          <- get(dsname) %>% tbl_df()
ds


## ----lastfm_prepare_dataset, out.lines=NULL------------------------------
ds <- ds %>% select(user, artist) %>% unique()
ds


## ----lastfm_as_transactions----------------------------------------------
library(arules)

trans <- as(split(ds$artist, ds$user), "transactions")


## ----lastfm_inspect_trans, out.lines=8-----------------------------------
inspect(trans[1:5])


## ----lastfm_plot_frequency, fig.height=5---------------------------------
itemFrequencyPlot(trans, support=0.075)


## ----out.lines=NULL------------------------------------------------------
model <- apriori(trans, parameter=list(support=0.01, confidence=0.5))


## ----out.lines=10--------------------------------------------------------
inspect(model)


## ----out.lines=10--------------------------------------------------------
inspect(subset(model, subset=lift>8))
inspect(sort(subset(model, subset=lift>8), by="confidence"))


## ----constants_baskets, echo=FALSE---------------------------------------
set.seed(42)
nb <- 10    # Number of baskets.
ni <- 5     # Number of items.
nc <- 40    # Number of combinations.


## ----constants_baskets, eval=FALSE---------------------------------------
## set.seed(42)
## nb <- 10    # Number of baskets.
## ni <- 5     # Number of items.
## nc <- 40    # Number of combinations.


## ----random_basket_dataset-----------------------------------------------
ds <- data.frame(id=sort(sprintf("b%02d", sample(1:nb, nc, replace=TRUE))),
                 item=sprintf("i%1d", sample(1:ni, nc, replace=TRUE)))
ds <- unique(ds)
rownames(ds) <- NULL


## ----summary_basket_sizes, out.lines=6-----------------------------------
ds %>% group_by(id) %>% tally()


## ----list_basket_contents, out.lines=NULL--------------------------------
ds %>% group_by(id) %>% summarise(items=paste(sort(item), collapse=", "))


## ----list_baskets_with-i1, out.lines=NULL--------------------------------
ds %>% group_by(id) %>% summarise(i1="i1" %in% item) %>% filter(i1)


## ----one_itemset_freq, out.lines=NULL------------------------------------
ds %>% group_by(item) %>% tally()


## ----one_itemset_support, out.lines=NULL---------------------------------
ds %>% group_by(item) %>% tally() %>% mutate(s=n/nb)


## ----arules_create_dst---------------------------------------------------
library(arules)
dst <- as(split(ds$item, ds$id), "transactions")
dst


## ----dst_item_frequency--------------------------------------------------
itemFrequency(dst)


## ------------------------------------------------------------------------
itemFrequency(dst, type="absolute")


## ----dst_plot_freq, fig.height=3.5---------------------------------------
itemFrequencyPlot(dst)


## ----echo=FALSE----------------------------------------------------------
is2.freq <- group_by(ds,id) %>% 
  summarise(is.1.2="i1" %in% item & "i2" %in% item) %>% 
  tally(is.1.2)

is3.freq <- group_by(ds,id) %>%
  summarise(is.1.2.3="i1" %in% item & 
            "i2" %in% item & 
            "i3" %in% item) %>%
  tally(is.1.2.3)


## ----out.lines=NULL------------------------------------------------------
merge(ds, ds, by="id") %>% 
  subset(as.character(item.x) < as.character(item.y)) %>% 
  mutate(itemset=paste(item.x, item.y)) %>% 
  group_by(itemset) %>% 
  tally()


## ----out.lines=NULL------------------------------------------------------
merge(ds, ds, by="id") %>% 
  merge(ds, by="id") %>% 
  subset(as.character(item.x) < as.character(item.y) & 
         as.character(item.y) < as.character(item)) %>% 
  mutate(itemset=paste(item.x, item.y, item)) %>% 
  group_by(itemset) %>% 
  tally()


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





