
## ----module, echo=FALSE, results="asis"----------------------------------
Module <- "KnitRO"
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
library(rattle)          # The weather dataset.
library(ggplot2)         # Data visualisation.
library(xtable)          # Format R data frames as LaTeX tables.
library(Hmisc)           # Escape special charaters in R strings for LaTeX.
library(diagram)         # Produce a flowchart.
library(dplyr)           # Data munging: tbl_df() for printing data frames.


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




## ----example_random_mean-------------------------------------------------
x <- runif(1000) * 1000
head(x)
mean(x)


## ----example_kable, echo=TRUE, results="asis", out.lines=NULL, out.truncate=NULL----
library(rattle)
library(dplyr)
set.seed(42)

dsname <- "weatherAUS"
ds     <- tbl_df(get(dsname))
nobs   <- nrow(ds)
obs    <- sample(nobs, 5)
vars   <- 2:7
ds     <- ds[obs, vars]
kable(ds)


## ----kable_no_row_names, echo=TRUE, results="asis", out.lines=NULL, out.truncate=NULL----
kable(ds, row.names=FALSE)


## ----kable_digits, echo=TRUE, results="asis", out.lines=NULL, out.truncate=NULL----
kable(ds, row.names=FALSE, digits=0)


## ----eval=FALSE----------------------------------------------------------
## \usepackage{booktabs}


## ----kable_booktabs, results="asis", out.lines=NULL, out.truncate=NULL----
kable(ds, row.names=FALSE, digits=0, booktabs=TRUE)


## ----kable_booktabs_no_midrule, results="asis", out.lines=NULL, out.truncate=NULL----
dst <- weatherAUS[sample(nobs, 20), vars]
kable(dst, row.names=FALSE, digits=0, booktabs=TRUE)


## ----example_table, echo=TRUE, results="asis", out.lines=NULL, out.truncate=NULL----
library(xtable)
xtable(dst)


## ----example_table_norow, echo=TRUE, results="asis", out.lines=NULL, out.truncate=NULL----
print(xtable(ds), include.rownames=FALSE)


## ----table_digits, echo=TRUE, results="asis", out.lines=NULL, out.truncate=NULL----
print(xtable(ds, digits=1), include.rownames=FALSE)


## ----table_large_numbers, results="asis", out.lines=NULL, out.truncate=NULL----
dst     <- ds
dst[-1] <- sample(10000:99999, nrow(dst)) * dst[-1]
print(xtable(dst, digits=0), include.rownames=FALSE)


## ----table_big_mark, results="asis", out.lines=NULL, out.truncate=NULL----
print(xtable(dst, digits=0),
      include.rownames=FALSE,
      format.args=list(big.mark=","))


## ----example_table_caption, echo=TRUE, results="asis", out.lines=NULL, out.truncate=NULL----
print(xtable(ds, 
             digits=0, 
             caption="Selected observations from \\textbf{weatherAUS}."),
      include.rownames=FALSE)


## ----example_table_enclosed, echo=TRUE, results="asis", out.lines=NULL, out.truncate=NULL----
print(xtable(ds, 
             digits=0, 
             caption="Selected observations from \\textbf{weatherAUS}.",
             label="MyTable"),
      include.rownames=FALSE)


## ----example_table_caption_chars, echo=TRUE, results="asis", out.lines=NULL, out.truncate=NULL----
print(xtable(ds, 
             digits=0, 
             caption=paste("Here we include in the caption a sample of \\LaTeX{}",
                 "symbols that can be included in the string, and note that the",
                 "caption string can be the result of R commands, using paste()",
                 "in this instance. Some sample symbols include:",
                 "$\\alpha$ $\\longrightarrow$ $\\wp$.",
                 "We also get a timestamp from R:",
                 Sys.time()),
             label="SymbolCaption"),
      include.rownames=FALSE)


## ----example_figure, eval=FALSE------------------------------------------
## library(rattle)   # For the weatherAUS dataset.
## library(ggplot2)  # To generate a density plot.
## cities <- c("Canberra", "Darwin", "Melbourne", "Sydney")
## ds <- subset(weatherAUS, Location %in% cities & ! is.na(Temp3pm))
## p  <- ggplot(ds, aes(Temp3pm, colour=Location, fill=Location))
## p  <- p + geom_density(alpha=0.55)
## p


## ----example_figure, echo=FALSE------------------------------------------
library(rattle)   # For the weatherAUS dataset.
library(ggplot2)  # To generate a density plot.
cities <- c("Canberra", "Darwin", "Melbourne", "Sydney")
ds <- subset(weatherAUS, Location %in% cities & ! is.na(Temp3pm))
p  <- ggplot(ds, aes(Temp3pm, colour=Location, fill=Location))
p  <- p + geom_density(alpha=0.55)
p


## ----example_figure_fig_width, echo=FALSE, fig.width=14------------------
p


## ----example_figure_fig_width_half, echo=FALSE, fig.height=3.5-----------
p


## ----example_figure_out_width, echo=FALSE, fig.height=3.5, out.width='\\textwidth'----
p


## ----example_figure_out_width_0.9, echo=FALSE, fig.height=3.5, out.width='0.9\\textwidth'----
p


## ----myfigure, fig.cap="The 3pm temperature for four locations.", fig.pos="h", echo=FALSE, fig.height=3.5, out.width='0.9\\textwidth'----
p


## ----eval=FALSE----------------------------------------------------------
## \usepackage{animate}


## ----animate_kmeans, fig.show="animate"----------------------------------
library(animation)
par(mar=c(3, 3, 1, 1.5), mgp=c(1.5, 0.5, 0), bg="white")
cent <- 1.5 * c(1, 1, -1, -1, 1, -1, 1, -1)
x <- NULL
for (i in 1:8) x <- c(x, rnorm(25, mean=cent[i]))
x <- matrix(x, ncol=2)
colnames(x) <- c("X1", "X2")
kmeans.ani(x, centers=4, pch=1:4, col=1:4)


## ----knitr_diagram_flowchart, echo=FALSE---------------------------------
library(diagram)

names   <- c(".Rnw", ".tex", ".pdf")

connect <- c(0,       0,          0,
             "knitr", 0,          0,
             0,       "pdflatex", 0)

M <- matrix(nrow=3, ncol=3, byrow=TRUE, data=connect)

pp <- plotmat(M, pos=c(1, 2), name=names, box.col="orange")


## ----knitr_diagram_flowchart, eval=FALSE---------------------------------
## library(diagram)
## 
## names   <- c(".Rnw", ".tex", ".pdf")
## 
## connect <- c(0,       0,          0,
##              "knitr", 0,          0,
##              0,       "pdflatex", 0)
## 
## M <- matrix(nrow=3, ncol=3, byrow=TRUE, data=connect)
## 
## pp <- plotmat(M, pos=c(1, 2), name=names, box.col="orange")


## ----eval=FALSE----------------------------------------------------------
## write_bib(sub("^.*/", "", grep("^/", searchpaths(), value=TRUE)),
##           file="mydoc.bib")
## 


## ----demo_chunk_ref, rcode=TRUE------------------------------------------
seq(0, 10, 2)


## ----output_hook, eval=FALSE---------------------------------------------
## opts_chunk$set(out.truncate=80)
## hook_output <- knit_hooks$get("output")
## knit_hooks$set(output=function(x, options)
## {
##   if (options$results != "asis")
##   {
##     # Split string into separate lines.
##     x <- unlist(stringr::str_split(x, "\n"))
##     # Truncate each line to length specified.
##     if (!is.null(m <- options$out.truncate))
##     {
##       len <- nchar(x)
##       x[len>m] <- paste0(substr(x[len>m], 0, m-3), "...")
##     }
##     # Paste lines back together.
##     x <- paste(x, collapse="\n")
##     # Continue with any other output hooks
##   }
##   hook_output(x, options)
## })


## ----example_truncate_long_line_null, out.truncate=NULL------------------
paste("This is a very long line that is truncated",
      "at character 80 by default. We change the point",
      "at which it gets truncated using out.truncate=")



## ----example_truncate_long_line------------------------------------------
paste("This is a very long line that is truncated",
      "at character 80 by default. We change the point",
      "at which it gets truncated using out.truncate=")



## ----example_truncate_long_line_40, out.truncate=40----------------------
paste("This is a very long line that is truncated",
      "at character 80 by default. We change the point",
      "at which it gets truncated using out.truncate=")



## ----output_hook_lines, eval=FALSE---------------------------------------
## opts_chunk$set(out.lines=4)
## hook_output <- knit_hooks$get("output")
## knit_hooks$set(output=function(x, options)
## {
##   if (options$results != "asis")
##   {
##     # Split string into separate lines.
##     x <- unlist(stringr::str_split(x, "\n"))
##     # Trim to the number of lines specified.
##     if (!is.null(n <- options$out.lines))
##     {
##       if (length(x) > n)
##       {
##         # Truncate the output.
##         x <- c(head(x, n), "....\n")
##       }
##     }
##     # Paste lines back together.
##     x <- paste(x, collapse="\n")
##   }
##   hook_output(x, options)
## })


## ----example_truncate_many_lines-----------------------------------------
weather[2:8]


## ----example_truncate_many_lines_2, out.lines=2--------------------------
weather[2:8]


## ----source_hook_select_lines, eval=FALSE--------------------------------
## opts_chunk$set(src.top=NULL)
## opts_chunk$set(src.bot=NULL)
## knit_hooks$set(source=function(x, options)
## {
##   # Split string into separate lines.
##   x <- unlist(stringr::str_split(x, "\n"))
##   # Trim to the number of lines specified.
##   if (!is.null(n <- options$src.top))
##   {
##     if (length(x) > n)
##     {
##       # Truncate the output.
##       if (is.null(m <-options$src.bot)) m <- 0
##       x <- c(head(x, n+1), "\n....\n", tail(x, m+2))
##    }
##   }
##   # Paste lines back together.
##   x <- paste(x, collapse="\n")
##   hook_source(x, options)
## })


## ----source_hook_select_lines_eg, src.top=4, src.bot=4, eval=FALSE-------
## opts_chunk$set(src.top=NULL)
## opts_chunk$set(src.bot=NULL)
## knit_hooks$set(source=function(x, options)
## {
##   # Split string into separate lines.
##   x <- unlist(stringr::str_split(x, "\n"))
##   # Trim to the number of lines specified.
##   if (!is.null(n <- options$src.top))
##   {
##     if (length(x) > n)
##     {
##       # Truncate the output.
##       if (is.null(m <-options$src.bot)) m <- 0
##       x <- c(head(x, n), "\n....\n", tail(x, m+2))
##     }
##   }
##   # Paste lines back together.
##   x <- paste(x, collapse="\n")
##   hook_source(x, options)
## })


## ----eval=FALSE----------------------------------------------------------
## opts_chunk$set(size="footnotesize", message=FALSE, tidy=FALSE)


## ----eval=FALSE----------------------------------------------------------
## background="#F7F7F7"            # The background colour of the code chunks.
## cache.path="cache/" 		#
## comment=NA			# Suppresses "\verb|##|" in R output.
## echo=FALSE			# Do not show R commands---just the output.
## echo=3:5			# Only echo lines 3 to 5 of the chunk.
## eval=FALSE 			# Do not run the R code---its just for display.
## eval=2:4 			# Only evaluate lines 2 to 4 of the chunk.
## fig.align="center" 		#
## fig.cap="Caption..." 		#
## fig.keep="high" 		#
## fig.lp="fig:" 			# Prefix for the label assigned to the figure.
## fig.path="figures/plot" 	#
## fig.scap="Short cap."		# For the table of figures in the contents.
## fig.show="animate" 		# Collect figures into an animation.
## fig.show="hold" 		#
## fig.height=9 			# Height of generated figure.
## fig.width=12 			# Width of generated figure.
## include=FALSE 			# Include code but not output/picture.
## message=FALSE 			# Do not display messages from the commands.
## out.height=".6\\textheight"	# Figure takes up 60\% of the page height.
## out.width=".8\\textwidth" 	# Figure takes up 80\% of the page width.
## results="markup" 		# The output from commands will be formatted.
## results="hide" 			# Do not show output from commands.
## results="asis" 			# Retain R command output as \LaTeX{} code.
## size="footnotesize" 		# Useful for Beamer slides.
## tidy=FALSE 			# Retain my own formatting used in the R code.


## ----eval=FALSE----------------------------------------------------------
## out.lines=4                     # Number of lines of \R{} output to show.
## out.truncate=80                 # Truncate \R{} output lines beyond this.
## src.bot=NULL                    # Number of lines of output at top to show.
## src.top=NULL                    # Number of lines of output at bottom to show.


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





