
## ----module, echo=FALSE, results="asis"----------------------------------
Module <- "PlotsO"
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



## ----libraries, message=FALSE--------------------------------------------
library(iplots)
library(ggplot2)
library(tabplot)
library(rattle)
library(dplyr)


## ----additional_dependent_pacakges, echo=FALSE, message=FALSE------------
# These are dependencies that would otherwise be loaded as required.



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





## ------------------------------------------------------------------------
source("http://onepager.togaware.com/dial.plot.R")
dial.plot(label="OnePageR", value=85)


## ------------------------------------------------------------------------
opar <- par(mfrow=c(2,3))
dial.plot(label="Temperature", label.cex=1, value=85, value.cex=2)
dial.plot(label="Humidity", label.cex=1, value=5, value.cex=2)
dial.plot(label="Sunshine", label.cex=1, value=65, value.cex=2)
dial.plot(label="Rainfull", label.cex=1, value=0, value.cex=2)
dial.plot(label="Electricity\nGenerated", label.cex=1, value=55, value.cex=2)
dial.plot(label="Electricity\nConsumed", label.cex=1, value=40, value.cex=2)


## ----echo=FALSE----------------------------------------------------------
par <- opar


## ----fig.width=9, out.width="\\textwidth", warning=FALSE-----------------
library(tabplot)
tableplot(weather, select=c(MinTemp:WindDir3pm, RainTomorrow))


## ----fig.width=9, out.width="\\textwidth", warning=FALSE-----------------
library(tabplot)
tableplot(weather, select=c(Humidity9am:RainToday, RainTomorrow), sortCol=Pressure3pm)


## ----visually_weighted_regression, echo=FALSE, message=FALSE, eval=FALSE----
## # B = number bootstrapped smoothers
## # shade: plot the shaded confidence region?
## # spag: plot spaghetti lines?
## # mweight: should the median smoother be visually weighted?
## # show.lm: should the linear regresison line be plotted?
## # show.CI: should the 95% CI limits be plotted?
## vwReg <- function(formula, data, B=1000, shade=TRUE,
##                   spag=FALSE, mweight=TRUE, show.lm=FALSE,
##                   show.CI=FALSE)
## {
##   IV <- all.vars(formula)[2]
##   DV <- all.vars(formula)[1]
##   data <- na.omit(data[order(data[, IV]), c(IV, DV)])
## 
##   # print("Computing boostrapped smoothers ...")
##   steps <- nrow(data)*3	# three times more prediction points
##                         # on the x-axis than original data points -
##                         # for smoother smoothers
##   newx <- seq(min(data[, IV]), max(data[, IV]), length=steps)
##   l0.boot <- matrix(NA, nrow=steps, ncol=B)
##   for (i in 1:B)
##   {
##     data2 <- data[sample(nrow(data), replace=TRUE), ]
##     data2 <- data2[order(data2[, IV]), ]
##     l0.boot[, i] <- predict(loess(formula, data2), newdata=newx)
##   }
## 
##   # compute median and CI limits of bootstrap
##   library(plyr)
##   CI.boot <- adply(l0.boot, 1, function(x)
##                    quantile(x, prob=c(.025, .5, .975), na.rm=TRUE))[, -1]
##   colnames(CI.boot)[1:3] <- c("LL", "M", "UL")
##   CI.boot$x <- newx
##   CI.boot$width <- CI.boot$UL - CI.boot$LL
## 
##   # scale the CI width to the range 0 to 1 and flip it (bigger numbers = narrower CI)
##   CI.boot$w2 <- (CI.boot$width - min(CI.boot$width))
##   CI.boot$w3 <- 1-(CI.boot$w2/max(CI.boot$w2))
## 
## 
##   # convert bootstrapped spaghettis to long format
##   require(reshape2)
##   b2 <- melt(l0.boot)
##   b2$x <- newx
##   colnames(b2) <- c("index", "B", "value", "x")
## 
##   # print("ggplot prints the figure ...")
##   library(ggplot2)
##   library(RColorBrewer)
## 
##   p1 <- ggplot(data, aes_string(x=IV, y=DV)) + theme_bw()
## 
## 
##   if (shade == TRUE)
##   {
##     #print("Computing density estimates for each vertical cut ...")
##     # vertical cross-sectional density estimate
##     d2 <- ddply(b2[, c("x", "value")], .(x), function(df) {
##       res <- data.frame(density(df$value, na.rm=TRUE, n=100)[c("x", "y")])
##       colnames(res) <- c("y", "dens")
##       return(res)
##     })
## 
##     maxdens <- max(d2$dens)
##     mindens <- min(d2$dens)
##     d2$dens.scaled <- (d2$dens - mindens)/maxdens	
## 
##     # alpha scaling
##     #p1 <- p1 + geom_point(data=d2, aes(x=x, y=y, alpha=dens.scaled), size=0.4, color="red")
## 
##     # color scaling
##     p1 <- p1 + geom_point(data=d2, aes(x=x, y=y, color=dens.scaled), size=1.4) + scale_color_gradientn("dens.scaled", colours=brewer.pal(9, "YlOrRd"))
##   }
## 
##   if (spag==TRUE) {
##     library(reshape2)
##     p1 <- p1 + geom_path(data=b2, aes(x=x, y=value, group=B), size=0.7, alpha=15/B, color="darkblue")
##   }
## 
##   if (mweight == TRUE) {
##     p1 <- p1 + geom_point(data=CI.boot, aes(x=x, y=M, alpha=w3), size=1, linejoin="mitre", color="darkred")
##   } else {
##     p1 <- p1 + geom_path(data=CI.boot, aes(x=x, y=M), size = 0.3, linejoin="mitre", color="darkred")
##   }
## 
##   # Confidence limits
##   if (show.CI == TRUE) {
##     p1 <- p1 + geom_path(data=CI.boot, aes(x=x, y=UL, group=B), size=1, color="red")
##     p1 <- p1 + geom_path(data=CI.boot, aes(x=x, y=LL, group=B), size=1, color="red")
##   }
## 
##   p1 <- p1 + geom_point(size=1)
## 
##   # plain linear regression line
##   if (show.lm==TRUE) {p1 <- p1 + geom_smooth(method="lm", color="darkgreen", se=FALSE)}
##   p1
## }
## 
## # build a demo data set
## set.seed(1)
## x <- rnorm(200, 0.8, 1.2)
## e <- rnorm(200, 0, 2)*(abs(x)^1.5 + .5)
## y <- 8*x - x^3 + e
## df <- data.frame(x, y)
## 
## vwReg(y~x, df)
## #vwReg(y~x, df, shade=FALSE, spag=TRUE)
## #vwReg(y~x, df, shade=TRUE, spag=FALSE, mweight=TRUE, show.CI=TRUE, show.lm=TRUE)
## #vwReg(y~x, df, shade=FALSE, spag=TRUE, show.lm=TRUE)


## ----load_f1-------------------------------------------------------------
(load("data/f1.RData"))


## ----head_f1-------------------------------------------------------------
head(f1)


## ----f1_race_track-------------------------------------------------------
p <- ggplot(f1, aes(NGPSLongitude, NGPSLatitude))
p <- p +  geom_point(aes(col=sign(gLat), size=abs(gLat)))
p


## ----f1_behaviour--------------------------------------------------------
library(scales)
p <- ggplot(f1, aes(sLap, Lap))
p <- p + geom_point(aes(col=rThrottlePedal, size=-NGear))
p <- p + scale_colour_gradient(low="blue", high="red")
p <- p + scale_x_continuous(labels=comma)
p


## ----f1_gears_track------------------------------------------------------
p <- ggplot(f1, aes(sLap, NGear))
p <- p + geom_line()
p


## ----f1_single_lap-------------------------------------------------------
ggplot(subset(f1, Lap==2), aes(sLap, vCar)) +
  geom_line(aes(colour=NGear))


## ----f1_speed_box_plot---------------------------------------------------
ggplot(f1, aes(factor(NGear), vCar)) + 
  geom_boxplot()


## ----f1_footwork---------------------------------------------------------
ggplot(f1, aes(factor(NGear))) + 
  geom_jitter(aes(y=rThrottlePedal), colour='darkgreen') + 
  geom_jitter(aes(y=pBrakeF), colour='darkred')


## ----f1_forces-----------------------------------------------------------
ggplot(f1, aes(factor(NGear), gLong)) + 
  geom_jitter(aes(col=pBrakeF)) + 
  scale_colour_gradient(low='red', high='green')


## ----f1_more_forces------------------------------------------------------
ggplot(f1, aes(factor(NGear), gLong)) + 
  geom_jitter(aes(col=rThrottlePedal)) +
  scale_colour_gradient(low='red', high="green")


## ----f1_forces_boxplot---------------------------------------------------
ggplot(f1, aes(factor(NGear), gLong)) + 
  geom_boxplot() + 
  geom_jitter(size=1)


## ----f1_rpm_speed_gear---------------------------------------------------
ggplot(f1, aes(nEngine, vCar)) +
  geom_point(aes(col=factor(NGear)))


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





