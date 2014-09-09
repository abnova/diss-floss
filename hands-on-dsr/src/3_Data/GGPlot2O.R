
## ----module, echo=FALSE, results="asis"----------------------------------
Module <- "GGPlot2O"
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



## ----libraries, message=FALSE, warning=FALSE-----------------------------
library(ggplot2)         # Visualise data.
library(scales)          # Include commas in numbers.
library(rattle)          # Weather dataset.
library(randomForest)    # Use na.roughfix() to deal with missing data.
library(gridExtra)       # Layout multiple plots.
library(wq)              # Regular grid layout.
library(xkcd)            # Some xkcd fun.
library(extrafont)       # Fonts for xkcd.
library(GGally)          # Parallel coordinates.
library(dplyr)           # Data manipulation.


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




## ----prepare_dataset-----------------------------------------------------
library(rattle)
dsname <- "weatherAUS"
ds     <- dsname %>% get() %>% tbl_df()


## ----dataset_summary-----------------------------------------------------
dim(ds)
names(ds)
head(ds)
tail(ds)
str(ds)
summary(ds)


## ----message=FALSE-------------------------------------------------------
names(ds)  <- normVarNames(names(ds)) # Optional lower case variable names.
vars       <- names(ds)
target     <- "rain_tomorrow"
id         <- c("date", "location")
ignore     <- id
inputs     <- setdiff(vars, target)
numi       <- which(sapply(ds[vars], is.numeric))
numi
numerics   <- names(numi)
numerics
cati       <- which(sapply(ds[vars], is.factor))
cati
categorics <- names(cati)
categorics


## ----impute_missing_values-----------------------------------------------
library(randomForest)
sum(is.na(ds))
ds[setdiff(vars, ignore)] <- na.roughfix(ds[setdiff(vars, ignore)])
sum(is.na(ds))


## ----scatter, echo=FALSE, fig.height=5-----------------------------------
sobs <- sample(nrow(ds), 1000)

ds[sobs,]                                                             %>%
  ggplot(aes(x=min_temp, y=max_temp, colour=rain_tomorrow))            +
  geom_point()


## ----scatter, eval=FALSE-------------------------------------------------
## sobs <- sample(nrow(ds), 1000)
## 
## ds[sobs,]                                                             %>%
##   ggplot(aes(x=min_temp, y=max_temp, colour=rain_tomorrow))            +
##   geom_point()


## ----scatter_temp_changes_over_time, echo=FALSE--------------------------
ds[sobs,]                                                             %>%
  ggplot(aes(x=date, y=max_temp))                                      +
  geom_point()                                                         +
  geom_smooth(method="gam", formula=y~s(x, bs="cs"))


## ----scatter_temp_changes_over_time, eval=FALSE--------------------------
## ds[sobs,]                                                             %>%
##   ggplot(aes(x=date, y=max_temp))                                      +
##   geom_point()                                                         +
##   geom_smooth(method="gam", formula=y~s(x, bs="cs"))


## ----temp_changes_over_time, echo=FALSE, fig.height=8, fig.width=10, out.width="\\textwidth"----
ds[sobs,]                                                             %>%
  ggplot(aes(x=date, y=max_temp))                                      +
  geom_point()                                                         +
  geom_smooth(method="gam", formula=y~s(x, bs="cs"))                   +
  facet_wrap(~location)                                                +
  theme(axis.text.x=element_text(angle=45, hjust=1))


## ----temp_changes_over_time, eval=FALSE----------------------------------
## ds[sobs,]                                                             %>%
##   ggplot(aes(x=date, y=max_temp))                                      +
##   geom_point()                                                         +
##   geom_smooth(method="gam", formula=y~s(x, bs="cs"))                   +
##   facet_wrap(~location)                                                +
##   theme(axis.text.x=element_text(angle=45, hjust=1))


## ----temp_changes_over_time_point, echo=FALSE, fig.height=8, fig.width=10, out.width="\\textwidth"----
ds[sobs,]                                                             %>%
  ggplot(aes(x=date, y=max_temp))                                      +
  geom_point(size=0.2)                                                 +
  geom_smooth(method="gam", formula=y~s(x, bs="cs"))                   +
  facet_wrap(~location)                                                +
  theme(axis.text.x=element_text(angle=45, hjust=1))


## ----temp_changes_over_time_point, eval=FALSE----------------------------
## ds[sobs,]                                                             %>%
##   ggplot(aes(x=date, y=max_temp))                                      +
##   geom_point(size=0.2)                                                 +
##   geom_smooth(method="gam", formula=y~s(x, bs="cs"))                   +
##   facet_wrap(~location)                                                +
##   theme(axis.text.x=element_text(angle=45, hjust=1))


## ----temp_changes_over_time_line, echo=FALSE, fig.height=8, fig.width=10, out.width="\\textwidth"----
ds[sobs,]                                                             %>%
  ggplot(aes(x=date, y=max_temp))                                      +
  geom_line()                                                          +
  geom_smooth(method="gam", formula=y~s(x, bs="cs"))                   +
  facet_wrap(~location)                                                +
  theme(axis.text.x=element_text(angle=45, hjust=1))


## ----temp_changes_over_time_line, eval=FALSE-----------------------------
## ds[sobs,]                                                             %>%
##   ggplot(aes(x=date, y=max_temp))                                      +
##   geom_line()                                                          +
##   geom_smooth(method="gam", formula=y~s(x, bs="cs"))                   +
##   facet_wrap(~location)                                                +
##   theme(axis.text.x=element_text(angle=45, hjust=1))


## ----temp_changes_over_time_line_thin, echo=FALSE, fig.height=8, fig.width=10, out.width="\\textwidth"----
ds[sobs,]                                                             %>%
  ggplot(aes(x=date, y=max_temp))                                      +
  geom_line(size=0.1)                                                  +
  geom_smooth(method="gam", formula=y~s(x, bs="cs"))                   +
  facet_wrap(~location)                                                +
  theme(axis.text.x=element_text(angle=45, hjust=1))


## ----temp_changes_over_time_line_thin, eval=FALSE------------------------
## ds[sobs,]                                                             %>%
##   ggplot(aes(x=date, y=max_temp))                                      +
##   geom_line(size=0.1)                                                  +
##   geom_smooth(method="gam", formula=y~s(x, bs="cs"))                   +
##   facet_wrap(~location)                                                +
##   theme(axis.text.x=element_text(angle=45, hjust=1))


## ----frequency_barchart, echo=FALSE--------------------------------------
p <- ds                                                               %>%
  ggplot(aes(x=wind_dir_3pm))                                          +
  geom_bar()
p


## ----frequency_barchart, eval=FALSE--------------------------------------
## p <- ds                                                               %>%
##   ggplot(aes(x=wind_dir_3pm))                                          +
##   geom_bar()
## p


## ----eval=FALSE----------------------------------------------------------
## ggsave("barchart.pdf", width=11, height=7)


## ----eval=FALSE----------------------------------------------------------
## pdf("barchart.pdf", width=11, height=7)
## p
## dev.off()


## ----bar_chart, echo=FALSE-----------------------------------------------
ds                                                                    %>%
  ggplot(aes(x=wind_dir_3pm))                                          +
  geom_bar()


## ----bar_chart, eval=FALSE-----------------------------------------------
## ds                                                                    %>%
##   ggplot(aes(x=wind_dir_3pm))                                          +
##   geom_bar()


## ----stacked_bar_chart, echo=FALSE---------------------------------------
ds                                                                    %>%
  ggplot(aes(x=wind_dir_3pm, fill=rain_tomorrow))                      +
  geom_bar()


## ----stacked_bar_chart, eval=FALSE---------------------------------------
## ds                                                                    %>%
##   ggplot(aes(x=wind_dir_3pm, fill=rain_tomorrow))                      +
##   geom_bar()


## ----dodged_bar_chart, echo=FALSE----------------------------------------
ds                                                                    %>%
  ggplot(aes(x=wind_dir_3pm, fill=rain_tomorrow))                      +
  geom_bar(position="dodge")


## ----dodged_bar_chart, eval=FALSE----------------------------------------
## ds                                                                    %>%
##   ggplot(aes(x=wind_dir_3pm, fill=rain_tomorrow))                      +
##   geom_bar(position="dodge")


## ----frequency_barchart_narrow, echo=FALSE-------------------------------
ds                                                                    %>%
  ggplot(aes(wind_dir_3pm))                                            +
  geom_bar(width=0.5)


## ----frequency_barchart_narrow, eval=FALSE-------------------------------
## ds                                                                    %>%
##   ggplot(aes(wind_dir_3pm))                                            +
##   geom_bar(width=0.5)


## ----frequency_barchart_wide, echo=FALSE---------------------------------
ds                                                                    %>%
  ggplot(aes(wind_dir_3pm))                                            +
  geom_bar(width=1)


## ----frequency_barchart_wide, eval=FALSE---------------------------------
## ds                                                                    %>%
##   ggplot(aes(wind_dir_3pm))                                            +
##   geom_bar(width=1)


## ----frequency_barchart_wide_border, echo=FALSE--------------------------
ds                                                                    %>% 
  ggplot(aes(wind_dir_3pm))                                            +
  geom_bar(width=1, colour="blue", fill="grey")


## ----frequency_barchart_wide_border, eval=FALSE--------------------------
## ds                                                                    %>%
##   ggplot(aes(wind_dir_3pm))                                            +
##   geom_bar(width=1, colour="blue", fill="grey")


## ----frequency_barchart_colour, echo=FALSE-------------------------------
ds                                                                    %>%
  ggplot(aes(wind_dir_3pm, fill=wind_dir_3pm))                         +
  geom_bar()                                                           +
  theme(legend.position="none")


## ----frequency_barchart_colour, eval=FALSE-------------------------------
## ds                                                                    %>%
##   ggplot(aes(wind_dir_3pm, fill=wind_dir_3pm))                         +
##   geom_bar()                                                           +
##   theme(legend.position="none")


## ----frequency_barchart_colour_comma, echo=FALSE-------------------------
ds                                                                    %>%
  ggplot(aes(wind_dir_3pm, fill=wind_dir_3pm))                         +
  geom_bar()                                                           +
  scale_y_continuous(labels=comma)                                     +
  theme(legend.position="none")


## ----frequency_barchart_colour_comma, eval=FALSE-------------------------
## ds                                                                    %>%
##   ggplot(aes(wind_dir_3pm, fill=wind_dir_3pm))                         +
##   geom_bar()                                                           +
##   scale_y_continuous(labels=comma)                                     +
##   theme(legend.position="none")


## ----frequency_barchart_colour_dollar, echo=FALSE------------------------
ds                                                                    %>%
  ggplot(aes(wind_dir_3pm, fill=wind_dir_3pm))                         +
  geom_bar()                                                           +
  scale_y_continuous(labels=dollar)                                    +
  labs(y="")                                                           +
  theme(legend.position="none")


## ----frequency_barchart_colour_dollar, eval=FALSE------------------------
## ds                                                                    %>%
##   ggplot(aes(wind_dir_3pm, fill=wind_dir_3pm))                         +
##   geom_bar()                                                           +
##   scale_y_continuous(labels=dollar)                                    +
##   labs(y="")                                                           +
##   theme(legend.position="none")


## ----mean_temp3pm_location, echo=FALSE, warning=FALSE--------------------
ds                                                                    %>%
  ggplot(aes(x=location, y=temp_3pm, fill=location))                   +
  stat_summary(fun.y="mean", geom="bar")                               +
  theme(legend.position="none")


## ----mean_temp3pm_location, eval=FALSE-----------------------------------
## ds                                                                    %>%
##   ggplot(aes(x=location, y=temp_3pm, fill=location))                   +
##   stat_summary(fun.y="mean", geom="bar")                               +
##   theme(legend.position="none")


## ----mean_temp3pm_location_rotated_labels, echo=FALSE, warning=FALSE-----
ds                                                                    %>%
  ggplot(aes(location, temp_3pm, fill=location))                       +
  stat_summary(fun.y="mean", geom="bar")                               +
  theme(legend.position="none", 
        axis.text.x=element_text(angle=90))


## ----mean_temp3pm_location_rotated_labels, eval=FALSE--------------------
## ds                                                                    %>%
##   ggplot(aes(location, temp_3pm, fill=location))                       +
##   stat_summary(fun.y="mean", geom="bar")                               +
##   theme(legend.position="none",
##         axis.text.x=element_text(angle=90))


## ----mean_temp3pm_location_coord_flip, echo=FALSE, warning=FALSE---------
ds                                                                    %>%
  ggplot(aes(location, temp_3pm, fill=location))                       +
  stat_summary(fun.y="mean", geom="bar")                               +
  theme(legend.position="none")                                        +
  coord_flip()


## ----mean_temp3pm_location_coord_flip, eval=FALSE------------------------
## ds                                                                    %>%
##   ggplot(aes(location, temp_3pm, fill=location))                       +
##   stat_summary(fun.y="mean", geom="bar")                               +
##   theme(legend.position="none")                                        +
##   coord_flip()


## ----mean_temp3pm_location_coord_flip_reorder, echo=FALSE, warning=FALSE----
ds                                                                    %>%
  mutate(location=factor(location, levels=rev(levels(location))))     %>%
  ggplot(aes(location, temp_3pm, fill=location))                       +
  stat_summary(fun.y="mean", geom="bar")                               +
  theme(legend.position="none")                                        +
  coord_flip()


## ----mean_temp3pm_location_coord_flip_reorder, eval=FALSE----------------
## ds                                                                    %>%
##   mutate(location=factor(location, levels=rev(levels(location))))     %>%
##   ggplot(aes(location, temp_3pm, fill=location))                       +
##   stat_summary(fun.y="mean", geom="bar")                               +
##   theme(legend.position="none")                                        +
##   coord_flip()


## ----mean_temp3pm_location_flip_coords_ci, echo=FALSE, warning=FALSE-----
ds                                                                    %>%
  mutate(location=factor(location, levels=rev(levels(location))))     %>%
  ggplot(aes(location, temp_3pm, fill=location))                       +
  stat_summary(fun.y="mean", geom="bar")                               +
  stat_summary(fun.data="mean_cl_normal", geom="errorbar", 
               conf.int=0.95, width=0.35)                              +
  theme(legend.position="none")                                        +
  coord_flip()


## ----mean_temp3pm_location_flip_coords_ci, eval=FALSE--------------------
## ds                                                                    %>%
##   mutate(location=factor(location, levels=rev(levels(location))))     %>%
##   ggplot(aes(location, temp_3pm, fill=location))                       +
##   stat_summary(fun.y="mean", geom="bar")                               +
##   stat_summary(fun.data="mean_cl_normal", geom="errorbar",
##                conf.int=0.95, width=0.35)                              +
##   theme(legend.position="none")                                        +
##   coord_flip()


## ----mean_temp3pm_location_coord_flip_labels, echo=FALSE-----------------
ds                                                                    %>%
  mutate(location=factor(location, levels=rev(levels(location))))     %>%
  ggplot(aes(location, fill=location))                                 +
  geom_bar(width=1, colour="white")                                    +
  theme(legend.position="none")                                        +
  coord_flip()                                                         +
  geom_text(stat="bin", color="white", hjust=1.0, size=3,
            aes(y=..count.., label=..count..))


## ----mean_temp3pm_location_coord_flip_labels, eval=FALSE-----------------
## ds                                                                    %>%
##   mutate(location=factor(location, levels=rev(levels(location))))     %>%
##   ggplot(aes(location, fill=location))                                 +
##   geom_bar(width=1, colour="white")                                    +
##   theme(legend.position="none")                                        +
##   coord_flip()                                                         +
##   geom_text(stat="bin", color="white", hjust=1.0, size=3,
##             aes(y=..count.., label=..count..))


## ----mean_temp3pm_location_coord_flip_labels_commas, echo=FALSE----------
ds                                                                    %>%
  mutate(location=factor(location, levels=rev(levels(location))))     %>%
  ggplot(aes(location, fill=location))                                 +
  geom_bar(width=1, colour="white")                                    +
  theme(legend.position="none")                                        +
  coord_flip()                                                         +
  geom_text(stat="bin", color="white", hjust=1.0, size=3,
            aes(y=..count.., label=scales::comma(..count..)))


## ----mean_temp3pm_location_coord_flip_labels_commas, eval=FALSE----------
## ds                                                                    %>%
##   mutate(location=factor(location, levels=rev(levels(location))))     %>%
##   ggplot(aes(location, fill=location))                                 +
##   geom_bar(width=1, colour="white")                                    +
##   theme(legend.position="none")                                        +
##   coord_flip()                                                         +
##   geom_text(stat="bin", color="white", hjust=1.0, size=3,
##             aes(y=..count.., label=scales::comma(..count..)))


## ----frequency_maxtemp, echo=FALSE---------------------------------------
ds                                                                    %>%
  ggplot(aes(x=max_temp))                                              +
  geom_density()


## ----frequency_maxtemp, eval=FALSE---------------------------------------
## ds                                                                    %>%
##   ggplot(aes(x=max_temp))                                              +
##   geom_density()


## ----frequency_rainfall, echo=FALSE--------------------------------------
ds                                                                    %>%
  filter(rainfall != 0)                                               %>%
  ggplot(aes(x=rainfall))                                              +
  geom_density()                                                       +
  scale_y_continuous(labels=comma)                                     +
  theme(legend.position="none")


## ----frequency_rainfall, eval=FALSE--------------------------------------
## ds                                                                    %>%
##   filter(rainfall != 0)                                               %>%
##   ggplot(aes(x=rainfall))                                              +
##   geom_density()                                                       +
##   scale_y_continuous(labels=comma)                                     +
##   theme(legend.position="none")


## ----frequency_rainfall_log_x, echo=FALSE--------------------------------
ds                                                                    %>%
  filter(rainfall != 0)                                               %>%
  ggplot(aes(x=rainfall))                                              +
  geom_density()                                                       +
  scale_x_log10()                                                      +
  theme(legend.position="none")


## ----frequency_rainfall_log_x, eval=FALSE--------------------------------
## ds                                                                    %>%
##   filter(rainfall != 0)                                               %>%
##   ggplot(aes(x=rainfall))                                              +
##   geom_density()                                                       +
##   scale_x_log10()                                                      +
##   theme(legend.position="none")


## ----frequency_rainfall_log_x_breaks, echo=FALSE-------------------------
ds                                                                    %>%
  filter(rainfall != 0)                                               %>%
  ggplot(aes(x=rainfall))                                              +
  geom_density(binwidth=0.1)                                           +
  scale_x_log10(breaks=c(1, 10, 100), labels=comma)                    +
  theme(legend.position="none")


## ----frequency_rainfall_log_x_breaks, eval=FALSE-------------------------
## ds                                                                    %>%
##   filter(rainfall != 0)                                               %>%
##   ggplot(aes(x=rainfall))                                              +
##   geom_density(binwidth=0.1)                                           +
##   scale_x_log10(breaks=c(1, 10, 100), labels=comma)                    +
##   theme(legend.position="none")


## ----frequency_rainfall_log_x_ticks, echo=FALSE--------------------------
ds                                                                    %>%
  filter(rainfall != 0)                                               %>%
  ggplot(aes(x=rainfall))                                              +
  geom_density(binwidth=0.1)                                           +
  scale_x_log10(breaks=c(1, 10, 100), labels=comma)                    +
  annotation_logticks(sides="bt")                                      +
  theme(legend.position="none")


## ----frequency_rainfall_log_x_ticks, eval=FALSE--------------------------
## ds                                                                    %>%
##   filter(rainfall != 0)                                               %>%
##   ggplot(aes(x=rainfall))                                              +
##   geom_density(binwidth=0.1)                                           +
##   scale_x_log10(breaks=c(1, 10, 100), labels=comma)                    +
##   annotation_logticks(sides="bt")                                      +
##   theme(legend.position="none")


## ----frequency_rainfall_log_x_label, echo=FALSE--------------------------
mm <- function(x) { sprintf("%smm", x) }

ds                                                                    %>%
  filter(rainfall != 0)                                               %>%
  ggplot(aes(x=rainfall))                                              +
  geom_density()                                                       +
  scale_x_log10(breaks=c(1, 10, 100), labels=mm)                       +
  annotation_logticks(sides="bt")                                      +
  theme(legend.position="none")


## ----frequency_rainfall_log_x_label, eval=FALSE--------------------------
## mm <- function(x) { sprintf("%smm", x) }
## 
## ds                                                                    %>%
##   filter(rainfall != 0)                                               %>%
##   ggplot(aes(x=rainfall))                                              +
##   geom_density()                                                       +
##   scale_x_log10(breaks=c(1, 10, 100), labels=mm)                       +
##   annotation_logticks(sides="bt")                                      +
##   theme(legend.position="none")


## ----density_temp3pm_cities, echo=FALSE----------------------------------
cities <- c("Canberra", "Darwin", "Melbourne", "Sydney")

ds                                                                    %>%
  filter(location %in% cities)                                        %>%
  ggplot(aes(temp_3pm, colour = location, fill = location))            +
  geom_density(alpha = 0.55)


## ----density_temp3pm_cities, eval=FALSE----------------------------------
## cities <- c("Canberra", "Darwin", "Melbourne", "Sydney")
## 
## ds                                                                    %>%
##   filter(location %in% cities)                                        %>%
##   ggplot(aes(temp_3pm, colour = location, fill = location))            +
##   geom_density(alpha = 0.55)


## ----box_plot, echo=FALSE, fig.width=10, out.width='\\textwidth'---------
ds                                                                    %>%
  mutate(year=factor(format(ds$date, "%Y")))                          %>%
  ggplot(aes(x=year, y=max_temp, fill=year))                           +
  geom_boxplot(notch=TRUE)                                             +
  theme(legend.position="none")


## ----box_plot, eval=FALSE------------------------------------------------
## ds                                                                    %>%
##   mutate(year=factor(format(ds$date, "%Y")))                          %>%
##   ggplot(aes(x=year, y=max_temp, fill=year))                           +
##   geom_boxplot(notch=TRUE)                                             +
##   theme(legend.position="none")


## ----violin_plot, echo=FALSE, fig.width=10, out.width='\\textwidth'------
ds                                                                    %>%
  mutate(year=factor(format(ds$date, "%Y")))                          %>%
  ggplot(aes(x=year, y=max_temp, fill=year))                           +
  geom_violin()                                                        +
  theme(legend.position="none")


## ----violin_plot, eval=FALSE---------------------------------------------
## ds                                                                    %>%
##   mutate(year=factor(format(ds$date, "%Y")))                          %>%
##   ggplot(aes(x=year, y=max_temp, fill=year))                           +
##   geom_violin()                                                        +
##   theme(legend.position="none")


## ----violin_box_plot, echo=FALSE, fig.width=10, out.width='\\textwidth'----
ds                                                                    %>%
  mutate(year=factor(format(ds$date, "%Y")))                          %>%
  ggplot(aes(x=year, y=max_temp, fill=year))                           +
  geom_violin()                                                        +
  geom_boxplot(width=.5, position=position_dodge(width=0))             +
  theme(legend.position="none")


## ----violin_box_plot, eval=FALSE-----------------------------------------
## ds                                                                    %>%
##   mutate(year=factor(format(ds$date, "%Y")))                          %>%
##   ggplot(aes(x=year, y=max_temp, fill=year))                           +
##   geom_violin()                                                        +
##   geom_boxplot(width=.5, position=position_dodge(width=0))             +
##   theme(legend.position="none")


## ----violin_box_plot_location, echo=FALSE, fig.width=10, out.width='\\textwidth'----
ds                                                                    %>%
  mutate(year=factor(format(ds$date, "%Y")))                          %>%
  ggplot(aes(x=year, y=max_temp, fill=year))                           +
  geom_violin()                                                        +
  geom_boxplot(width=.5, position=position_dodge(width=0))             +
  theme(legend.position="none",
        axis.text.x=element_text(angle=45, hjust=1))                   +
  facet_wrap(~location)


## ----violin_box_plot_location, eval=FALSE--------------------------------
## ds                                                                    %>%
##   mutate(year=factor(format(ds$date, "%Y")))                          %>%
##   ggplot(aes(x=year, y=max_temp, fill=year))                           +
##   geom_violin()                                                        +
##   geom_boxplot(width=.5, position=position_dodge(width=0))             +
##   theme(legend.position="none",
##         axis.text.x=element_text(angle=45, hjust=1))                   +
##   facet_wrap(~location)


## ----ggpairs, echo=FALSE, fig.width=12, fig.height=12, out.width='\\textwidth'----
weather[c(3,4,6,7,10,19,22,24)]                                       %>%
  na.omit()                                                           %>%
  ggpairs(params=c(shape=I("."), outlier.shape=I(".")))


## ----ggpairs_echo, eval=FALSE, ref.label="ggpairs"-----------------------
## NA


## ----cumsum_plot, echo=FALSE, message=FALSE, fig.height=5----------------
cities <- c("Adelaide", "Canberra", "Darwin", "Hobart")

ds                                                                    %>% 
  filter(location %in% cities & date >= "2009-01-01")                 %>% 
  group_by(location)                                                  %>% 
  mutate(cumRainfall=order_by(date, cumsum(rainfall)))                %>%
  ggplot(aes(x=date, y=cumRainfall, colour=location))                  +
  geom_line()                                                          +
  ylab("millimetres")                                                  +
  scale_y_continuous(labels=comma)                                     +
  ggtitle("Cumulative Rainfall")


## ----cumsum_plot, eval=FALSE---------------------------------------------
## cities <- c("Adelaide", "Canberra", "Darwin", "Hobart")
## 
## ds                                                                    %>%
##   filter(location %in% cities & date >= "2009-01-01")                 %>%
##   group_by(location)                                                  %>%
##   mutate(cumRainfall=order_by(date, cumsum(rainfall)))                %>%
##   ggplot(aes(x=date, y=cumRainfall, colour=location))                  +
##   geom_line()                                                          +
##   ylab("millimetres")                                                  +
##   scale_y_continuous(labels=comma)                                     +
##   ggtitle("Cumulative Rainfall")


## ----parallel_coordinates, echo=FALSE, fig.height=5, out.width="\\textwidth"----
library(GGally)

cities <- c("Canberra", "Darwin", "Melbourne", "Sydney")

ds                                                                    %>%
  filter(location %in% cities & rainfall>75)                          %>%
  ggparcoord(columns=numi)                                             +
  theme(axis.text.x=element_text(angle=45))


## ----parallel_coordinates, eval=FALSE------------------------------------
## library(GGally)
## 
## cities <- c("Canberra", "Darwin", "Melbourne", "Sydney")
## 
## ds                                                                    %>%
##   filter(location %in% cities & rainfall>75)                          %>%
##   ggparcoord(columns=numi)                                             +
##   theme(axis.text.x=element_text(angle=45))


## ----parallel_coordinates_hjust, echo=FALSE, fig.height=5, out.width="\\textwidth"----
ds                                                                    %>%
  filter(location %in% cities & rainfall>75)                          %>%
  ggparcoord(columns=numi)                                             +
  theme(axis.text.x=element_text(angle=45, hjust=1))


## ----parallel_coordinates_hjust, eval=FALSE------------------------------
## ds                                                                    %>%
##   filter(location %in% cities & rainfall>75)                          %>%
##   ggparcoord(columns=numi)                                             +
##   theme(axis.text.x=element_text(angle=45, hjust=1))


## ----parallel_coordinates_group, echo=FALSE, fig.height=5, out.width="\\textwidth"----
ds                                                                    %>%
  filter(location %in% cities & rainfall>75)                          %>%
  ggparcoord(columns=numi, group="location")                           +
  theme(axis.text.x=element_text(angle=45, hjust=1),
        legend.position=c(0.25, 1),
        legend.justification=c(0.0, 1.0),
        legend.direction="horizontal",
        legend.background=element_rect(fill="transparent"),
        legend.key=element_rect(fill="transparent", 
            colour="transparent"))


## ----parallel_coordinates_group, eval=FALSE------------------------------
## ds                                                                    %>%
##   filter(location %in% cities & rainfall>75)                          %>%
##   ggparcoord(columns=numi, group="location")                           +
##   theme(axis.text.x=element_text(angle=45, hjust=1),
##         legend.position=c(0.25, 1),
##         legend.justification=c(0.0, 1.0),
##         legend.direction="horizontal",
##         legend.background=element_rect(fill="transparent"),
##         legend.key=element_rect(fill="transparent",
##             colour="transparent"))


## ----polar_coords, echo=FALSE--------------------------------------------
ds                                                                    %>%
  mutate(month=format(date, "%b"))                                    %>%
  group_by(month)                                                     %>%
  summarise(max=max(max_temp))                                        %>%
  mutate(month=factor(month, levels=month.abb))                       %>%
  ggplot(aes(x=month, y=max, group=1))                                 +
  geom_bar(width=1, stat="identity", fill="orange", color="black")     +
  coord_polar(theta="x", start=-pi/12)


## ----polar_coords, eval=FALSE--------------------------------------------
## ds                                                                    %>%
##   mutate(month=format(date, "%b"))                                    %>%
##   group_by(month)                                                     %>%
##   summarise(max=max(max_temp))                                        %>%
##   mutate(month=factor(month, levels=month.abb))                       %>%
##   ggplot(aes(x=month, y=max, group=1))                                 +
##   geom_bar(width=1, stat="identity", fill="orange", color="black")     +
##   coord_polar(theta="x", start=-pi/12)


## ----xkcd_plot, echo=FALSE, fig.height=4, warning=FALSE, message=FALSE----
library(xkcd)
xrange <- range(mtcars$mpg)
yrange <- range(mtcars$wt)
mtcars                                                                %>%
  ggplot(aes(mpg, wt))                                                 +
  geom_point()                                                         +
  xkcdaxis(xrange, yrange) 


## ----xkcd_setup, eval=FALSE----------------------------------------------
## library(extrafont)
## download.file("http://simonsoftware.se/other/xkcd.ttf", dest="xkcd.ttf")
## system("mkdir ~/.fonts")
## system("mv xkcd.ttf ~/.fonts")
## font_import()
## loadfonts()


## ----xkcd_load_fonts, echo=FALSE, message=FALSE--------------------------
loadfonts()


## ----xkcd_remove_package, eval=FALSE-------------------------------------
## remove.packages(c("extrafont","extrafontdb"))


## ----xkcd_plot, eval=FALSE-----------------------------------------------
## library(xkcd)
## xrange <- range(mtcars$mpg)
## yrange <- range(mtcars$wt)
## mtcars                                                                %>%
##   ggplot(aes(mpg, wt))                                                 +
##   geom_point()                                                         +
##   xkcdaxis(xrange, yrange)


## ----xkcd_bar_chart, echo=FALSE, fig.height=4----------------------------
library(xkcd)
library(scales)
volunteers <- data.frame(year=c(2007:2011), 
                         number=c(56470, 56998, 59686, 61783, 64251))
dsname <- "volunteers"

ds <- dsname                                                          %>% 
  get()                                                               %>% 
  mutate(xmin=year-0.1, xmax=year+0.1, ymin=50000, ymax=number)

xrange <- range(min(ds$xmin) - 0.1, max(ds$xmax) + 0.1)
yrange <- range(min(ds$ymin) + 500, max(ds$ymax) + 1000)
mapping <- aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax)

ggplot()                                                               +
  xkcdrect(mapping, ds)                                                +
  xkcdaxis(xrange, yrange)                                             +
  xlab("Year")                                                         +
  ylab("Volunteers")                                                   +
  scale_y_continuous(labels=comma)


## ----xkcd_bar_chart, eval=FALSE------------------------------------------
## library(xkcd)
## library(scales)
## volunteers <- data.frame(year=c(2007:2011),
##                          number=c(56470, 56998, 59686, 61783, 64251))
## dsname <- "volunteers"
## 
## ds <- dsname                                                          %>%
##   get()                                                               %>%
##   mutate(xmin=year-0.1, xmax=year+0.1, ymin=50000, ymax=number)
## 
## xrange <- range(min(ds$xmin) - 0.1, max(ds$xmax) + 0.1)
## yrange <- range(min(ds$ymin) + 500, max(ds$ymax) + 1000)
## mapping <- aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax)
## 
## ggplot()                                                               +
##   xkcdrect(mapping, ds)                                                +
##   xkcdaxis(xrange, yrange)                                             +
##   xlab("Year")                                                         +
##   ylab("Volunteers")                                                   +
##   scale_y_continuous(labels=comma)


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





