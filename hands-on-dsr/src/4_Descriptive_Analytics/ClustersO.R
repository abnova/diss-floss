
## ----module, echo=FALSE, results="asis"----------------------------------
Module <- "ClustersO"
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
library(rattle)	      # The weather dataset and normVarNames().
library(randomForest) # Impute missing values using na.roughfix().
library(ggplot2)      # Visualise the data through plots.
library(animation)    # Demonstrate kmeans.
library(reshape2)     # Reshape data for plotting.
library(fpc)          # Tuning clusterng with kmeansruns() and clusterboot().
library(clusterCrit)  # Clustering criteria.
library(wskm)         # Weighted subspace clustering.
library(amap)         # hclusterpar
library(cba)          # Dendrogram plot
library(dendroextras) # To colour clusters
library(kohonen)      # Self organising maps.


## ----documentation, child='documentation.Rnw', eval=TRUE-----------------


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




## ----prepare_data, message=FALSE-----------------------------------------
# Required packages
library(rattle)		# Load weather dataset. Normalise names normVarNames().
library(randomForest)	# Impute missing using na.roughfix().

# Identify the dataset.
dsname     <- "weather"
ds         <- get(dsname)
names(ds)  <- normVarNames(names(ds))
vars       <- names(ds)
target     <- "rain_tomorrow"
risk       <- "risk_mm"
id         <- c("date", "location")

# Ignore the IDs and the risk variable.
ignore     <- union(id, if (exists("risk")) risk)

# Ignore variables which are completely missing.
mvc        <- sapply(ds[vars], function(x) sum(is.na(x))) # Missing value count.
mvn        <- names(ds)[(which(mvc == nrow(ds)))]         # Missing var names.
ignore     <- union(ignore, mvn)

# Initialise the variables
vars       <- setdiff(vars, ignore)

# Variable roles.
inputc     <- setdiff(vars, target)
inputi     <- sapply(inputc, function(x) which(x == names(ds)), USE.NAMES=FALSE)
numi       <- intersect(inputi, which(sapply(ds, is.numeric)))
numc       <- names(ds)[numi]
cati       <- intersect(inputi, which(sapply(ds, is.factor)))
catc       <- names(ds)[cati]

# Impute missing values, but do this wisely - understand why missing.
if (sum(is.na(ds[vars]))) ds[vars] <- na.roughfix(ds[vars])

# Number of observations.
nobs       <- nrow(ds)


## ----two_observations----------------------------------------------------
ds[1:2, numi[1:5]]
x <- ds[1, numi[1:5]]
y <- ds[2, numi[1:5]]


## ------------------------------------------------------------------------
x-y


## ------------------------------------------------------------------------
sapply(x-y, '^', 2)


## ------------------------------------------------------------------------
sum(sapply(x-y, '^', 2))


## ------------------------------------------------------------------------
sqrt(sum(sapply(x-y, '^', 2)))


## ------------------------------------------------------------------------
dist(ds[1:2, numi[1:5]])


## ------------------------------------------------------------------------
sum(abs(x-y))
dist(ds[1:2, numi[1:5]], method="manhattan")


## ------------------------------------------------------------------------
dist(ds[1:2, numi[1:5]], method="minkowski", p=1)
dist(ds[1:2, numi[1:5]], method="minkowski", p=2)
dist(ds[1:2, numi[1:5]], method="minkowski", p=3)
dist(ds[1:2, numi[1:5]], method="minkowski", p=4)


## ----echo=FALSE, fig.height=3, out.width="\\textwidth"-------------------
dsm <- data.frame(mink=sapply(1:20, 
                      function(x) dist(ds[1:2, numi[1:5]], 
                                       method="minkowski", 
                                       p=as.numeric(x))), 
                  index=1:20)
ggplot(dsm, aes(index, mink)) + 
    geom_line() + 
    scale_y_continuous(breaks=c(6, 10, 14, 18))


## ------------------------------------------------------------------------
dist(ds[1:5, numi[1:5]])
daisy(ds[1:5, numi[1:5]])
daisy(ds[1:5, cati])


## ------------------------------------------------------------------------
model <- m.km <- kmeans(ds, 10)


## ------------------------------------------------------------------------
set.seed(42)
model <- m.km <- kmeans(ds[numi], 10)


## ------------------------------------------------------------------------
model$size


## ------------------------------------------------------------------------
model$centers


## ------------------------------------------------------------------------
head(model$cluster)


## ------------------------------------------------------------------------
model$iter
model$ifault


## ------------------------------------------------------------------------
summary(ds[numi[1:5]])
summary(scale(ds[numi[1:5]]))


## ------------------------------------------------------------------------
dsc <- scale(ds[numi[1:5]])
attr(dsc, "scaled:center")
attr(dsc, "scaled:scale")


## ------------------------------------------------------------------------
sapply(ds[numi[1:5]], mean)
sapply(ds[numi[1:5]], sd)


## ------------------------------------------------------------------------
set.seed(42)
model <- m.kms <- kmeans(scale(ds[numi]), 10)
model$size
model$centers


## ------------------------------------------------------------------------
model$totss
model$withinss
model$tot.withinss
model$betweenss
model$iter
model$ifault


## ------------------------------------------------------------------------
library(animation)


## ------------------------------------------------------------------------
cent <- 1.5 * c(1, 1, -1, -1, 1, -1, 1, -1)
x    <- NULL
for (i in 1:8) x <- c(x, rnorm(25, mean=cent[i]))
x  <- matrix(x, ncol=2)
colnames(x) <- c("X1", "X2")

dim(x)
head(x)


## ----animate, eval=FALSE-------------------------------------------------
## par(mar=c(3, 3, 1, 1.5), mgp=c(1.5, 0.5, 0), bg="white")
## kmeans.ani(x, centers=4, pch=1:4, col=1:4)


## ----animate, echo=FALSE, out.width="\\textwidth"------------------------
par(mar=c(3, 3, 1, 1.5), mgp=c(1.5, 0.5, 0), bg="white")
kmeans.ani(x, centers=4, pch=1:4, col=1:4)


## ----viz_ggplot2, echo=FALSE---------------------------------------------
dscm <- melt(model$centers)
names(dscm) <- c("Cluster", "Variable", "Value")
dscm$Cluster <- factor(dscm$Cluster)
dscm$Order <- as.vector(sapply(1:length(numi), rep, 10))
p <- ggplot(subset(dscm, Cluster %in% 1:10), 
            aes(x=reorder(Variable, Order), 
                y=Value, group=Cluster, colour=Cluster))
p <- p + coord_polar()
p <- p + geom_point()
p <- p + geom_path()
p <- p + labs(x=NULL, y=NULL)
p <- p + theme(axis.ticks.y=element_blank(), axis.text.y = element_blank())
p


## ----viz_ggplot2, eval=FALSE---------------------------------------------
## dscm <- melt(model$centers)
## names(dscm) <- c("Cluster", "Variable", "Value")
## dscm$Cluster <- factor(dscm$Cluster)
## dscm$Order <- as.vector(sapply(1:length(numi), rep, 10))
## p <- ggplot(subset(dscm, Cluster %in% 1:10),
##             aes(x=reorder(Variable, Order),
##                 y=Value, group=Cluster, colour=Cluster))
## p <- p + coord_polar()
## p <- p + geom_point()
## p <- p + geom_path()
## p <- p + labs(x=NULL, y=NULL)
## p <- p + theme(axis.ticks.y=element_blank(), axis.text.y = element_blank())
## p


## ----viz_ggplot2_k4, echo=FALSE------------------------------------------
nclust <- 4
model <- m.kms <- kmeans(scale(ds[numi]), nclust)
dscm <- melt(model$centers)
names(dscm) <- c("Cluster", "Variable", "Value")
dscm$Cluster <- factor(dscm$Cluster)
dscm$Order <- as.vector(sapply(1:length(numi), rep, nclust))
p <- ggplot(dscm, 
            aes(x=reorder(Variable, Order), 
                y=Value, group=Cluster, colour=Cluster))
p <- p + coord_polar()
p <- p + geom_point()
p <- p + geom_path()
p <- p + labs(x=NULL, y=NULL)
p <- p + theme(axis.ticks.y=element_blank(), axis.text.y = element_blank())
p


## ----viz_ggplot2_k4, eval=FALSE------------------------------------------
## nclust <- 4
## model <- m.kms <- kmeans(scale(ds[numi]), nclust)
## dscm <- melt(model$centers)
## names(dscm) <- c("Cluster", "Variable", "Value")
## dscm$Cluster <- factor(dscm$Cluster)
## dscm$Order <- as.vector(sapply(1:length(numi), rep, nclust))
## p <- ggplot(dscm,
##             aes(x=reorder(Variable, Order),
##                 y=Value, group=Cluster, colour=Cluster))
## p <- p + coord_polar()
## p <- p + geom_point()
## p <- p + geom_path()
## p <- p + labs(x=NULL, y=NULL)
## p <- p + theme(axis.ticks.y=element_blank(), axis.text.y = element_blank())
## p


## ----radial_plot, echo=FALSE---------------------------------------------
source("http://onepager.togaware.com/CreateRadialPlot.R")
dsc <- data.frame(group=factor(1:4), model$centers)
CreateRadialPlot(dsc, grid.min=-2, grid.max=2, plot.extent.x=1.5)


## ----radial_plot, eval=FALSE---------------------------------------------
## source("http://onepager.togaware.com/CreateRadialPlot.R")
## dsc <- data.frame(group=factor(1:4), model$centers)
## CreateRadialPlot(dsc, grid.min=-2, grid.max=2, plot.extent.x=1.5)


## ----radial_plot_4, echo=FALSE-------------------------------------------
CreateRadialPlot(subset(dsc, group==4), grid.min=-2, grid.max=2, plot.extent.x=1.5)


## ----radial_plot_4, eval=FALSE-------------------------------------------
## CreateRadialPlot(subset(dsc, group==4), grid.min=-2, grid.max=2, plot.extent.x=1.5)


## ----radial_plot_grid, echo=FALSE, fig.width=10, fig.height=10-----------
p1 <- CreateRadialPlot(subset(dsc, group==1), 
                       grid.min=-2, grid.max=2, plot.extent.x=2)
p2 <- CreateRadialPlot(subset(dsc, group==2), 
                       grid.min=-2, grid.max=2, plot.extent.x=2)
p3 <- CreateRadialPlot(subset(dsc, group==3), 
                       grid.min=-2, grid.max=2, plot.extent.x=2)
p4 <- CreateRadialPlot(subset(dsc, group==4), 
                       grid.min=-2, grid.max=2, plot.extent.x=2)
library(gridExtra)
grid.arrange(p1+ggtitle("Cluster1"), p2+ggtitle("Cluster2"), 
             p3+ggtitle("Cluster3"), p4+ggtitle("Cluster4"))


## ----radial_plot_grid, eval=FALSE----------------------------------------
## p1 <- CreateRadialPlot(subset(dsc, group==1),
##                        grid.min=-2, grid.max=2, plot.extent.x=2)
## p2 <- CreateRadialPlot(subset(dsc, group==2),
##                        grid.min=-2, grid.max=2, plot.extent.x=2)
## p3 <- CreateRadialPlot(subset(dsc, group==3),
##                        grid.min=-2, grid.max=2, plot.extent.x=2)
## p4 <- CreateRadialPlot(subset(dsc, group==4),
##                        grid.min=-2, grid.max=2, plot.extent.x=2)
## library(gridExtra)
## grid.arrange(p1+ggtitle("Cluster1"), p2+ggtitle("Cluster2"),
##              p3+ggtitle("Cluster3"), p4+ggtitle("Cluster4"))


## ------------------------------------------------------------------------
model <- m.kms <- kmeans(scale(ds[numi]), 1)
model$size
model$centers
model$totss
model$withinss
model$tot.withinss
model$betweenss
model$iter
model$ifault


## ------------------------------------------------------------------------
library(fpc)
model <- m.kmcb <- clusterboot(scale(ds[numi]), 
                               clustermethod=kmeansCBI,
                               runs=10, 
                               krange=10, 
                               seed=42)
model
str(model)


## ------------------------------------------------------------------------
model <- kmeans(scale(ds[numi]), 10)
model$totss
model$withinss
model$tot.withinss
model$betweenss


## ----withinss------------------------------------------------------------
model$withinss
model$tot.withinss


## ------------------------------------------------------------------------
model$betweenss


## ----echo=FALSE, fig.height=3.5, out.width="\\textwidth"-----------------
m.kms <- sapply(1:50, function(x) kmeans(scale(ds[numi]), x, nstart=10))
ds.kms <- melt(data.frame(size=sapply(m.kms['size',], length), 
                          tot.withinss=unlist(m.kms['tot.withinss',]), 
                          betweenss=unlist(m.kms['betweenss',])),
               "size")
p <- ggplot(ds.kms, aes(x=size, y=value, colour=variable))
p <- p + geom_line()
p <- p + labs(x="Number of Clusters", y="Sum of Squares", colour="Measure")
p


## ------------------------------------------------------------------------
crit  <- vector()
nk    <- 1:20
for (k in nk) 
{
  m <- kmeans(scale(ds[numi]), k)
  crit <- c(crit, sum(m$withinss))
}
crit


## ----best_wss, fig.height=2----------------------------------------------
dsc  <- data.frame(k=nk, crit=scale(crit))
dscm <- melt(dsc, id.vars="k", variable.name="Measure")

p <- ggplot(dscm, aes(x=k, y=value, colour=Measure))
p <- p + geom_point(aes(shape=Measure))
p <- p + geom_line(aes(linetype=Measure))
p <- p + scale_x_continuous(breaks=nk, labels=nk)
p <- p + theme(legend.position="none")
p


## ------------------------------------------------------------------------
library(fpc)
nk <- 1:20
model <- km.c <- kmeansruns(scale(ds[numi]), krange=nk, criterion="ch")
class(model)
model
model$crit
model$bestk


## ----best_ch, fig.height=2-----------------------------------------------
dsc  <- data.frame(k=nk, crit=scale(km.c$crit))
dscm <- melt(dsc, id.vars="k", variable.name="Measure")

p <- ggplot(dscm, aes(x=k, y=value, colour=Measure))
p <- p + geom_point(aes(shape=Measure))
p <- p + geom_line(aes(linetype=Measure))
p <- p + scale_x_continuous(breaks=nk, labels=nk)
p <- p + theme(legend.position="none")
p


## ------------------------------------------------------------------------
library(fpc)
nk <- 1:20
model <- km.a <- kmeansruns(scale(ds[numi]), krange=nk, criterion="asw")
class(model)
model
model$crit
model$bestk


## ----best_asw, fig.height=2----------------------------------------------
dsc  <- data.frame(k=nk, crit=scale(km.a$crit))
dscm <- melt(dsc, id.vars="k", variable.name="Measure")

p <- ggplot(dscm, aes(x=k, y=value, colour=Measure))
p <- p + geom_point(aes(shape=Measure))
p <- p + geom_line(aes(linetype=Measure))
p <- p + scale_x_continuous(breaks=nk, labels=nk)
p <- p + theme(legend.position="none")
p


## ----echo=FALSE----------------------------------------------------------
set.seed(6543)


## ------------------------------------------------------------------------
library(clusterCrit)
crit <- vector()
for (k in 1:20) 
{
  m <- kmeans(scale(ds[numi]), k)
  crit <- c(crit, as.numeric(intCriteria(as.matrix(ds[numi]), m$cluster, 
                                         "Calinski_Harabasz")))
}
crit[is.nan(crit)] <- 0 # 
crit
bestCriterion(crit, "Calinski_Harabasz")


## ----best_crit_ch, fig.height=2------------------------------------------
dsc  <- data.frame(k=nk, crit=scale(crit))
dscm <- melt(dsc, id.vars="k", variable.name="Measure")

p <- ggplot(dscm, aes(x=k, y=value, colour=Measure))
p <- p + geom_point(aes(shape=Measure))
p <- p + geom_line(aes(linetype=Measure))
p <- p + scale_x_continuous(breaks=nk, labels=nk)
p <- p + theme(legend.position="none")
p


## ------------------------------------------------------------------------
m <- kmeans(scale(ds[numi]), 5)
ic <- intCriteria(as.matrix(ds[numi]), m$cluster, "all")
names(ic)


## ------------------------------------------------------------------------
crit <- data.frame()
for (k in 2:20) 
{
  m <- kmeans(scale(ds[numi]), k)
  crit <- rbind(crit, as.numeric(intCriteria(as.matrix(ds[numi]), m$cluster, 
                                             "all")))
}
names(crit) <- substr(sub("_", "", names(ic)), 1, 5) # Shorten for plots.
crit <- data.frame(sapply(crit, function(x) {x[is.nan(x)] <- 0; x}))


## ----best_crit_all, fig.height=2, out.width="\\textwidth"----------------
dsc  <- cbind(k=2:20, data.frame(sapply(crit, scale)))

dscm <- melt(dsc, id.vars="k", variable.name="Measure")
dscm$value[is.nan(dscm$value)] <- 0

ms <- as.character(unique(dscm$Measure))

p <- ggplot(subset(dscm, Measure %in% ms[1:6]), aes(x=k, y=value, colour=Measure))
p <- p + geom_point(aes(shape=Measure)) + geom_line(aes(linetype=Measure))
p <- p + scale_x_continuous(breaks=nk, labels=nk)
p


## ----echo=FALSE, fig.width=15, fig.height=20, out.width="\\textwidth"----
p1 <- ggplot(subset(dscm, Measure %in% ms[7:12]), aes(x=k, y=value, colour=Measure))
p1 <- p1 + geom_point(aes(shape=Measure))
p1 <- p1 + geom_line(aes(linetype=Measure))
p1 <- p1 + scale_x_continuous(breaks=nk, labels=nk)
p1 <- p1 + theme(legend.title=element_blank())

p2 <- ggplot(subset(dscm, Measure %in% ms[13:18]), aes(x=k, y=value, colour=Measure))
p2 <- p2 + geom_point(aes(shape=Measure))
p2 <- p2 + geom_line(aes(linetype=Measure))
p2 <- p2 + scale_x_continuous(breaks=nk, labels=nk)
p2 <- p2 + theme(legend.title=element_blank())

p3 <- ggplot(subset(dscm, Measure %in% ms[19:24]), aes(x=k, y=value, colour=Measure))
p3 <- p3 + geom_point(aes(shape=Measure))
p3 <- p3 + geom_line(aes(linetype=Measure))
p3 <- p3 + scale_x_continuous(breaks=nk, labels=nk)
p3 <- p3 + theme(legend.title=element_blank())

p4 <- ggplot(subset(dscm, Measure %in% ms[25:30]), aes(x=k, y=value, colour=Measure))
p4 <- p4 + geom_point(aes(shape=Measure))
p4 <- p4 + geom_line(aes(linetype=Measure))
p4 <- p4 + scale_x_continuous(breaks=nk, labels=nk)
p4 <- p4 + theme(legend.title=element_blank())

p5 <- ggplot(subset(dscm, Measure %in% ms[31:36]), aes(x=k, y=value, colour=Measure))
p5 <- p5 + geom_point(aes(shape=Measure))
p5 <- p5 + geom_line(aes(linetype=Measure))
p5 <- p5 + scale_x_continuous(breaks=nk, labels=nk)
p5 <- p5 + theme(legend.title=element_blank())

p6 <- ggplot(subset(dscm, Measure %in% ms[37:42]), aes(x=k, y=value, colour=Measure))
p6 <- p6 + geom_point(aes(shape=Measure))
p6 <- p6 + geom_line(aes(linetype=Measure))
p6 <- p6 + scale_x_continuous(breaks=nk, labels=nk)
p6 <- p6 + theme(legend.title=element_blank())

library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, p6, ncol=1)


## ------------------------------------------------------------------------
set.seed(42)
train <- sample(nobs, 0.7*nobs)
test  <- setdiff(seq_len(nobs), train)

model <- kmeans(ds[train, numi], 2)
predict(model, ds[test, numi])


## ----message=FALSE-------------------------------------------------------
set.seed(42)
library(wskm)
m.ewkm <- ewkm(ds, 10)


## ------------------------------------------------------------------------
m.ewkm <- ewkm(ds[numi], 10)


## ----out.lines=11--------------------------------------------------------
round(100*m.ewkm$weights)


## ----pam-----------------------------------------------------------------
model <- pam(ds[numi], 10, FALSE, "euclidean")

summary(model)

plot(ds[numi[1:5]], col=model$clustering)
points(model$medoids, col=1:10, pch=4)

plot(model)


## ------------------------------------------------------------------------
library(amap)
model <- hclusterpar(na.omit(ds[numi]), 
                     method="euclidean", 
                     link="ward",
                     nbproc=1)


## ------------------------------------------------------------------------
plot(model, main="Cluster Dendrogram", xlab="", labels=FALSE, hang=0)

#Add in rectangles to show the clusters.

rect.hclust(model, k=10)


## ------------------------------------------------------------------------
library(dendroextras)
plot(colour_clusters(model, k=10), xlab="")


## ----som_plot, echo=FALSE------------------------------------------------
library("kohonen")
set.seed(42)
model <- som(scale(ds[numi[1:14]]), grid = somgrid(5, 4, "hexagonal"))
plot(model, main="Weather Data")


## ----som_plot, eval=FALSE------------------------------------------------
## library("kohonen")
## set.seed(42)
## model <- som(scale(ds[numi[1:14]]), grid = somgrid(5, 4, "hexagonal"))
## plot(model, main="Weather Data")


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





