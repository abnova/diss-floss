# Start with a clean environment
## @knitr CleanEnv
rm(list = ls(all.names = TRUE))

## @knitr LoadPackages
if (!suppressMessages(require(RCurl))) install.packages('RCurl')
if (!suppressMessages(require(stringr))) install.packages('stringr')
if (!suppressMessages(require(ggplot2))) install.packages('ggplot2')
if (!suppressMessages(require(scales))) install.packages('scales')
if (!suppressMessages(require(RColorBrewer)))
  install.packages('RColorBrewer')
if (!suppressMessages(require(gridExtra))) install.packages('gridExtra')
if (!suppressMessages(require(psych))) install.packages('psych')
if (!suppressMessages(require(polycor))) install.packages('polycor')
if (!suppressMessages(require(GGally))) install.packages('GGally')
if (!suppressMessages(require(tables))) install.packages('tables')
if (!suppressMessages(require(Hmisc))) install.packages('Hmisc') # for 'tables'

library(RCurl)
library(stringr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(gridExtra)
library(psych)
library(polycor)
library(GGally)
library(tables)
library(Hmisc)

## @knitr PrepareEDA
PRJ_HOME <- Sys.getenv("DISS_FLOSS_HOME")

source(file.path(PRJ_HOME, "config/diss-floss-config.R"))
source(file.path(PRJ_HOME, "utils/factors.R"))
source(file.path(PRJ_HOME, "utils/qq.R"))
source(file.path(PRJ_HOME, "utils/data.R"))
source(file.path(PRJ_HOME, "utils/utils.R"))
source(file.path(PRJ_HOME, "utils/platform.R"))
source(file.path(PRJ_HOME, "analysis/mixDist.R"))
source(file.path(PRJ_HOME, "utils/knit.R"))

DIST_FIT_COLOR <- "green" # ggplot2 line color for distrib. fitting

GRADIENT_LOW  <- "#56B1F7"  # light blue color 
GRADIENT_HIGH <- "#132B43"  # dark blue color

allPlots <- list()


## @knitr PerformEDA

##### MISC FUNCTIONS #####

##### EDA CATEGORIES #####

uniDescriptiveEDA <- function (df, var, colName, extraFun) {
  
  data <- df[[colName]]
  
  if (is.numeric(data) || is.factor(data)) {
    message("\nDecriptive statistics for '", colName, "':\n")
    print(summary(data))
  }
}


uniVisualEDA <- function (df, var, colName, extraFun) {
  
  data <- df[[colName]]
  
  #env <- new.env()
  #do.call(extraFun, list(df, var), envir = env)
  
  if (is.numeric(data)) {
    # plot original data
    g_var <- paste0("histogram_", colName)
    assign(g_var, plotHistogram(df, colName), envir = .GlobalEnv)
    myPlot <- get(g_var, envir = .GlobalEnv)
    myList <- list(myPlot)
    names(myList) <- g_var
    allPlots <<- c(allPlots, myList)
    
    # plot log-transformed data
    g_var <- paste0("histogram_log_", colName)
    assign(g_var, plotHistogram(df, colName, TRUE), envir = .GlobalEnv)
    myPlot <- get(g_var, envir = .GlobalEnv)
    myList <- list(myPlot)
    names(myList) <- g_var
    allPlots <<- c(allPlots, myList)
  }
  
  if (is.factor(data)) {
    g_var <- paste0("barchart_", colName)
    assign(g_var, plotBarChart(df, colName), envir = .GlobalEnv)
    myPlot <- get(g_var, envir = .GlobalEnv)
    myList <- list(myPlot)
    names(myList) <- g_var
    allPlots <<- c(allPlots, myList)
  }
  
  if (is.numeric(data)) {
    g_var <- paste0("qqplot_", colName)
    assign(g_var, ggQQplot(df, colName), envir = .GlobalEnv)
    myPlot <- get(g_var, envir = .GlobalEnv)
    myList <- list(myPlot)
    names(myList) <- g_var
    allPlots <<- c(allPlots, myList)
  }
  
  #if (TBD CONDITION) {
  #  plot <- plotDensity(df, colName)
  #  allPlots <<- c(allPlots, list(plot))
  #}
}


multiDescriptiveEDA <- function (df) {
  
  # extract the dataset name from actual function call
  datasetName <- getArgValue(match.call(), "df")
  
  # it's possible to customize describe()'s output as follows:
  # describe(mtcars)[, c(2, 3, 4, 5, 8, 9)]
  
  if (KNITR) {
    describe_var <- paste0("describe_", datasetName)
    assign(describe_var, psych::describe(df), envir = .GlobalEnv)
  } else {
    message("\nDecriptive statistics for '", datasetName, "':\n")
    # suppress "NAs introduced by coercion" warnings
    suppressWarnings(print(psych::describe(df)))
  }
}


correlationAnalysis <- function (df) {
  
  # due to very small amount of projects with "Non-OSI" licesnse
  # and their disapperance due to calculating correlations,
  # we remove this indicator from EDA (consider analyzing it
  # at later phases with inclusion of imputed data)
  df <- df[setdiff(names(df), "License.Category")]

  # use hetcor() from 'polycor' package instead of corr.test()
  # in order to handle heterogenous data w/out conversion
  corr.info <- hetcor(df, use="pairwise.complete.obs",
                      std.err = FALSE) # use $correlations just for corr
  print(corr.info, digits=2)
}


mvnTests <- function (df, indicators) {
  
  # Test for multivariate normality, using 'MVN' package
  message("\nTesting data for multivariate normality...\n")
  
  # default to 1% of the data set
  sampleSize <- nrow(df) / 100
  
  # sample size should not exceed 2000 for one of MVN tests
  sampleSize <- ifelse(sampleSize >= 2000, 1000, sampleSize)
  
  # sample the data set
  flossDataTest <- sampleDF(df, sampleSize)

  # convert factors to integers
  factorCols <- vapply(flossDataTest, is.factor, logical(1))
  flossDataTest[factorCols] <- lapply(flossDataTest[factorCols], as.integer)
  
  flossDataTest <-
    flossDataTest[setdiff(names(flossDataTest), "License.Category")]
  
  mvn.result <- MVN::mardiaTest(flossDataTest, cov = TRUE, qqplot = FALSE)
  print(mvn.result)
  
  mvn.result <- MVN::hzTest(flossDataTest, cov = TRUE, qqplot = FALSE)
  print(mvn.result)
  
  mvn.result <- MVN::roystonTest(flossDataTest, qqplot = FALSE)
  print(mvn.result)
}


multiAnalyticalEDA <- function (df, indicators) {
  
  corrMat <- correlationAnalysis(df)
  if (R.version$major > 2) mvnTests(df)
  
  return (corrMat)
}


multiVisualEDA <- function (df, corrMat) {

  # log transform continuous data
  df["Project.Age"] <- log(df["Project.Age"])
  df["Development.Team.Size"] <- log(df["Development.Team.Size"])
  df["User.Community.Size"] <- log(df["User.Community.Size"])

  # drop this indicator to match dimensions of the hetcor()'s results
  df <- df[setdiff(names(df), "License.Category")]
  
  gPlotMatrix <- ggpairs(
    df, title = "",
    lower = list(continuous = "smooth", combo = "box", discrete = "ratio",
                 params = c(color = "blue")),
    upper = list(params = list(corSize = 6), combo = ""),
    diag = list(continuous = "bar", params = c(colour = "blue")),
    axisLabels = "show"
  )

  # Customization options (TBD, low priority):
  # 1. extract plot object / getPlot(), apply theme() to it, put back
  # 2. Redefine GGally::ggally_diagAxis() in global env.
  # 3. use "params" in ggpairs() for lower triangle definition (FAIL)
  
  if (FALSE)
  # rotate x- and y-axis labels & y-axis title; move them away from axes
  gPlotMatrix <- gPlotMatrix +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, color = "black"),
          axis.text.y = element_text(angle = 45, vjust = 1, color = "black"),
          axis.title.y = element_text(angle = 45, vjust = 1, color = "black"))
  
  # generate custom panels with corr. coefficients from hetcor()
  
  # index of upper triangle rows and columns
  # replace r with hetcor matrix
  index <- which(upper.tri(corrMat), arr.ind = TRUE)
  
  # loop through upper triangle and replace
  for (i in 1:nrow(index)) {
    gPlotMatrix <- putPlot(gPlotMatrix,
                           ggally_text(sprintf("Corr:\n%0.2f",
                                               corrMat[index[i, 1],
                                                       index[i, 2]])),
                           index[i, 1], index[i, 2])
  }
  
  # title will be saved as & extracted from an attribute (for fig. caption)
  attr(gPlotMatrix, "title") <- "Plot matrix (summary visual overview)"
  
  # display customized plot in RStudio Plots pane
  if (.Platform$GUI == "RStudio") print(gPlotMatrix)
  
  if (KNITR) {  # export plot object for knitr report

    # extract the dataset name from actual function call
    datasetName <- getArgValue(match.call(), "df")

    g_var <- paste0("plotmatrix_", "pmat_", datasetName)
    assign(g_var, gPlotMatrix, envir = .GlobalEnv)
    
  } else {  # save plot object into a file
    
    fPlotMatrix <- file.path(EDA_RESULTS_DIR, paste0("gPlotMatrix", ".svg"))
    svg(fPlotMatrix, height = 7, width = 7)
    print(gPlotMatrix)
    dev.off()
  }
  
  if (FALSE) {
    # Plot with 'Project.Stage' as color (should be a factor)
    # (this plot doesn't seem to be very informative, but...)
    g2 <- ggpairs(df, title = "Pairwise Scatterplots",
                  lower=list(continuous = "smooth", params = c(color = "blue")),
                  upper=list(params = list(corSize = 6)),
                  diag=list(continuous = "bar", params = c(color = "blue")), 
                  axisLabels = "show",
                  color = "Project.Stage")
    print(g2)
  }
}


performEDA <- function (df, indicator, colName, extraFun) {
  
  # Create a subset of the original data set
  # by excluding missing values for the analyzed variable
  df <- df[complete.cases(df[[colName]]), ]
  
  uniDescriptiveEDA(df, indicator, colName, extraFun)
  uniVisualEDA(df, indicator, colName, extraFun)

  # mixture analysis and results visualization
  if (is.numeric(df[[colName]]) &&
        all(prop.table(table(df[[colName]])) < .1))
    if (DO_MIX_ANALYSIS) mixDistAnalysis(df, indicator, colName)
}


performMultiEDA <- function (flossData, dataSource, indicators) {

  # validity check
  if (length(indicators) == 0) {
    warning("No indicators specified for '", dataSource, "' - EDA skipped!")
    return
  }

  # restrict EDA to specified set of indicators
  flossData <- flossData[indicators]
  
  # exclude nominal variables from further analysis
  analysisCols <- vapply(flossData,
                         function(x) {is.ordered(x) || is.numeric(x)},
                         logical(1))
  flossData <- flossData[, analysisCols]
  
  # remove projects with level 'Inactive' from further analysis
  pmFactor <- as.name(flossData[["Project.Stage"]])
  levels(pmFactor)[nlevels(pmFactor)] <- NA
  
  # perform multivariate EDA
  multiDescriptiveEDA(flossData)
  corrMat <- multiAnalyticalEDA(flossData)
  if (DO_MULTI_VISUAL) multiVisualEDA(flossData, corrMat$correlations)
}


##### VISUAL EDA #####

##### Univariate Visual EDA

# Plot distribution of a continuous variable "colName"
plotHistogram <- function (df, colName, log = FALSE, print = TRUE) {
  
  df <- df
  df$var <- df[[colName]]
  yAxisLog <- FALSE
  
  if (log) {
    if (any(df$var < 0)) df$var <- df$var + abs(min(df$var)) + 0.01
    # instead of log transforming data directly,
    # we do that further via ggplot2's scales functionality (x-axis)
  }

  title <- paste("Projects distribution across", colName, "range")
  xLabel <- colName
  yLabel <- "Number of projects"
  
  if (identical(colName, "Project.Age"))
    xLabel <- paste(xLabel, "(months)")
  
  # check whether log transformation of data is requested
  
  if (log) {  # log-transform data and x-axis scale
    
    # fit distribution
    optimalDist <- suppressWarnings(findOptimalDist(log(df$var + 1)))
    
    xLabel <- paste(xLabel, "[Log]")
    scale_x <- scale_x_continuous(
      xLabel, trans = "log",
      breaks = trans_breaks('log', function(x) exp(x)),
      labels = trans_format('log', math_format(.x)))
    
  } else {  # no log transformation

    # fit distribution
    optimalDist <- suppressWarnings(findOptimalDist(df$var))
    
    scale_x <- scale_x_continuous(xLabel, labels = prettyNum)
  }

  # calculate optimal value for histogram's bin width
  breaks <- pretty(range(df$var), n = nclass.FD(df$var), min.n = 1)
  bwidth <- (breaks[2] - breaks[1]) / 2
  if (log) {
    # Adjust bin width value, if needed, to compensate for
    # x-axis scale log transformation (depends on data's IQR):
    # divide by some heuristic-based numbers (two distinct cases).
    if (bwidth > 1) bwidth <- bwidth / 100
    else if (bwidth > 0.25) bwidth <- bwidth / 10
  }

  # assess kurtosis of the data distribution
  
  # if the distribution is is leptokurtic or platycurtic
  # (excess curtosis) - log transform data and y-axis scale
  if (abs(kurtosi(df$var)) > 10) {
    
    yAxisLog <- TRUE
    
    # on-the-fly transform count data to prevent negative values
    # during log transformation of count data and y-axis scale
    
    yLabel <- paste(yLabel, "[Log10]")
    scale_y <-
      scale_y_continuous(yLabel,
                         #breaks = trans_breaks("log10", function(x) 10^x),
                         labels = prettyNum)

    myHist <- geom_histogram(aes(y = log10(..count.. + 1),
                                 fill = log10(..count.. + 1)),
                             binwidth = bwidth,
                             position = "identity")
  } else {
    
    scale_y <-
      scale_y_continuous(yLabel,
                         labels = prettyNum)

    myHist <- geom_histogram(aes(y = ..count..,
                                 fill = ..count..),
                             binwidth = bwidth,
                             position = "identity")
  }
  
  # handle platform's version differences for 'ggplot2' API
  if (compareVersion(GGPLOT2_VER, "0.9.1") == 1)  # later version
    myTitle <- ggtitle(label=title)
  else
    myTitle <- opts(title=title)

  # build the plot
  g <- ggplot(df, aes(x = var)) +
    scale_fill_continuous("Number of\nprojects",
                          low = GRADIENT_LOW, high = GRADIENT_HIGH) +
    scale_x +
    scale_y +
    myHist
  
  # overlay with density-like plot, based on data count
  g <- g + stat_function(fun = dist.count,
                         args = list(distInfo = optimalDist,
                                     n = length(df$var),
                                     binwidth = bwidth,
                                     logScale = yAxisLog),
                         color = "red")
  
  # ignore NA values for mean
  g <- g + geom_vline(aes(xintercept = mean(var + 1)),
                      linetype = "longdash", color = "red")
  
  # title will be saved as & extracted from an attribute (for fig. caption)
  attr(g, "title") <- title
  
  if (.Platform$GUI == "RStudio") print(g)
  
  #TODO: consider moving to main
  if (!KNITR) {
    edaFile <- str_replace_all(string=colName, pattern=" ", repl="")
    edaFile <- file.path(EDA_RESULTS_DIR, paste0(edaFile, ".svg"))
    suppressMessages(ggsave(file=edaFile, plot=g, width=8.5, height=11))
  }
  
  return (g)
}


# Plot distribution of a continuous variable "colName" by category
plotDensity <- function (df, colName) {
  
  df <- df
  df$var <- df[[colName]]
  df$category <- factor(df[[colName]])
  
  title <- paste("Projects distribution across", colName,
                 "range (by category)")
  xLabel <- colName
  
  breaks <- pretty(range(df$var), n = nclass.FD(df$var), min.n = 1)
  bwidth <- breaks[2] - breaks[1]
  
  g <- ggplot(df, aes(x=var, fill=var)) +
    geom_density(aes(y=..count..), 
                 binwidth=bwidth, position="identity")
  
  # title will be saved as & extracted from an attribute (for fig. caption)
  attr(g, "title") <- title
  
  if (.Platform$GUI == "RStudio") print(g)
  
  #TODO: consider moving to main
  if (!KNITR) {
    edaFile <- str_replace_all(string=colName, pattern=" ", repl="")
    edaFile <- file.path(EDA_RESULTS_DIR, paste0(edaFile, ".svg"))
    suppressMessages(ggsave(file=edaFile, plot=g, width=8.5, height=11))
  }
  
  return (g)
}


# Plot distribution of a categorical variable "colName"
plotBarChart <- function (df, colName) {
  
  SHOW_LEVELS <- 10
  df <- df
  df$var <- factor(df[[colName]])
  
  # sort factor levels by the frequency of levels
  df$var <- reorder(df$var, df$var, function(x) -length(x))
  df$var <- topFactors(df$var, SHOW_LEVELS, o="The Rest")
  
  title <- paste("Projects distribution across", colName, "range")
  
  # prepare to place percentage on top of bars, using geom_text()
  if (FALSE) {
    dfTab <- as.data.frame(table(df))
    colnames(dfTab)[1] <- "x"
    dfTab$lab <- as.character(100 * dfTab$Freq / sum(dfTab$Freq))
  }
  
  # handle platform's version differences for 'ggplot2' API
  if (compareVersion(GGPLOT2_VER, "0.9.1") == 1)  # later version
    myTitle <- ggtitle(label=title)
  else
    myTitle <- opts(title=title)
  
  # build the plot
  g <- ggplot(df, aes(x=var, fill=var)) +
    geom_bar(stat="bin", position="identity") +
    scale_fill_discrete(colName) + 
    xlab(colName) +
    ylab("Number of projects")
  
  # display pre-calculated percentage on top of bars
  if (FALSE) {
    g <- g +
      geom_text(data=dfTab,aes(x=x,y=Freq,label=lab),vjust=0) +
      theme(axis.text.x=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),legend.title=element_blank(),
            axis.title.y=element_blank())  
  }
  
  # title will be saved as & extracted from an attribute (for fig. caption)
  attr(g, "title") <- title
  
  if (.Platform$GUI == "RStudio") print(g)
  
  #TODO: consider moving to main
  if (!KNITR) {
    edaFile <- str_replace_all(string=colName, pattern=" ", repl="")
    edaFile <- file.path(EDA_RESULTS_DIR, paste0(edaFile, ".svg"))
    suppressMessages(ggsave(file=edaFile, plot=g, width=8.5, height=11))
  }
  
  return (g)
}


ggQQplot <- function (df, colName) # argument: vector of numbers
{
  title <- paste0("Q-Q plot for '", colName, "'")
  vec <- df[[colName]]
  
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  d <- data.frame(resids = vec)
  
  # handle platform's version differences for 'ggplot2' API
  if (compareVersion(GGPLOT2_VER, "0.9.1") == 1)  # later version
    myTitle <- ggtitle(label=title)
  else
    myTitle <- opts(title=title)
  
  # build the plot
  g <- ggplot(d, aes(sample = resids)) +
    
    # Normal distribution function by default
    # suppress "Removed N rows containing missing values (stat_qq)"
    suppressWarnings(stat_qq()) +
    
    # Gamma distribution function
    #stat_qq(distribution = qgamma, dparams=list(shape=1)) +
    
    # Adaptive QQ plotting fuction
    #stat_function(fun=qq, args = list(x0 = sample, y0 = vec)) +
    
    geom_abline(slope = slope, intercept = int) +
    scale_x_continuous("Theoretical Quantiles") +
    scale_y_continuous("Sample Quantiles")
  
  # title will be saved as & extracted from an attribute (for fig. caption)
  attr(g, "title") <- title
  
  if (.Platform$GUI == "RStudio") print(g)
  
  #TODO: consider moving to main
  if (!KNITR) {
    edaFile <- str_replace_all(string=colName, pattern=" ", repl="")
    edaFile <- file.path(EDA_RESULTS_DIR, paste0("QQ-", edaFile, ".svg"))
    suppressMessages(ggsave(file=edaFile, plot=g, width=8.5, height=11))
  }
  
  return (g)
}


ggQQplotMV <- function(dat) {

  title <- "Q-Q plot of Mahalanobis distance"
  
  descStat <- list(mu = colMeans(dat, na.rm = TRUE),
                   sigma = cov(dat, use = "pairwise.complete.obs"))
  
  dat <- na.omit(dat)
  D2 <- mahalanobis(dat, descStat$mu, descStat$sigma)
  
  results <- 
    data.frame(theor = sort(qchisq(ppoints(nrow(dat)), df = ncol(dat))),
               observ = sort(D2))
  
  # handle platform's version differences for 'ggplot2' API
  if (compareVersion(GGPLOT2_VER, "0.9.1") == 1)  # later version
    myTitle <- ggtitle(label=title)
  else
    myTitle <- opts(title=title)
  
  # build the plot
  g <- ggplot(results, aes(x = theor, y = observ)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0) +
    scale_x_continuous("Theoretical Quantiles") +
    scale_y_continuous("Sample Quantiles")
  
  # title will be saved as & extracted from an attribute (for fig. caption)
  attr(g, "title") <- title
  
  if (.Platform$GUI == "RStudio") {
    g <- g + myTitle
    print(g)
  }
  
  #TODO: consider moving to main
  if (!KNITR) {
    edaFile <- file.path(EDA_RESULTS_DIR, paste0("QQ-MV", edaFile, ".svg"))
    suppressMessages(ggsave(file=edaFile, plot=g, width=8.5, height=11))
  }
  
  return (g)
}


##### Multivariate Visual EDA: see above


##### EDA MAIN #####


message("\n===== Starting Exploratory Data Analysis (EDA)...")

fileName <- paste0(READY4EDA_FILE, RDS_EXT)
ready4edaFile <- file.path(READY4EDA_DIR, fileName)

# Create results directory, if it doesn't exist
if (!file.exists(EDA_RESULTS_DIR)) {
  dir.create(EDA_RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)
}

# load data
message("\nLoading data...")
flossData <- loadData(ready4edaFile)

# construct list of indicators & corresponding extra functions
sfIndicators <- c("prjAge", "devTeamSize",
                  "prjLicense", "prjMaturity")
sfColumnNames <- c("Project.Age", "Development.Team.Size",
                   "Project.License", "Project.Stage")
sfExtraFun <- list("projectAge", "devTeamSize",
                   "projectLicense", "projectMaturity")

# sequentially call EDA functions for all indicators in data source
silent <- lapply(seq_along(sfIndicators), function(i) {
  performEDA(flossData, sfIndicators[[i]], sfColumnNames[[i]],
             sfExtraFun[[i]])
})


if (!KNITR) {
  edaFilePDF <- file.path(EDA_RESULTS_DIR, "eda-univar.pdf")
  mg <- do.call(marrangeGrob, c(allPlots, list(nrow=2, ncol = 1)));
  suppressMessages(ggsave(filename=edaFilePDF, mg, width=8.5, height=11))
}

message("\n===== Univariate EDA completed, results can be found ",
        "in directory \"", EDA_RESULTS_DIR, "\"\n")

# -- ## @knitr DoNotUse
# -- stop('OK! Intentionally stopped to prevent code from running.')

# define data sources
dataSourcesList <- c("SourceForge")  # TODO: add "FLOSSmole", when ready

# sets of indicators for multivariate EDA per data source
indicators <- c()
indicators[["SourceForge"]] <- c("Project.Age",
                                 "Project.License",
                                 "License.Category",
                                 "License.Restrictiveness",
                                 "Development.Stage",
                                 "Project.Stage",
                                 "Development.Team.Size",
                                 "User.Community.Size",
                                 "Software.Type")

indicators[["FLOSSmole"]] <- c()

# TODO: Options for converting multi-word var/column names
#       to sysntactically valid names:
# 1) names(df) <- make.names(names(df))
# 2) TBD

for (dataSource in dataSourcesList)
  performMultiEDA(flossData, dataSource, indicators[[dataSource]])

# TODO: produce 'multiPlots'
#if (!KNITR) {
#edaFilePDF <- file.path(EDA_RESULTS_DIR, "eda-multivar.pdf")
#pdf(edaFilePDF)
#silent <- lapply(multiPlots, print)
#}

message("\n===== Multivariate EDA completed, results can be found ",
        "in directory \"", EDA_RESULTS_DIR, "\"\n")


##### "EXTRA" (CUSTOMIZATION) FUNCTIONS #####

## @knitr CustomFunctions

projectAge <- function (df, var) {}

projectLicense <- function (df, var) {}

devTeamSize <- function (df, var) {}

projectMaturity <- function (df, var) {}
