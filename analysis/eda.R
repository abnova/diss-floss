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

library(RCurl)
library(stringr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(gridExtra)
library(psych)
library(polycor)
library("GGally")

## @knitr PrepareEDA
PRJ_HOME <- Sys.getenv("DISS_FLOSS_HOME")

KNITR <<- isTRUE(getOption("knitr.in.progress"))

source(file.path(PRJ_HOME, "utils/factors.R"))
source(file.path(PRJ_HOME, "utils/qq.R"))
source(file.path(PRJ_HOME, "utils/data.R"))
source(file.path(PRJ_HOME, "utils/utils.R"))
source(file.path(PRJ_HOME, "utils/mixedDist.R"))

READY4EDA_DIR  <- file.path(PRJ_HOME, "data/ready4eda")
READY4EDA_FILE <- "flossData" # default
RDS_EXT <- ".rds"

# temporary, until implementing EDA for single data file
TRANSFORM_DIR <- file.path(PRJ_HOME, "data/transformed")

EDA_RESULTS_DIR <- file.path(PRJ_HOME, "results/eda")

DIST_FIT_COLOR <- "green" # ggplot2 line color for distrib. fitting

DEBUG <- TRUE # TODO: retrieve debug flag via CL arguments

allPlots <- list()

## @knitr PerformEDA

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
  
  datasetName <- deparse(substitute(df))
  
  # it's possible to customize describe()'s output as follows:
  # describe(mtcars)[, c(2, 3, 4, 5, 8, 9)]
  
  if (KNITR) {
    describe_var <- paste0("describe_", datasetName)
    assign(describe_var, describe(df), envir = .GlobalEnv)
  } else {
    message("\nDecriptive statistics for '", datasetName, "':\n")
    # suppress "NAs introduced by coercion" warnings
    suppressWarnings(print(describe(df)))
  }
}


correlationAnalysis <- function (df) {
  
  names(df) <- make.names(names(df))

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
    flossDataTest[setdiff(names(flossDataTest), "License Category")]
  
  mvn.result <- MVN::mardiaTest(flossDataTest, cov = TRUE, qqplot = FALSE)
  print(mvn.result)
  
  mvn.result <- MVN::hzTest(flossDataTest, cov = TRUE, qqplot = FALSE)
  print(mvn.result)
  
  mvn.result <- MVN::roystonTest(flossDataTest, qqplot = FALSE)
  print(mvn.result)
}


multiAnalyticalEDA <- function (df, indicators) {
  
  corrMat <- correlationAnalysis(df)
  mvnTests(df)
  
  return (corrMat)
}


multiVisualEDA <- function (df, corrMat) {

  names(df) <- make.names(names(df))

  # log transform continuous data
  df["Project.Age"] <- log(df["Project.Age"])
  df["Development.Team.Size"] <- log(df["Development.Team.Size"])
  df["User.Community.Size"] <- log(df["User.Community.Size"])

  # drop this indicator to match dimensions of the hetcor()'s results
  df <- df[setdiff(names(df), "License.Category")]
  
  g1 <- ggpairs(df, title = "Pairwise Scatterplots",
                lower=list(continuous = "smooth", combo="box",
                           discrete="ratio", params = c(colour = "blue")),
                upper=list(params = list(corSize = 6), combo=""),
                diag=list(continuous = "bar", params = c(colour = "blue")),
                axisLabels = "show")

  # generate custom panels with corr. coefficients from hetcor()
  
  # index of upper triangle rows and columns
  # replace r with hetcor matrix
  index <- which(upper.tri(corrMat), arr.ind = TRUE)
  
  # loop through upper triangle and replace
  for (i in 1:nrow(index)) {
    g1 <- putPlot(g1,
                  ggally_text(sprintf("Corr:\n%0.2f",
                                      corrMat[index[i, 1],
                                              index[i, 2]])),
                  index[i, 1], index[i, 2])
  }
  
  # print customized plot
  print(g1)
  stop("Intended stop!")
  
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


performEDA <- function (dataSource, indicator, colName, extraFun) {
  
  fileName <- paste0(indicator, RDS_EXT)
  rdataFile <- file.path(TRANSFORM_DIR, dataSource, fileName)
  if (file.exists(rdataFile)) {
    data <- readRDS(rdataFile)
  }
  else {
    stop("RDS file for \'", indicator, "\' not found! ",
         "Run 'make' first.")
  }
  
  uniDescriptiveEDA(data, indicator, colName, extraFun)
  uniVisualEDA(data, indicator, colName, extraFun)
  # TODO: Integrate mixture analysis from 'sandbox'
  #fitDistParam(data, indicator, colName, extraFun)
  #fitDistNonParam(data, indicator, colName, extraFun)

  rm(data)
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
  pmFactor <- as.name(flossData[["Project Stage"]])
  levels(pmFactor)[nlevels(pmFactor)] <- NA
  
  # perform multivariate EDA
  multiDescriptiveEDA(flossData)
  corrMat <- multiAnalyticalEDA(flossData)
  multiVisualEDA(flossData, corrMat$correlations)
}


##### VISUAL EDA #####

##### Univariate Visual EDA

# Plot distribution of a continuous variable "colName"
plotHistogram <- function (df, colName, log = FALSE, print = TRUE) {
  
  df <- df
  df$var <- df[[colName]]
  df <- na.omit(df)
  if (log) {
    if (any(df$var < 0)) df$var <- df$var + abs(min(df$var)) + 0.01
    # instead of log transforming data directly,
    # we do that further via ggplot2's scales functionality
    # df$var <- log(df$var)
  }
  upperLimit <- max(df$var)
  
  title <- paste("Projects distribution across", colName, "range")
  xLabel <- colName
  
  if (identical(colName, "Project Age"))
    xLabel <- paste(xLabel, "(months)")
  
  if (log) {
    xLabel <- paste(xLabel, "[Log]")
    scale_x <-
      scale_x_continuous(xLabel,
                         trans = "log",
                         #limit = c(1, upperLimit),
                         breaks = trans_breaks("log10", function(x) 10^x),
                         labels = prettyNum)
  } else {
    scale_x <-
      scale_x_continuous(xLabel,
                         #limit = c(0, upperLimit),
                         #breaks = trans_breaks("log10", function(x) 10^x),
                         labels = prettyNum)
  }
  
  g <- ggplot(df, aes(x = var)) +
    scale_fill_continuous("Number of\nprojects",
                          low = "#56B1F7", high = "#132B43") + 
    scale_x +
    scale_y_continuous("Number of projects",
                       #limit = c(1, upperLimit),
                       #breaks = trans_breaks("log10", function(x) 10^x),
                       labels = prettyNum) +
  ggtitle(label=title)
  
  breaks <- pretty(range(df$var), n = nclass.FD(df$var), min.n = 1)
  bwidth <- (breaks[2] - breaks[1]) / 2
  if (log) bwidth <- bwidth/100
  
  # Use (..density..) * bwidth IF want to match y-range with kernel density
  g <- g + geom_histogram(aes(fill = ..count..), # y = ..density..
                          binwidth = bwidth, #0.01, #bwidth
                          position = "identity")
  
  # Overlay with transparent density plot
  #g <- g + geom_density(alpha = .2, fill = "#FF6666")

  mean <- ifelse(log, mean(log(df$var)), mean(df$var))
  sd <- ifelse(log, sd(log(df$var)), sd(df$var))
  
  # Overlay with density-like plot, based on data count
  g <- g + stat_function(fun = dnorm.count, 
                         args = list(mean = mean,
                                     sd = sd,
                                     n = length(df$var),
                                     binwidth = bwidth),
                         color = "red")
  
  # Ignore NA values for mean
  g <- g + geom_vline(aes(xintercept=mean(var, na.rm = TRUE)),
                      linetype = "longdash", color = "red")
  
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
  df <- na.omit(df)
  
  title <- paste("Projects distribution across", colName,
                 "range (by category)")
  xLabel <- colName
  
  breaks <- pretty(range(df$var), n = nclass.FD(df$var), min.n = 1)
  bwidth <- breaks[2] - breaks[1]
  
  g <- ggplot(df, aes(x=var, fill=var)) +
    geom_density(aes(y=..count..), 
                 binwidth=bwidth, position="identity")
  
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
  df <- na.omit(df)
  
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
  
  # df[!is.na(df$var), ]
  g <- ggplot(df, aes(x=var, fill=var)) +
    geom_bar(stat="bin", position="identity") +
    scale_fill_discrete(colName) + 
    xlab(colName) +
    ylab("Number of projects") +
    ggtitle(label=title)
  
  # display pre-calculated percentage on top of bars
  if (FALSE) {
    g <- g +
      geom_text(data=dfTab,aes(x=x,y=Freq,label=lab),vjust=0) +
      theme(axis.text.x=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),legend.title=element_blank(),
            axis.title.y=element_blank())  
  }
  
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
    scale_y_continuous("Sample Quantiles") +
    ggtitle(label=title)
  
  if (.Platform$GUI == "RStudio") print(g)
  
  #TODO: consider moving to main
  if (!KNITR) {
    edaFile <- str_replace_all(string=colName, pattern=" ", repl="")
    edaFile <- file.path(EDA_RESULTS_DIR, paste0("QQ-", edaFile, ".svg"))
    suppressMessages(ggsave(file=edaFile, plot=g, width=8.5, height=11))
  }
  
  return (g)
}


##### Multivariate Visual EDA


scatterPlot <- function (df, xName, yName, facetVar) {
  
  df <- df
  df <- na.omit(df)
  df$x <- df[[xName]]
  df$y <- df[[yName]]
  
  datasetName <- deparse(substitute(df))
  title <- paste0("Scatterplot for '", datasetName, "' with faceting")
  xLabel <- colName
  
  g <- ggplot(df, aes(x, y)) + geom_point() + facet_grid(.~facetVar)
  
  if (.Platform$GUI == "RStudio") print(g)
  
  #TODO: consider moving to main
  if (!KNITR) {
    edaFile <- str_replace_all(string=colName, pattern=" ", repl="")
    edaFile <- file.path(EDA_RESULTS_DIR, paste0(edaFile, ".svg"))
    suppressMessages(ggsave(file=edaFile, plot=g, width=8.5, height=11))
  }
  
  return (g)
}


##### EDA MAIN #####


message("\n===== Starting Exploratory Data Analysis (EDA)...")

fileName <- paste0(READY4EDA_FILE, RDS_EXT)
ready4edaFile <- file.path(READY4EDA_DIR, fileName)

# load data
message("\nLoading data...")
flossData <- loadData(ready4edaFile)

# construct list of indicators & corresponding extra functions
sfIndicators <- c("prjAge", "devTeamSize",
                  "prjLicense", "prjMaturity")
sfColumnNames <- c("Project Age", "Development Team Size",
                   "Project License", "Project Stage")
sfExtraFun <- list("projectAge", "devTeamSize",
                   "projectLicense", "projectMaturity")

# sequentially call EDA functions for all indicators in data source
silent <- lapply(seq_along(sfIndicators), function(i) {
  performEDA("SourceForge", sfIndicators[[i]], sfColumnNames[[i]],
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
indicators[["SourceForge"]] <- c("Project Age",
                                 "Project License",
                                 "License Category",
                                 "License Restrictiveness",
                                 "Development Stage",
                                 "Project Stage",
                                 "Development Team Size",
                                 "User Community Size",
                                 "Software Type")

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
