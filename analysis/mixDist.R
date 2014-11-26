# Don't clear R environment, since this module gets sourced?

if (!suppressMessages(require(mclust))) install.packages('mclust')
if (!suppressMessages(require(mixtools))) install.packages('mixtools')

library(mclust)
library(mixtools)

source(file.path(PRJ_HOME, "config/diss-floss-config.R"))
source(file.path(PRJ_HOME, "utils/platform.R"))

NUM_ITERATIONS <- 500
CHANGE_RATE <- 0.01

set.seed(100) # for reproducibility


mixDistAnalysis <- function (df, indicator, colName) {
  
  myData <- df[[colName]]
  
  mix <- determineMixtures(myData)
  assessMixGoF(myData, mix)
  gMixPlot <- visualizeMixtures(myData, mix, indicator, colName)

  g_var <- paste0("mixplot_", "mp_", colName)
  
  # save mixture analysis results visualization
  
  if (KNITR) {  # export plot object for knitr report
    
    assign(g_var, gMixPlot, envir = .GlobalEnv)
    myPlot <- get(g_var, envir = .GlobalEnv)
    myList <- list(myPlot)
    names(myList) <- g_var
    allPlots <<- c(allPlots, myList)
    
  } else {  # save plot object into a file
    
    fMixPlot <- file.path(EDA_RESULTS_DIR, paste0(g_var, ".svg"))
    svg(fMixPlot, height = 7, width = 7)
    print(gMixPlot)
    dev.off()
  }
}


# Using method of model-based clustering, classification and
# density estimation, based on finite normal mixture modeling.
# Using 'mclust' package (http://www.jstatsoft.org/v18/i06/paper;
# http://www.stat.washington.edu/research/reports/2012/tr597.pdf).

determineMixtures <- function (myData) {
  
  # Note that we log transform data for passing to mclustBIC() and
  # normalmixEM(), as these functions can work only with normal data.
  # We use natural log transformation as opposed to other log bases
  # because plnorm() is based on natural log transformation.
  
  # selecting the number of components
  
  message("\nDetermining mixture components ",
          "by using model-based clustering...")
  
  mc <- mclustBIC(log(myData))
  bicDeltas <- diff(diff(mc[,1]/max(mc[,1])))
  # now bicDelta contains differences between the rate of change for BIC
  
  # count components, whose rate of change is higher than heuristic
  numComponents <- length(bicDeltas[bicDeltas > CHANGE_RATE])
  message("Number of mixture components determined: ", numComponents)
  
  message("\nExtracting mixture components...\n")
  
  # extract 'k' components from mixture distribution 'myData'
  mix <- normalmixEM(log(myData), k = numComponents,
                     maxit = NUM_ITERATIONS, epsilon = 0.01)
  print(summary(mix))
  return (mix)
}


##### Assessment of Goodness-of-Fit (GoF)

# CDF for mixture of normals (any number)
mix_pnorm <- function(q, mean, sd, lambda) {
  Reduce(`+`, lapply(seq_along(mean), function(i)
    pnorm(q, mean = mean[i], sd = sd[i]) * lambda[i]))
}

# CDF for mixture of log-normals (any number)
mix_plnorm <- function(q, mean, sd, lambda) {
  Reduce(`+`, lapply(seq_along(mean), function(i)
    plnorm(q, mean = mean[i], sd = sd[i]) * lambda[i]))
}


# Count data values, converted from density,
# for mixture of log-normals (any number)
calc.components <- function (x, mix, comp.number, n, binwidth) {
  
  n * binwidth * mix$lambda[comp.number] *
    dlnorm(x, mean = mix$mu[comp.number], sd = mix$sigma[comp.number])
}


assessMixGoF <- function (myData, mix) {
  
  message("Assessing the solution's goodness-of-fit (GoF)...\n")
  
  # use Kolmogorov-Smirnov (KS) test to assess GoF
  ks.info <- suppressWarnings(ks.test(myData, mix_plnorm,
                     mean = mix$mu, sd = mix$sigma, lambda = mix$lambda))
  #print(ks.info)
  
  # D-value being low enough indicates a good fit
  # (in this case, D-value indicates less then 5% deviation
  #  between the data dstribution and a fitted mixture).
  # P-value being high enough indicates the same.
  fit.deviation <- ks.info$statistic * 100
  fit.dev.str <- sprintf("%.2f", fit.deviation)
  if (ks.info$statistic < 0.05 || ks.info$p.value > 0.05)
    message("\nKS test confirmed a good fit of calculated mixture to ",
            "\nthe data distribution (", fit.dev.str, "% of deviation).")
  else
    message("\nKS test confirmed an absense of good fit of calculated mixture ",
            "\nto the data distribution (", fit.dev.str, "% of deviation).")
}


visualizeMixtures <- function (data, mix, indicator, colName) {
  
  title <- paste("Projects distribution across", colName,
                 "range (mixture analysis)")
  xLabel <- colName
  
  if (identical(colName, "Project.Age"))
    xLabel <- paste(colName, "(months)")
  
  breaks <- pretty(range(data), n = nclass.FD(data), min.n = 1)
  bwidth <- (breaks[2] - breaks[1]) / 2
  
  # TODO: consider covering both original and log-transformed data
  # in terms of plotting corresponding mixture distributions
  #if (log) bwidth <- bwidth/100
  
  # handle platform's version differences for 'ggplot2' API
  if (compareVersion(GGPLOT2_VER, "0.9.1") == 1)  # later version
    myTitle <- ggtitle(label=title)
  else
    myTitle <- opts(title=title)
  
  # build the plot
  g <- ggplot(data.frame(x = data)) +
    scale_fill_continuous("Number of\nprojects",
                          low="#56B1F7", high="#132B43") + 
    scale_x_continuous(xLabel) +
    scale_y_continuous("Number of projects") +
    geom_histogram(aes(x = x, y = ..count.., fill = ..count..),
                   binwidth = bwidth)
  
  # we could select needed number of colors randomly:
  #DISTRIB_COLORS <- sample(colors(), numComponents)
  
  # or, better, use a palette with more color differentiation:
  numComponents <- length(mix$mu)

  # set color for components: currently, single color for all
  DISTRIB_COLORS <- rep("red", numComponents)
  
  #DISTRIB_COLORS <- suppressWarnings(brewer.pal(numComponents, "Reds"))
  #DISTRIB_COLORS <- suppressWarnings(brewer.pal(numComponents, "Set1"))
  
  if (FALSE) {  # an attempt to dynamically change color, if in "Blues" range
    DISTRIB_COLORS <- unlist(lapply(DISTRIB_COLORS, function (x) {
      x <- substr(x, 2, nchar(x))
      hVal <- as.hexmode(x)
      if (hVal < as.hexmode("000066") && hVal > as.hexmode("66CCCC"))
        hVal <- hVal - 10  # TBD: delta for color shift (blue -> other)
      hVal <- as.character(as.hexmode(hVal))
    }))
  }
  
  distComps <- lapply(seq(numComponents), function(i)
    stat_function(fun = calc.components,
                  arg = list(mix = mix, comp.number = i,
                             length(data), bwidth),
                  geom = "line",
                  size = 1,
                  color = DISTRIB_COLORS[i]))
  
  g <- g + distComps
  
  if (.Platform$GUI == "RStudio") print(g)
  
  if (!KNITR) {
    edaFile <- str_replace_all(string=colName, pattern=" ", repl="")
    edaFile <- file.path(EDA_RESULTS_DIR, paste0(edaFile, "-mix", ".svg"))
    suppressMessages(ggsave(file=edaFile, plot=g, width=8.5, height=11))
  }
  
  # title will be saved as & extracted from an attribute (for fig. caption)
  attr(g, "title") <- myTitle
  
  return (g)
}