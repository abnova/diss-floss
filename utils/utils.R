DEBUG_KS <- FALSE

DVAL_LIM <- 0.40
PVAL_LIM <- 0.40


# return character representation of a specified argument of a function 'fun'
getArgValue <- function (fun, argName) {

  argValue <- as.character(as.list(fun)[[argName]])
}


# return value of an attribute of an object stored in a file

getAttr <- function (attr, obj, file) 
{
  x <- load(file, envir = environment())
  attrs <- attributes(obj)
  return (list(file=x, y=y))
}


# convert normal density to counts (for histogram overlaying plots);
# supports log transformation, but result isn't the same as using dlnorm()
dnorm.count <- function(x, mean = 0, sd = 1,
                        log = FALSE, n = 1, binwidth = 1,
                        logScale = FALSE) {  # y-axis scale
  
  #mult <- n
  #if (logScale) mult <- log10(n + 1)

  result <- n * binwidth * dnorm(x = x, mean = mean, sd = sd, log = log)
  if (logScale)
    result <- log10(result + 1)
  
  return (result)
}


dist.count <- function (x, distInfo, n, binwidth, logScale) {
  
  distName <- distInfo$name
  distObj <- distInfo$obj[[1]]

  if (distName == "normal") {
    val <- dnorm(x, distObj$estimate[1], distObj$estimate[2])
  } else if (distName == "exponential") {
    val <- dexp(x, distObj$estimate[1])
  } else if (distName == "gamma") {
    val <- dgamma(x, distObj$estimate[1], distObj$estimate[2])
  } else if (distName == "Poisson") {
    val <- dpois(x, distObj$estimate)
  } else if (distName == "weibull") {
    val <- dweibull(x, distObj$estimate[1], distObj$estimate[2])
  } else {
    stop("Unknown distribution requested!")
  }
  
  result <- n * binwidth * val
  if (logScale)
    result <- log10(result + 1)
  
  return (result)
}


# TBD: Consider adding conditions for particular dist. functions
findOptimalDist <- function (x) {
  
  if (!suppressMessages(require(MASS))) install.packages('MASS')
  library(MASS)
  
  distList <- c("exponential", "gamma", "Poisson", "weibull", "geometric")
  distFuns <- c("dexp", "dgamma", "dpois", "dweibull", "dgeom")
  fit <- ks.info <- c()
  optimDist <- "No optimal distribution found"
  
  for (dist in distList) {

    # determine optimal fit for the data distribution
    fit[[dist]] <- fitdistr(x, dist)

    # use Kolmogorov-Smirnov (KS) test to assess GoF
    if (dist == "normal") {
      ks.info[[dist]] <-
        suppressWarnings(ks.test(x, pnorm,
                                 fit[[dist]]$estimate[1],
                                 fit[[dist]]$estimate[2]))
      if (DEBUG_KS) print(ks.info)
    } else if (dist == "exponential") {
      ks.info[[dist]] <-
        suppressWarnings(ks.test(x, pexp,
                                 fit[[dist]]$estimate[1]))
      if (DEBUG_KS) print(ks.info)
    } else if (dist == "gamma") {
      ks.info[[dist]] <-
        suppressWarnings(ks.test(x, pgamma,
                                 fit[[dist]]$estimate[1],
                                 fit[[dist]]$estimate[2]))
      if (DEBUG_KS) print(ks.info)
    } else if (dist == "Poisson") {
      ks.info[[dist]] <-
        suppressWarnings(ks.test(x, ppois,
                                 fit[[dist]]$estimate[1]))
      if (DEBUG_KS) print(ks.info)
    } else if (dist == "weibull") {
      ks.info[[dist]] <-
        suppressWarnings(ks.test(x, pweibull,
                                 fit[[dist]]$estimate[1],
                                 fit[[dist]]$estimate[2]))
      if (DEBUG_KS) print(ks.info)
    } else if (dist == "geometric") {
      ks.info[[dist]] <-
        suppressWarnings(ks.test(x, pgeom,
                                 fit[[dist]]$estimate[1],
                                 fit[[dist]]$estimate[2]))
      if (DEBUG_KS) print(ks.info)
    } else {
      stop("Unknown distribution requested!")
    }
  }

  optimStatIdx <- which.min(sapply(ks.info, `[[`, "statistic"))
  optimPvalIdx <- which.max(sapply(ks.info, `[[`, "p.value"))
  
  fit.deviation <- ks.info[[optimStatIdx]]$statistic * 100
  fit.dev.str <- sprintf("%.2f", fit.deviation)
  if (ks.info[[optimStatIdx]]$statistic < DVAL_LIM ||
        ks.info[[optimPvalIdx]]$p.value > PVAL_LIM) {
    message("\nKS test confirmed a good fit to the data distribution (",
            fit.dev.str, "% of deviation).")
    optimDist <- distList[optimStatIdx]
  }
  else
    message("\nKS test confirmed an absense of good fit",
            "\nto the data distribution (", fit.dev.str, "% of deviation).")

  message("\nOptimal distribution determined: ", optimDist)
  
  return (list(name=optimDist, obj=fit[optimStatIdx]))
}