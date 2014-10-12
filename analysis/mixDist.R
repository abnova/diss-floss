# Don't clear R environment, since this module gets sourced

if (!suppressMessages(require(mclust))) install.packages('mclust')
if (!suppressMessages(require(mixtools))) install.packages('mixtools')

library(mclust)
library(mixtools)

NUM_ITERATIONS <- 500
CHANGE_RATE <- 0.01

set.seed(100) # for reproducibility


mixDistAnalysis <- function (df, indicator, colName) {
  
  myData <- df[[colName]]
  
  mix <- determineMixtures(myData)
  assessMixGoF(myData, mix)
  # visualizeMixtures(myData, mix, indicator, colName)
  # TODO: integrate visualizing mixtures, based on "utils/mixedDist.R"
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
  
  message("Determining mixture components ",
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


assessMixGoF <- function (myData, mix) {
  
  message("Assessing the solution's goodness-of-fit (GoF)...\n")
  
  # use Kolmogorov-Smirnov (KS) test to assess GoF
  ks.info <- ks.test(myData, mix_plnorm,
                     mean = mix$mu, sd = mix$sigma, lambda = mix$lambda)
  print(ks.info)
  
  # D-value being low enough indicates a good fit
  # (in this case, D-value indicates less then 5% deviation
  #  between the data dstribution and a fitted mixture).
  # P-value being high enough indicates the same.
  fit.devation <- ks.info$statistic * 100
  if (ks.info$statistic < 0.05 || ks.info$p.value > 0.05)
    message("KS test confirmed a good fit of calculated mixture to ",
            "the data distribution (", fit.deviation, "% of deviation).")
  else
    message("KS test confirmed an absense of good fit of calculated mixture ",
            "to the data distribution (", fit.deviation, "% of deviation).")
}
