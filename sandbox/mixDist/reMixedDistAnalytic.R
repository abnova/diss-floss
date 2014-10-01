# A graphical technique to evaluate the goodness of fit
# can be drawing pdf curve and histogram together (Ricci, 2005, p. 15).
# Ricci, V. (2005). Fitting distributions with R. Retreived from
# http://cran.r-project.org/doc/contrib/Ricci-distributions-en.pdf

# Goodness of fit tests indicate whether or not it is reasonable
# to assume that a random sample comes from a specific distribution.
# They are a form of hypothesis testing where
# the null and alternative hypotheses are (Ricci, 2005, p. 16):
#   H0: Sample data come from the stated distribution
#   HA: Sample data do not come from the stated distribution

#library(ggplot2)
#library(scales)
#library(RColorBrewer)
#library(ddst)
library(mclust)
library(mixtools)
library(flexmix)

NUM_ITERATIONS <- 500
CHANGE_RATE <- 0.01

set.seed(12345) # for reproducibility

data(diamonds, package='ggplot2')  # use built-in data
myData <- diamonds$price
myData <- log10(myData)


# selecting the number of components

# Using method of model-based clustering, classification and
# density estimation, based on finite normal mixture modeling.
# Using 'mclust' package (http://www.jstatsoft.org/v18/i06/paper;
# http://www.stat.washington.edu/research/reports/2012/tr597.pdf).

mc <- mclustBIC(myData)
bicDeltas <- diff(diff(mc[,1]/max(mc[,1])))
# now bicDelta contains differences between the rate of change for BIC

numComponents <- length(bicDeltas[bicDeltas > CHANGE_RATE])


# extract 'k' components from mixed distribution 'data'
mix <- normalmixEM(myData, k = numComponents,
                   maxit = NUM_ITERATIONS, epsilon = 0.01)
summary(mix)

numComponents <- length(mix$sigma)
message("Extracted number of component distributions: ",
        numComponents)

##### TESTING of GOODNESS-OF-FIT

# CDF of mixture of two log-normal distributions
pmlnorm <- function(x, meanlog, sdlog, pmix) {
  
  pmix[1] * plnorm(x, meanlog[1], sdlog[1]) + 
    (1 - pmix[1]) * plnorm(x, meanlog[2], sdlog[2])
}


##### Kolmogorov-Smirnov (K-S) test

ks.info <- ks.test(log(myData), pmlnorm,
                   meanlog=mix$mu, sdlog=mix$sigma, pmix=mix$lambda)
print(ks.info)

# Use parametric bootstraping (to partially overcome bias)

# Bootstrap estimation of ks statistic distribution
if (FALSE) {
  
  N <- length(myData)
  
  ks.boot <- rep(0,1000)
  
  for (i in 1:1000) {
    
    z <- rbinom(N, 1, mix$lambda[1])
    
    x.boot <- z * rnorm(N, mix$mu[1], mix$sigma[1]) + 
      (1 - z) * rnorm(N, mix$mu[2], mix$sigma[2])
    
    mix.boot <- normalmixEM(x.boot, maxit = NUM_ITERATIONS,
                            lambda = mix$lambda,
                            mu = mix$mu, sigma = mix$sigma)
    
    ks.boot[i] <- ks.test(x.boot, pmlnorm,
                          mu = mix.boot$mu,
                          sigma = mix.boot$sigma,
                          pmix = mix.boot$lambda)$statistic
  }
  
  ks.info.boot <- mean(ks.info$statistic <= ks.boot)
  print(ks.info.boot)
}


# A bootstrap version of K-S test, based on Monte-Carlo approach
#
# Fits an observed distribution with respect to a lognormal model
# and computes p value, using method described in:
# A. Clauset, C. R. Shalizi, M. E. J. Newman. Power-law distributions
# in empirical data. SIAM Review 51, 661-703 (2009)
# INPUT: 
#   d: the observed distribution to fit
#   limit: the number of synthetic data sets to generate
# OUTPUT
#   meanlog: the meanlog (mu) parameter
#   sdlog: the sdlog (sigma) parameter
#   stat: the KS statistic
#   p: the percentage of time the observed distribution has a KS statistic lower than
#      or equal to the synthetic distribution
#   KSp: the KS p-value
lognormalKS <- function (d, limit = 2500) {
  
  # load MASS package to use fitdistr
  # mle = fitdistr(d, "lognormal")
  # meanlog = mle$estimate["meanlog"]
  # sdlog = mle$estimate["sdlog"]
  
  # MLE for lognormal distribution
  meanlog <- mean(log(d))
  sdlog <- sd(log(d))
  
  # compute KS statistic
  t <- ks.test(d, "plnorm", meanlog = meanlog, sdlog = sdlog)
  
  # compute p-value
  count = 0
  for (i in 1:limit) {
    syn <- rlnorm(length(d), meanlog = meanlog, sdlog = sdlog)
    meanlog2 <- mean(log(syn))
    sdlog2 <- sd(log(syn))
    t2 <- ks.test(syn, "plnorm", meanlog = meanlog2, sdlog = sdlog2)
    if (t2$stat >= t$stat) {count = count + 1}
  }
  
  return (list(meanlog = meanlog, sdlog = sdlog,
                 stat = t$stat, p = count/limit, KSp = t$p))
}

#ks.logn <- lognormalKS(myData, limit = 100)
#print(ks.logn$KSp)


##### Anderson-Darling test

# smooth test of goodness-of-fit
#ddst.info <- ddst.norm.test(myData, compute.p = FALSE)
#print(ddst.info)
