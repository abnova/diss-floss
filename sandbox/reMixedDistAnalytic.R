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
library(mclust)
library(MASS)
library(mixtools)
library(ddst)
library(rebmix)

NUM_COMPONENTS <- 2
NUM_ITERATIONS <- 100

set.seed(12345) # for reproducibility

data(diamonds, package='ggplot2')  # use built-in data
myData <- diamonds$price
myData <- log10(myData)

# basic distribution fitting ('MASS' package)
#fitdistr(myData,"gamma")

# selecting the number of components

# Using method of model-based clustering, classification and
# density estimation, based on finite normal mixture modeling.
# Using 'mclust' package (http://www.jstatsoft.org/v18/i06/paper;
# http://www.stat.washington.edu/research/reports/2012/tr597.pdf).
mc <- Mclust(myData)
print(summary(mc))

bestModel <- mclustModel(myData, mc)
print(summary(bestModel))

# using 'mixtools' package
#mix.sel <- multmixmodel.sel(myData, comps = 1:4, epsilon = 0.01)
#print(mix.sel)

# determine number of components in mixture distribution
# by hypothesis testing via parametric bootstrap
mix.boot <- boot.comp(myData, max.comp = 10, mix.type = "normalmix",
                      maxit = 400, epsilon = 0.01)
print(summary(mix.boot))

# extract 'k' components from mixed distribution 'data'
mix <- normalmixEM(myData, k = NUM_COMPONENTS,
                   maxit = NUM_ITERATIONS, epsilon = 0.01)
summary(mix)

numComponents <- length(mix$sigma)
message("Extracted number of component distributions: ",
        numComponents)

##### TESTING

# CDF of mixture of two log-normal distributions
pmlnorm <- function(x, meanlog, sdlog, pmix) {
  
  pmix[1] * plnorm(x, meanlog[1], sdlog[1]) + 
    (1 - pmix[1]) * plnorm(x, meanlog[2], sdlog[2])
}

# Kolmogorov-Smirnov (K-S) test
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


# Anderson-Darling test


# smooth test of goodness-of-fit
ddst.info <- ddst.norm.test(myData, compute.p = FALSE)
print(ddst.info)


# REBMIX
dist.est <- REBMIX(Dataset = list(data.frame(myData)),
                   Preprocessing = "histogram",
                   cmax = 8, Criterion = c("AIC", "BIC"),
                   Variables = "continuous",
                   pdf = "gamma", K = 30:80)
summary(dist.est)

plot(dist.est, pos = 1, what = c("den", "dis"), ncol = 2, npts = 1000)

coef(dist.est)
numComps <- as.numeric(dist.est$summary$c)
summary(numComps)

# parametric bootstraping to determine number of components
dist.boot <- boot.REBMIX(x = dist.est, pos = 1,
                         Bootstrap = "p", B = 100,
                         n = NULL, replace = TRUE, prob = NULL)
summary(dist.boot)
