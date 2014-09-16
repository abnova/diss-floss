library(ggplot2)
library(scales)
library(RColorBrewer)
library(mixtools)

NUM_COMPONENTS <- 2

set.seed(12345) # for reproducibility

data(diamonds, package='ggplot2')  # use built-in data
myData <- diamonds$price

calc.component <- function(x, lambda, mu, sigma) {
  lambda * dnorm(x, mean = mu, sd = sigma)
}


hist_with_density <- function(data, func, start = NULL) {
  
  # fit density to data
  # extract 'k' components from mixed distribution 'data'
  mix <- normalmixEM(data, k = NUM_COMPONENTS,
                     maxit = 100, epsilon = 0.01)
  summary(mix)
  
  DISTRIB_COLORS <- brewer.pal(numComponents, "Set1")
  
  # plot histogram, empirical and fitted densities
  g <- "qplot(data, geom = `blank`)"
  
  for (i in length(mix$lambda)) {
    args <- list(lambda = mix$lambda[i], mu = mix$mu[i], sigma = mix$sigma[i])
    g <- paste(g, "stat_function(fun = func, args = args, aes(color = ",
               DISTRIB_COLORS[i], ")) +\n")
  }
  
  tail <- 
  "geom_line(aes(y = ..density..,colour = `Empirical`),stat = `density`) +
    geom_histogram(aes(y = ..density..), alpha = 0.4) +
    scale_colour_manual(name = ``, values = c(`red`, `blue`)) + 
    theme(legend.position = `top`, legend.direction = `horizontal`)"
  
  g <- paste(g, tail)
  gr <- eval(parse(text = g))
  return (gr)
}

hist_with_density(log10(myData),
                  'calc.component', start = list(mean = 0, sd = 1))
