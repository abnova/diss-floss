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


overlayHistDensity <- function(data, func) {
  # extract 'k' components from mixed distribution 'data'
  mix <- normalmixEM(data, k = NUM_COMPONENTS,
                     maxit = 100, epsilon = 0.01)
  summary(mix)
  
  DISTRIB_COLORS <- 
    suppressWarnings(brewer.pal(NUM_COMPONENTS, "Set1"))
  
  # plot histogram, empirical and fitted densities
  g <- "ggplot(as.data.frame(data), aes(x = data)) +\n"
  
  for (i in seq(length(mix$lambda))) {
    args <- paste0("args.", i)
    assign(args, list(lambda = mix$lambda[i], mu = mix$mu[i],
                      sigma = mix$sigma[i]))
    g <- paste0(g,
                "stat_function(fun = func, args = ",
                args,
                ", color = '",
                DISTRIB_COLORS[i], "', size = 2) +\n")
  }
  
  # geom_line(aes(y = ..density..,colour = 'Empirical'),stat = 'density') +
  # scale_colour_manual(name = '', values = c('red', 'blue')) +
    
  tailStr <- "geom_histogram(aes(y = ..density..),
                             binwidth = 0.01, alpha = 0.5) +
  theme(legend.position = 'top', legend.direction = 'horizontal')"
  
  g <- paste0(g, tailStr)
  gr <- eval(parse(text = g))
  return (gr)
}

overlayPlot <- overlayHistDensity(log10(myData), 'calc.component')
print(overlayPlot)
