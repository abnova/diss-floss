library(ggplot2)
library(scales)
library(RColorBrewer)
library(mixtools)

NUM_COMPONENTS <- 2

set.seed(12345) # for reproducibility

data(diamonds, package='ggplot2')  # use built-in data
myData <- diamonds$price


calc.components <- function(x, mix, comp.number) {

  mix$lambda[comp.number] *
    dnorm(x, mean = mix$mu[comp.number], sd = mix$sigma[comp.number])
}


overlayHistDensity <- function(data, calc.comp.fun) {
  
  # extract 'k' components from mixed distribution 'data'
  mix.info <- normalmixEM(data, k = NUM_COMPONENTS,
                          maxit = 100, epsilon = 0.01)
  summary(mix.info)
  
  numComponents <- length(mix.info$sigma)
  message("Extracted number of component distributions: ",
          numComponents)
  
  DISTRIB_COLORS <- 
    suppressWarnings(brewer.pal(NUM_COMPONENTS, "Set1"))
  
  # create (plot) histogram and ...
  g <- ggplot(as.data.frame(data), aes(x = data)) +
    geom_histogram(aes(y = ..density..),
                   binwidth = 0.01, alpha = 0.5) +
    theme(legend.position = 'top', legend.direction = 'horizontal')
  
  comp.labels <- lapply(seq(numComponents),
                        function (i) paste("Component", i))
  
  # ... fitted densities of components
  distComps <- lapply(seq(numComponents), function (i)
    stat_function(fun = calc.comp.fun,
                  args = list(mix = mix.info, comp.number = i),
                  size = 2, color = DISTRIB_COLORS[i]))

  legend <- list(scale_colour_manual(name = "Legend:",
                                     values = DISTRIB_COLORS,
                                     labels = unlist(comp.labels)))
    
  return (g + distComps + legend)
}

overlayPlot <- overlayHistDensity(log10(myData), 'calc.components')
print(overlayPlot)
