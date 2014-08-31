library(ggplot2)
library(RColorBrewer)
library(mixtools)


set.seed(12345) # for reproducibility

fitMixDist <- function (data, num.components) {
  
  # extract 'k' components from mixed distribution 'data'
  mix.info <- normalmixEM(data, k = num.components,
                          maxit = 100, epsilon = 0.01)
  summary(mix.info)
  
  numComponents <- length(mix.info$sigma)
  message("Extracted number of component distributions: ",
          numComponents)
  return (mix.info)
}


calc.components <- function (x, mix, comp.number) {
  
  mix$lambda[comp.number] *
    dnorm(x, mean = mix$mu[comp.number], sd = mix$sigma[comp.number])
}


plotMixedDist <- function (data, mix.info, numComponents) {
  
  g <- ggplot(data.frame(x = data)) +
    geom_histogram(aes(x = data, y = ..density..),
                   fill = "white", color = "black", binwidth = 0.5)
  
  # we could select needed number of colors randomly:
  #DISTRIB_COLORS <- sample(colors(), numComponents)
  
  # or, better, use a palette with more color differentiation:
  DISTRIB_COLORS <- brewer.pal(numComponents, "Set1")
  
  distComps <- lapply(seq(numComponents), function(i)
    stat_function(mapping = aes(names(data)[1]),
                  fun = calc.components,
                  arg = list(mix = mix.info, comp.number = i),
                  geom = "line", # use alpha=.5 for "polygon"
                  size = 2,
                  color = DISTRIB_COLORS[i]))
  return (g + distComps)
}
