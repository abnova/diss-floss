library(ggplot2)
library(RColorBrewer)
library(mixtools)

source("../utils/color.R")

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


plotMixedDist <- function (data, mix.info, numComponents,
                           colName) {
  
  title <- paste("Projects distribution across", colName, "range")
  xLabel <- colName
  
  if (identical(colName, "Project Age"))
    xLabel <- paste(colName, "(months)")
  
  #df <- data
  g <- ggplot(data.frame(x = data)) +
    scale_fill_continuous("Number of\nprojects") + 
    #scale_x_log10(xLabel) +
    #scale_y_log10("Number of projects") +
    scale_x_continuous(xLabel) +
    scale_y_continuous("Number of projects") +
    scale_fill_gradient(low="#99CCFF", high="#003366") +
    ggtitle(label=title) +
    geom_histogram(aes(x = x, fill = ..count..), # y = ..count..
                   #fill = "white", color = "black",
                   binwidth = 0.01,
                   position = "identity") # 0.5
  print(g)
  
  # we could select needed number of colors randomly:
  #DISTRIB_COLORS <- sample(colors(), numComponents)
  
  # or, better, use a palette with more color differentiation:
  DISTRIB_COLORS <- brewer.pal(numComponents, "Set1")
  
  distComps <- lapply(seq(numComponents), function(i)
    stat_function(#aes(data=data, x=x, y=..density..),
                  fun = calc.components,
                  arg = list(mix = mix.info, comp.number = i),
                  geom = "line", # density / use alpha=.5 for "polygon"
                  position = "identity",
                  size = 1,
                  color = DISTRIB_COLORS[i]))
  return (g + distComps)
}
