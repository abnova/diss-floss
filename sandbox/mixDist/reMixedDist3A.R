library(ggplot2)
library(scales)
library(RColorBrewer)
library(mixtools)

NUM_COMPONENTS <- 2

set.seed(12345) # for reproducibility

data(diamonds, package='ggplot2')  # use built-in data
myData <- diamonds$price

# extract 'k' components from mixed distribution 'data'
mix.info <- normalmixEM(myData, k = NUM_COMPONENTS,
                        maxit = 100, epsilon = 0.01)
summary(mix.info)

numComponents <- length(mix.info$sigma)
message("Extracted number of component distributions: ",
        numComponents)

calc.components <- function(x, mix, comp.number) {
  
  mix$lambda[comp.number] *
    dnorm(x, mean = mix$mu[comp.number], sd = mix$sigma[comp.number])
}

# we could select needed number of colors randomly:
#DISTRIB_COLORS <- sample(colors(), numComponents)

# or, better, use a palette with more color differentiation:
DISTRIB_COLORS <- brewer.pal(numComponents, "Set1")

distComps <- lapply(seq(numComponents), function(i)
  stat_function(fun = calc.components,
                arg = list(mix = mix.info, comp.number = i),
                geom = "line",
                size = 1,
                color = DISTRIB_COLORS[i])) # "red"

g <- ggplot(data.frame(x = myData)) +
  scale_fill_continuous("Count", low="#56B1F7", high="#132B43") + 
  scale_x_log10("Diamond Price [log10]", labels = prettyNum) +
  scale_y_continuous("Count") +
  geom_histogram(aes(x = myData, fill = 0.01 * ..density..), # ..count..
                 binwidth = 0.01) +
  distComps

print(g + distComps)
