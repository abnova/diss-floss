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

comp.1 <- list(myData, mix.info, 1)
comp.2 <- list(myData, mix.info, 2)

calc.components <- function(x, mix, comp.number) {
  
  mean(x) * mix$lambda[comp.number] *
    dnorm(x, mean = mix$mu[comp.number], sd = mix$sigma[comp.number])
}

DISTRIB_COLORS <- brewer.pal(numComponents, "Set1")

g <- ggplot(myData) +
  scale_fill_continuous("Density", low="#56B1F7", high="#132B43") +
  scale_x_log10("Diamond Price [log10]") +
  scale_y_continuous("Density") +
  geom_histogram(aes(x = myData, y = ..density.., fill = ..density..),
                 binwidth = 0.01) +
  stat_function(fun = "calc.components", args = comp.1,
                aes(color = DISTRIB_COLORS[1])) +
  stat_function(fun = "calc.components", args = comp.2,
                aes(color = DISTRIB_COLORS[2]))
print(g)
