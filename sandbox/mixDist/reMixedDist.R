library(ggplot2)
library(mixtools)

DISTRIB_COLORS <- c("green", "red")
NUM_COMPONENTS <- 2

set.seed(12345)

mix.info <- normalmixEM(faithful$waiting, k = NUM_COMPONENTS,
                        maxit = 100, epsilon = 0.01)
summary(mix.info)

plot.components <- function(mix, comp.number) {
  localEnv <- environment()
  g <- stat_function(fun = function(mix, comp.number) 
  {mix$lambda[comp.number] *
     dnorm(x, mean = mix$mu[comp.number],
           sd = mix$sigma[comp.number])},
  args = list(mix = mix, comp.number = comp.number),
  geom = "line", local(aes(colour = DISTRIB_COLORS[comp.number],
                           envir = localEnv)))
  return (g)
}

g <- ggplot(faithful, aes(x = waiting)) +
  geom_histogram(binwidth = 0.5)

distComps <- lapply(seq(NUM_COMPONENTS),
                    function(i) plot.components(mix.info, i))
print(g + distComps)
