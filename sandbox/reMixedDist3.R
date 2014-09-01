library(ggplot2)
library(RColorBrewer)
library(mixtools)

NUM_COMPONENTS <- 2

set.seed(12345) # for reproducibility

data <- faithful$waiting # use R built-in data

# extract 'k' components from mixed distribution 'data'
mix.info <- normalmixEM(data, k = NUM_COMPONENTS,
                        maxit = 100, epsilon = 0.01)
summary(mix.info)

numComponents <- length(mix.info$sigma)
message("Extracted number of component distributions: ",
        numComponents)

calc.components <- function(x, mix, comp.number) {
  
  mix$lambda[comp.number] *
    dnorm(x, mean = mix$mu[comp.number], sd = mix$sigma[comp.number])
}

g <- ggplot(data.frame(x = data)) +
  scale_x_log10("Waiting time (mins)") +
  scale_y_log10("Count of particular waiting times",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = prettyNum) +
  geom_histogram(aes(x = data, fill = ..count..),
                 binwidth = 0.5)
print(g)

# we could select needed number of colors randomly:
#DISTRIB_COLORS <- sample(colors(), numComponents)

# or, better, use a palette with more color differentiation:
DISTRIB_COLORS <- brewer.pal(numComponents, "Set1")

distComps <- lapply(seq(numComponents), function(i)
  stat_function(fun = calc.components,
                arg = list(mix = mix.info, comp.number = i),
                geom = "line", # use alpha=.5 for "polygon"
                size = 2,
                color = DISTRIB_COLORS[i]))
print(g + distComps)
