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
  
  mean(x) * mix$lambda[comp.number] *
    dnorm(x, mean = mix$mu[comp.number], sd = mix$sigma[comp.number])
}

# we could select needed number of colors randomly:
#DISTRIB_COLORS <- sample(colors(), numComponents)

# or, better, use a palette with more color differentiation:
DISTRIB_COLORS <- brewer.pal(numComponents, "Set1")

#distComps <- lapply(seq(numComponents), function(i)
#  calc.components(myData, mix.info, i))

# create a data frame with columns consisting of
# calculated values for each component distribution
my.df <- data.frame(comp1 = calc.components(myData, mix.info, 1),
                    comp2 = calc.components(myData, mix.info, 2),
                    data = myData)

g <- ggplot(my.df) +
  scale_fill_continuous("Density", low="#56B1F7", high="#132B43") +
  #scale_x_log10("Diamond Price [log10]",
  #              breaks = trans_breaks("log10", function(x) 10^x),
  #              labels = prettyNum) +
  scale_x_log10("Diamond Price [log10]") +
  scale_y_continuous("Density") +
  geom_histogram(aes(x = data, y = ..density..,
                     fill = ..density..),
                 binwidth = 0.01) +
  geom_line(aes(x = data, y = comp1, color = DISTRIB_COLORS[1])) +
  geom_line(aes(x = data, y = comp2, color = DISTRIB_COLORS[2]))

print(g)

#print(g + distComps)
