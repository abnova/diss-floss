library(ggplot2)
library(scales)
library(RColorBrewer)
library(reshape)
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

# create a data frame with columns consisting of
# calculated values for each component distribution
my.df <- data.frame(comp1 = calc.components(myData, mix.info, 1),
                    comp2 = calc.components(myData, mix.info, 2),
                    data = myData)

# convert to long format
my.df.long <- melt(my.df, id.vars = "data")

# we could select needed number of colors randomly:
#DISTRIB_COLORS <- sample(colors(), numComponents)

# or, better, use a palette with more color differentiation:
DISTRIB_COLORS <- brewer.pal(numComponents, "Set1")

g <- ggplot(my.df, aes(x = myData)) +
  geom_histogram(aes(x = myData, fill = ..density..), # ..count..
                 binwidth = 0.01) +
  #stat_ecdf() +
  geom_line(data = my.df.long,
            aes(x = data, y = value, color = variable)) +
  scale_x_log10("Diamond Price [log10]",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = prettyNum) +
  scale_y_continuous("Count") +
  scale_fill_continuous("Count", low="#56B1F7", high="#132B43")
  #theme(legend.position = 'top', legend.direction = 'vertical')
print(g)

#print(g + distComps)
