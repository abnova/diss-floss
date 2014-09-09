library("GGally")
data(iris)

ggpairs(iris, colour="Species", axisLabels="none")