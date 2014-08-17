# Start session with a clean R environment
rm(list = ls(all.names = TRUE))

if (!suppressMessages(require(devtools))) install.packages('devtools')
library(devtools)

if (!suppressMessages(require(devtools))) 
  devtools::install_github("ggthemr", "cttobin")
library(ggthemr)

data(cars)

ggthemr("light", type = "inner")

mpgPlot <- 
  ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) + geom_point()
print(mpgPlot)

myTheme <- ggthemr("light", type = "inner", static = FALSE)

mpgPlot + theme(panel.margin=unit(5 , "lines"))
print(mpgPlot)
