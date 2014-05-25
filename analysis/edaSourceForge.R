if (!suppressMessages(require(ggplot2))) install.packages('ggplot2')

source("../utils/graphics.R")


data <- readRDS("../cache/SourceForge/cHJqQWdl.rds")

g <- qplot(data[["Project Age"]], data = data, binwidth = 1) +
  #scale_size_area("Number of projects") + 
  scale_x_continuous("Project Age") +
  scale_y_continuous("Number of projects") +
  ggtitle(label="Projects distribution across their age")

g <- g + geom_histogram(aes(fill = ..count..), binwidth = 1)
print(g)

g <- g + geom_histogram(aes(y = ..density..), binwidth = 1) +
  geom_density()
print(g)


#g <- qplot(data[["Project Age"]], data = data,
#           geom = "histogram", binwidth = 1)

#g <- g + geom_histogram(aes(y = ..count..), binwidth = 1)

#g <- g + geom_histogram(aes(fill = ..count..), binwidth = 1) +
#  scale_fill_gradient("Count", low = "green", high = "red")
