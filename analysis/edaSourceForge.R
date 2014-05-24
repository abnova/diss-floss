if (!suppressMessages(require(ggplot2))) install.packages('ggplot2')

source("../utils/graphics.R")


g <- qplot(data[["Project Age"]], data = data,
           geom = "histogram", binwidth = 1)

# g + geom_histogram(aes(y = ..count..))
# g + geom_histogram(aes(fill = ..count..))

g + geom_histogram(aes(fill = ..count..)) +
  scale_fill_gradient("Count", low = "green", high = "red")