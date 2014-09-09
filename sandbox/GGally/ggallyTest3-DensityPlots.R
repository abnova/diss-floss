library("GGally")
data(iris)

ggpairs(iris[,3:5], 
        upper = list(continuous = "density", combo = "box"), 
        lower = list(continuous = "points", combo = "dot"), 
        diag = list(continuous = "bar", discrete = "bar"))


# example 3 – some new stuff!!!
dat <- data.frame(x = rnorm(100),
                  y = rnorm(100),
                  z = rnorm(100))

plotmatrix <- 
  ggpairs(dat,
          lower = list(continuous = "density", 
                       aes_string = aes_string(fill = "..level..")),
          upper = "blank") 
plotmatrix
