# return value of an attribute of an object stored in a file

getAttr <- function (attr, obj, file) 
{
  x <- load(file, envir = environment())
  attrs <- attributes(obj)
  return (list(file=x, y=y))
}


# convert normal density to counts (for histogram overlaying plots);
# supports log transformation, but is result the same as using dlnorm()???
dnorm.count <- function(x, mean = 0, sd = 1,
                        log = FALSE, n = 1, binwidth = 1) {
  
  n * binwidth * dnorm(x = x, mean = mean, sd = sd, log = log) 
}
