# return value of an attribute of an object stored in a file

getAttr <- function (attr, obj, file) 
{
  x <- load(file, envir = environment())
  attrs <- attributes(obj)
  return (list(file=x, y=y))
}

