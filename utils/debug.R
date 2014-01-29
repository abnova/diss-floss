DEBUG_INFO <- function(msg) {
  print(msg)
}

DEBUG_VAR <- function(var) {
  print(sprintf("%s = %s", deparse(substitute(var)), var))
}
