DEBUG_INFO <- function (msg) {
  print(msg)
}

DEBUG_VAR <- function (var) {
  print(sprintf("%s = %s", deparse(substitute(var)), var))
}

View <- function (...) {
  if (.Platform$GUI == "RStudio")
    View(...)
  else
    print(...)
}