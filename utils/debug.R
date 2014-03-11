DEBUG_INFO <- function (msg) {
  print(msg)
}

DEBUG_VAR <- function (var) {
  print(sprintf("%s = %s", deparse(substitute(var)), var))
}

View2 <- function (...) {
  if (.Platform$GUI == "RStudio")
    View(...)
  else
    print(...)
}

View <- if (identical(utils::View, View)) print else View