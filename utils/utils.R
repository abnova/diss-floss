if (!suppressMessages(require(stringr))) install.packages('stringr')
#library(stringr)

replace_all <- function(df, pattern, replacement) {
  char <- vapply(df, function(x) is.factor(x) || is.character(x),
                 logical(1))
  df[char] <- lapply(df[char], str_replace_all, pattern, replacement)
  return (df)
}

#replace_all(iris, "setosa", "barbosa")
#replace_all(iris, fixed("setosa"), "barbosa")