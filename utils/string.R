# returns string w/o leading whitespace
trimLeading <- function (x)  sub("^\\s+", "", x)

# returns string w/o trailing whitespace
trimTrailing <- function (x) sub("\\s+$", "", x)

# returns string w/o leading or trailing whitespace
trimLT <- function (x) gsub("^\\s+|\\s+$", "", x)


if (!suppressMessages(require(stringr))) install.packages('stringr')

library(stringr)

replace_all <- function(df, pattern, replacement) {
  char <- vapply(df, function(x) is.factor(x) || is.character(x),
                 logical(1))
  df[char] <- lapply(df[char], str_replace_all, pattern, replacement)
  return (df)
}

#replace_all(iris, "setosa", "barbosa")
#replace_all(iris, fixed("setosa"), "barbosa")

replace_all_df <- function(df, pattern, replacement) {
  char <- vapply(df, function(x) is.factor(x) || is.character(x),
                 logical(1))
  df[char] <-
    lapply(df[char], function(x) 
      do.call("gsub", list(pattern, replacement, x, fixed = TRUE)))
  return (df)
}

#encoded  <- c('!@#', '@@', 'http//', 'mailto@')
#original <- c(': ',  '::', 'http://', 'mailto:')

#for (i in length(encoded))
#data <- replace_all_df(data, encoded[i], original[i])