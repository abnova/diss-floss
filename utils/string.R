# returns string w/o leading whitespace
trimLeading <- function (x)  sub("^\\s+", "", x)

# returns string w/o trailing whitespace
trimTrailing <- function (x) sub("\\s+$", "", x)

# returns string w/o leading or trailing whitespace
trimLT <- function (x) gsub("^\\s+|\\s+$", "", x)