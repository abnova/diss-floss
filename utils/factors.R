# returns factor with top N levels (by count) of the original factor

topFactors <- function(fac, N=10, o="Beyond Top") {
  
  if (missing(o)) o <- paste(o, N)
  levels(fac)[rank(-xtabs(~ fac))[levels(fac)] > N] <- o
  return (fac)
}