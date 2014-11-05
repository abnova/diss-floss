
# golden ratio: mostly to use its inverse for plots' aspect ratio
PHI <- 1.618


#
# Redefine ggplot2::qplot() to print the graphics object
# in order to actually produce output in non-interactive mode
#
# For details, see:
#
# 1. http://stackoverflow.com/questions/6675066/ggplots-qplot-does-not-execute-on-sourcing
# 2. http://cran.r-project.org/doc/FAQ/R-FAQ.html#Why-do-lattice_002ftrellis-graphics-not-work_003f)
#

myQplot <- function (x, y = NULL, z = NULL, ...) {
  p <- ggplot2::qplot (x = x, y = y, z = z, ...)
  print (p)
}