if (!suppressMessages(require(parallel))) install.packages('parallel')
library(parallel)


packages <- installed.packages()

if ("ggplot2" %in% packages) {
  GGPLOT2_VER <- packages["ggplot2", c("Version")]
} else {
  stop("Package 'ggplot2' is required! Please install it and try again!")
}


# MULTIPROCESSING: Multi-core processing (based on 'parallel' package)

# increase number of cores to use to the maximum available
options("mc.cores"=detectCores())