# This module will handle missing values in data by using
# multiple imputation, implemented in Amelia II software.

# Corresponding R package 'Amelia' and documentation are
# available at http://cran.r-project.org/web/packages/Amelia.

if (!suppressMessages(require(Amelia))) install.packages('Amelia')
library(Amelia)

# convert factors to numeric (TODO: move to transform?)

flossData[["Project License"]] <- 
  as.integer(flossData[["Project License"]])

flossData[["License Restrictiveness"]] <- 
  as.integer(flossData[["License Restrictiveness"]])

# perform multiple imputations with 'Amelia'

a.out <- amelia(flossData[, !names(flossData) %in% c("Repo URL")])
str(a.out)

# TODO: analyze results?
