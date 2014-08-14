# This module will handle missing values in data by using
# multiple imputation, implemented in Amelia II software.

# Corresponding R package 'Amelia' and documentation are
# available at http://cran.r-project.org/web/packages/Amelia.

if (!suppressMessages(require(Amelia))) install.packages('Amelia')
library(Amelia)

# perform multiple imputation with 'Amelia'

a.out <- amelia(flossData)
str(a.out)

# TODO: analyze results?
