# Start session with a clean R environment
rm(list = ls(all.names = TRUE))

# This module will handle missing values in data by using
# multiple imputation, implemented in Amelia II software.

# Corresponding R package 'Amelia' and documentation are
# available at http://cran.r-project.org/web/packages/Amelia.

if (!suppressMessages(require(methods))) install.packages('methods')
if (!suppressMessages(require(Amelia))) install.packages('Amelia')

# 'methods' is needed for 'Amelia' to alleviate the following error:
# "Error in match.fun(FUN) : object 'is' not found"
library(methods)
library(Amelia)

# load data
source("~/diss-floss/prepare/merge.R")

# use only (numeric) columns of our interest;
# this is a recommended (preferred) alternative
# to declaring unused variables as ID variables
flossData <- flossData[c("Repo URL",
                         "Project License",
                         "License Restrictiveness",
                         "User Community Size")]

# temp fix for limited dataset - comment out/remove for full dataset
flossData[["Repo URL"]] <- NULL

# perform multiple imputation with 'Amelia'
a.out <- amelia(flossData)
str(a.out)

# TODO: analyze results?
