# Start session with a clean R environment
rm(list = ls(all.names = TRUE))

# This module will handle missing values in data by using
# multiple imputation, implemented in Amelia II software.

# Corresponding R package 'Amelia' and documentation are
# available at http://cran.r-project.org/web/packages/Amelia.

if (!suppressMessages(require(mice))) install.packages('mice')
if (!suppressMessages(require(MissMech))) install.packages('MissMech')
if (!suppressMessages(require(BaylorEdPsych))) 
  install.packages('BaylorEdPsych')
if (!suppressMessages(require(mvnmle))) install.packages('mvnmle')
if (!suppressMessages(require(methods))) install.packages('methods')
if (!suppressMessages(require(Amelia))) install.packages('Amelia')

# 'mice' is needed for determining missingness patterns
# 'MissMech' is needed for testing data for being MCAR
# 'BaylorEdPsych' is needed as an alternative for MCAR testing
# 'mvnmle' is needed as it is used by 'BaylorEdPsych'
# 'methods' is needed for 'Amelia' to alleviate the following error:
# "Error in match.fun(FUN) : object 'is' not found"
library(mice)
library(MissMech)
library(BaylorEdPsych)
library(mvnmle)
library(methods)
library(Amelia)

# ===== PREPARATION =====

# load data
message("Loading data...\n")
source("~/diss-floss/prepare/merge.R")
message("\n")

# use only (numeric) columns of our interest;
# this is a recommended (preferred) alternative
# to declaring unused variables as ID variables
flossData <- flossData[c("Repo URL",
                         "Project License",
                         "License Restrictiveness",
                         "User Community Size")]

# temp fix for limited dataset - comment out/remove for full dataset
flossData[["Repo URL"]] <- NULL

# ===== ANALYSIS =====

# First, determine the missingness patterns
# (amount of missingness across observations and variables)
message("Analyzing missingness patterns...\n")
mice::md.pattern(flossData)
message("\n")

message("Testing data for being MCAR...\n")

# currently disabled due to producing the following error:
# "Error: cannot allocate vector of size 4.3 Gb"
#MissMech::TestMCARNormality(flossData)

# instead, let's use function from 'BaylorEdPsych' package
# currently also disabled due to producing the following error:
# "Error in solve.default(cov) : 'a' is 0-diml"
#mcar.little <- BaylorEdPsych::LittleMCAR(flossData)
#print(mcar.little[c("chi.square", "df", "p.value")])
message("\n")


# ===== HANDLE MISSING VALUES =====

# perform multiple imputation with 'Amelia'
a.out <- amelia(flossData)
str(a.out)

# TODO: analyze results?
