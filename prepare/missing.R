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

DEBUG <- FALSE

message("\n===== HANDLING MISSING VALUES: MI and FIML =====")

# ===== PREPARATION =====

# load data
message("\nLoading data...\n")
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

# ===== ANALYSIS =====

# First, determine the missingness patterns
# (amount of missingness across observations and variables)
message("\nAnalyzing missingness patterns...\n")
print(mice::md.pattern(flossData))

# add trailing '\n' when code below is enabled
message("\nTesting data for being MCAR... Currently disabled.")

# currently disabled due to producing the following error:
# "Error: cannot allocate vector of size 4.3 Gb"
#MissMech::TestMCARNormality(flossData)

# instead, let's use function from 'BaylorEdPsych' package
# currently also disabled due to producing the following error:
# "Error in solve.default(cov) : 'a' is 0-diml"
#mcar.little <- BaylorEdPsych::LittleMCAR(flossData)

# try removing all incomplete cases to prevent error below
#mcar.little <- 
#  BaylorEdPsych::LittleMCAR(flossData[complete.cases(flossData),])

#print(mcar.little[c("chi.square", "df", "p.value")])
#message("\n")


# ===== HANDLE MISSING VALUES =====

message("\nPerforming Multiple Imputation (MI)...", appendLF = FALSE)

# perform multiple imputation with 'Amelia'
a.out <- amelia(flossData, p2s = 0)

#if (DEBUG) str(a.out) #TODO: why fails?

# display results of the MI and data summary
message("Completed.\n")
message("MI Results:")
message("===========")
summary(a.out)

# output data summary before and after MI (latest MI iteration)
message("Data before MI:")
message("===============\n")
print(summary(flossData))

message("\nData after MI:")
message("================\n")
print(summary(a.out$imputations$imp5))

# suppress "NAs introduced by coercion" warnings
suppressWarnings(describe(flossData))

# TODO: analyze results?
