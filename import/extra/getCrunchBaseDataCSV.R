#' Module: getCrunchBaseData.R
#'
#' Downloads FLOSS startup data by using CrunchBase's RESTful API,
#' parses and normalizes JSON response, converts data to data frame format.
#'
#' @author Aleksandr Blekh \email{blekh@@nova.edu}
#' 
#' TODO: Introduce defines for various types & sheets of CB data 
#' TODO: Update documentation; replace AL references to CB
#' TODO (low priority):
#'       Consider using CrunchBase APIs to get data instead of
#'       reading manually exported and uploaded CSV files 

if (!require(RCurl)) install.packages('RCurl')
if (!require(RODBC)) install.packages('RODBC')
if (!require(gdata)) install.packages('gdata')

library(RCurl)

#data.access <- [CSV, ODBC, GDATA]

CSV <- 0
ODBC <- 1
GDATA <- 2

method <- CSV

# CrunchBase FULL data set (monthly export - January 2014)
CRUNCHBASE_FULL_DATA <- "/home/ruser/diss-floss/data/crunchbase_monthly_export.xlsx"

# CrunchBase FULL data set APIs endpoint URL for FLOSS startups
CRUNCHBASE_FLOSS_DATA <- "http://api.angel.co/1/tags/59/startups"


#' getCrunchBaseData
#'
#' Downloads FLOSS startup data by using CrunchBase's RESTful API,
#' parses and normalizes JSON response, converts data to data frame format.
#'
#' @param tags List of startup tags per AngelList APIs
#' @return Data frame with AngelList's FLOSS startups info
#' @export
#'
#' @author Aleksandr Blekh \email{blekh@@nova.edu}
#'
#' @examples
#' getCrunchBaseData()

getCrunchBaseData <- function () {
  file.name <- CRUNCHBASE_FULL_DATA
  sheet.name <- "Companies"
  
  if (method == CSV) {
    cbData.Companies <- read.csv("~/diss-floss/data/CrunchBase_FLOSS_Companies_20131030.csv")
  } else if (method == ODBC) {
    library(RODBC)
    excel.connect <- odbcConnectExcel(file.name)
    dat <- sqlFetch(excel.connect, sheet.name, na.strings=c("","-"))
    odbcClose(excel.connect)  
  } else if (method == GDATA) {
    library(gdata)
    #a <- read.xls(path.expand(CRUNCHBASE_FULL_DATA), sheet=2)
    a <- read.xls(CRUNCHBASE_FULL_DATA, sheet=2)
  }
  
}

getCrunchBaseData()