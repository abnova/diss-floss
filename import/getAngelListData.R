#' Module: getAngelListData.R
#'
#' Downloads FLOSS startup data by using AngelList's RESTful API,
#' parses and normalizes JSON response, converts data to data frame format.
#'
#' @author Aleksandr Blekh \email{blekh@@nova.edu}

if (!require(RCurl)) install.packages('RCurl')
if (!require(jsonlite))
  install.packages("jsonlite", repos="http://cran.r-project.org")
if (!require(plyr)) install.packages('plyr')

library(RCurl)
library(jsonlite)
library(plyr)

# AngelList APIs endpoint URL for FLOSS startups
# ('Market' tag = '1', 'FLOSS' tag = '59')
API_ENDPOINT_URL <- "http://api.angel.co/1/tags/59/startups"

DEBUG <- TRUE


#' getDataPaginated
#'
#' Downloads RESTful API's response in JSON paginated format.
#'
#' @param page Number of page for JSON response
#' @return TODO
#' @export
#'
#' @author Aleksandr Blekh \email{blekh@@nova.edu}
#'
#' @examples
#' getDataPaginated(1)

getDataPaginated <- function (page) {
  
  # construct full URL for paginated API request
  url <- paste(API_ENDPOINT_URL, "?page=", page, collapse="", sep="")
  
  # retrieve API reply (JSON data)
  startupData <- getURL(url)
  
  #options(stringsAsFactors = FALSE)
  
  # parse JSON reply (field 'startups' will contain data frame)
  data <- jsonlite::fromJSON(startupData)
  
  if (DEBUG && page == 1) View(data$startups)
  
  # collect only NOT hidden rows from the source data frame
  startups <- rbind.fill(data$startups[data$startups$hidden == FALSE, ])
  
  if (DEBUG && page == 1) View(startups)
  
  return (startups)
}


#' getAngelListData
#'
#' Downloads FLOSS startup data by using AngelList's RESTful API,
#' parses and normalizes JSON response, converts data to data frame format.
#'
#' @param tags List of startup tags per AngelList APIs
#' @return Data frame with AngelList's FLOSS startups info
#' @export
#'
#' @author Aleksandr Blekh \email{blekh@@nova.edu}
#'
#' @examples
#' getAngelListData()
#' 
#' TODO: refactor to more generic function:
#'         getAngelListData(1, 59)
#'         getAngelListData('Market', 'FLOSS')

getAngelListData <- function () {
  
  # TODO: Dyn. construct URL here: url <- paste(baseURL, ...) 
  startups <- lapply(1:4, getDataPaginated)
  
  startups <- data.frame(startups)
  #startups <- unlist(startups)
  #startups <- data.frame(unlist(startups))
  
  if (DEBUG) {
    print(nrow(startups))
    print(class(startups))
    #print(head(startups))
  }
  return (startups)
}


message("\nRetrieving AngelList data...\n")

getAngelListData()