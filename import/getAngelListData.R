#' Module: getAngelListData.R
#'
#' Downloads FLOSS startup data by using AngelList's RESTful API,
#' parses and normalizes JSON response, converts data to data frame format.
#'
#' @author Aleksandr Blekh \email{blekh@@nova.edu}

if (!require(RCurl)) install.packages('RCurl')
if (!require(jsonlite))
  install.packages("jsonlite", repos="http://cran.r-project.org")

library(RCurl)
library(jsonlite)

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
  url <- paste(API_ENDPOINT_URL, "?page=", page, collapse="", sep="")
  startupData <- getURL(url)
  data <- jsonlite::fromJSON(startupData)
  startups <- do.call('rbind', data)
  if (DEBUG) print(class(startups))
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
  #startups <- unlist(lapply(1:4, getDataPaginated), recursive=F)
  startups <- lapply(1:4, getDataPaginated)
  
  if (DEBUG) {
    print(class(startups))
    #print(startups) 
  }
  return (startups)
}


message("\nRetrieving AngelList data...\n")

getAngelListData()