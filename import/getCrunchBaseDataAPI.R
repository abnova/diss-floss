#' Module: getCrunchBaseDataAPI.R
#'
#' Downloads FLOSS startup data by using CrunchBase's RESTful API,
#' parses and normalizes JSON response, converts data to data frame format.
#'
#' @author Aleksandr Blekh \email{blekh@@nova.edu}
#' 
#' TODO: Introduce defines for various types & sheets of CB data 
#' TODO: Update documentation; replace AL references to CB

if (!require(RCurl)) install.packages('RCurl')
if (!require(RODBC)) install.packages('RODBC')
if (!require(gdata)) install.packages('gdata')

library(RCurl)

# Retrieve CrunchBase API key, stored in ENV for security
CB_API_KEY <- Sys.getenv("CRUNCHBASE_API_KEY")

# CrunchBase FULL data set APIs endpoint URL for FLOSS startups
CB_FLOSS_DATA <- "http://api.angel.co/1/tags/59/startups"

CB_API_SEARCH_URL <- "http://api.crunchbase.com/v/1/search.js"


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


getCBDataPaginated <- function (query, field, page) {
  
  # Construct CB API request (this search is for companies only!)
  url <- paste(CB_API_SEARCH_URL, "?query=", query, collapse="", sep="")
  url <- paste(url, "&entity=company", "", collapse="", sep="")
  url <- paste(url, "&field=", field, collapse="", sep="")
  url <- paste(url, "&api_key=", CB_API_KEY, collapse="", sep="")
  url <- paste(url, "?page=", page, collapse="", sep="")
  
  # Retrieve data
  startupData <- getURL(url)
  
  # Convert JSON data to list?
  fromJSON(startupData)
}


#' getCBDataAPI
#'
#' Downloads FLOSS startup data by using CrunchBase's RESTful API,
#' parses and normalizes JSON response, converts data to data frame format.
#'
#' @param tags List of startup tags per CrunchBase APIs
#' @return Data frame with CrunchBase's FLOSS startups info
#' @export
#'
#' @author Aleksandr Blekh \email{blekh@@nova.edu}
#'
#' @examples
#' getCBDataAPI()
#' 
#' TODO: refactor to more generic function:
#'         getCrunchBaseData(1, 59)
#'         getCrunchBaseData('Market', 'FLOSS')

getCBDataAPI <- function (queryArg, fieldArg) {
  
  # TODO: Dyn. construct URL here: url <- paste(baseURL, ...)
  
  startups <- unlist(lapply(pageArg = 1:4,
                            getCBDataPaginated,
                            query = queryArg, field = fieldArg, page = pageArg),
                     recursive=F)
  #startupsDF <- data.frame(startups)
}

getCBDataAPI("open source", "overview")