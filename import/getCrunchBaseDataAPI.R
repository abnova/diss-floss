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
if (!require(RJSONIO)) install.packages('RJSONIO')

library(RCurl)
library(RJSONIO)

# Retrieve CrunchBase API key, stored in ENV for security
#CB_API_KEY <- Sys.getenv("CRUNCHBASE_API_KEY")
CB_API_KEY <- "uuxr6qxxm3be8zwbpt5kuvs2"

# Per CB APIs documentation
CB_REPLY_OBJS_PER_PAGE <- 10

# CrunchBase FULL data set APIs endpoint URL for FLOSS startups
CB_FLOSS_DATA <- "http://api.angel.co/1/tags/59/startups"

CB_API_SEARCH_URL <- "http://api.crunchbase.com/v/1/search.js"

# GLOBAL Indicator of JSON response's first page
firstPage <<- TRUE

# GLOBAL Counter for total number of pages in JSON reply
# Another solution would be to create a custom environment
# (for details see: http://rpubs.com/chrisbrunsdon/local)
totalPages <<- 0

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
  url <- paste(url, "&page=", page, collapse="", sep="")
  
  # Retrieve data
  startupData <- getURL(url)
  
  # Convert JSON data to list
  startups <- fromJSON(startupData)
  
  # Calculate number of pages in the response; do it only once
  if (firstPage == TRUE) {
    totalPages <<- startups$total %/% CB_REPLY_OBJS_PER_PAGE
    if (startups$total %% CB_REPLY_OBJS_PER_PAGE > 0)
      totalPages <<- totalPages + 1
    firstPage <<- FALSE
    DEBUG_INFO("Retrieving CB data... Pages:")
  }
  DEBUG_INFO(c(page, '.'))
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

getCBDataAPI <- function (query, field) {
  
  # Initial call is separate from the subsequent calls
  # in order to retrieve data for calculation of totalPages
  try(getCBDataPaginated(query, field, page <- 1))
  
  #pages <- 2:totalPages
  # Continue with the rest of reply's pages
  reply <- lapply(2:totalPages,
                  function(page) try(getCBDataPaginated(query, field, page), silent=FALSE))
  
  startups <- unlist(reply, recursive=F)
  #startupsDF <- data.frame(startups)
}

getCBDataAPI("open+source", "overview")