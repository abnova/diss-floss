#' Module: getCrunchBaseDataAPI.R
#'
#' Downloads FLOSS startup data by using CrunchBase's RESTful API,
#' parses and normalizes JSON response, converts data to data frame format.
#'
#' @author Aleksandr Blekh \email{blekh@@nova.edu}
#' 
#' TODO: Introduce defines for various types & sheets of CB data 
#' TODO: Update documentation; replace AL references to CB
#' TODO(general): "wrap" packages operations in a function loadPackages(),
#'     then call suppressMessages(loadPackages()) if 'verbose' is disabled

if (!require(RCurl)) install.packages('RCurl')
if (!require(jsonlite))
  install.packages("jsonlite", repos="http://cran.r-project.org")
if (!require(plyr)) install.packages('plyr')

library(RCurl)
library(jsonlite)
library(plyr)

source("../utils/debug.R")

# Limit per CB APIs v.1 documentation
CB_REPLY_OBJS_PER_PAGE <- 10

# CrunchBase FULL data set APIs endpoint URL for FLOSS startups
# TODO: update or delete
CB_FLOSS_DATA <- "http://api.angel.co/1/tags/59/startups"

CB_API_SEARCH_URL <- "http://api.crunchbase.com/v/1/search.js"

# GLOBAL Indicator of JSON response's first page
firstPage <<- TRUE

# GLOBAL Counter for total number of pages in JSON reply
# Another solution would be to create a custom environment
# (for details see: http://rpubs.com/chrisbrunsdon/local)
totalPages <<- 0

DEBUG <- TRUE # TODO: retrieve debug flag via CL arguments


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

getCBDataPaginated <- function (query, field, page, progress, useProgress) {
  
  # Construct CB API request (this search is for companies only!)
  url <- paste(CB_API_SEARCH_URL, "?query=", query, collapse="", sep="")
  url <- paste(url, "&entity=company", "", collapse="", sep="")
  url <- paste(url, "&field=", field, collapse="", sep="")
  url <- paste(url, "&api_key=", CB_API_KEY, collapse="", sep="")
  url <- paste(url, "&page=", page, collapse="", sep="")
  
  # Retrieve data
  startupData <- getURL(url)
  
  # Convert JSON data to data frame
  startups <- jsonlite::fromJSON(startupData, simplifyVector = FALSE)
  
  # Calculate number of pages in the response; do it only once
  if (firstPage == TRUE) {
    totalPages <<- startups$total %/% CB_REPLY_OBJS_PER_PAGE
    if (startups$total %% CB_REPLY_OBJS_PER_PAGE > 0)
      totalPages <<- totalPages + 1
    if (DEBUG) message("API reply contains: ",
                       startups$total, " startups, ",
                       totalPages, " pages.\n")
    firstPage <<- FALSE
  }

  # Update progress bar
  if (useProgress)
    setTxtProgressBar(progress, page)
  
  return (startups$results)
}


#' convertMultiWordQuery
#'
#' Converts multi-word query string into CrunchBase query format
#' by replacing spaces between words with '+' characters
#'
#' @param query Multi-word query string
#' @return Query string converted into string per CrunchBase APIs
#' @export
#'
#' @examples
#' convertMultiWordQuery()

convertMultiWordQuery <- function (query) {
  return (gsub(" ", "+", query))
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
  
  query <- convertMultiWordQuery(query)
  
  # Dummy call just to obtain 'progress' handle so it can be passed on
  progress <- txtProgressBar(max = 1,
                             initial = NA,
                             char = '>',
                             style = 3)
  
  # Initial call is separate from the subsequent calls
  # in order to retrieve data for calculation of totalPages
  getCBDataPaginated(query, field, page <- 1, progress, FALSE)
  
  # Real progress bar call (notice 'initial' is no longer NA)
  progress <- txtProgressBar(max = totalPages - 1,
                             initial = 0,
                             char = '>',
                             style = 3)
  
  # Continue with the rest of reply's pages
  reply <- lapply(1:totalPages,
                  function(page) 
                    try(getCBDataPaginated(query, field, page, progress, TRUE),
                    silent = TRUE))
  
  reply <- do.call(c, reply)
  startups <- jsonlite:::simplify(reply)

  # save current data frame to RData file
  #TODO: Think about feasibility of caching non-archival data,
  # such as AngelList and CrunchBase
  #save(startups, file = rdataFile)
  
  # clean up
  #rm(startups)
  
  close(progress)
  #if (DEBUG) print(head(reply))
  
  return (invisible(startups))
}


field <- "overview"
query <- "drone" # "wearable"

debugInfo <- paste(" for request ['", field, "' = \"", query, "\"]",
                   sep = "")

message("\nRetrieving CrunchBase data",
        ifelse(DEBUG, debugInfo, ""), "...\n")

#getCBDataAPI("open+source", "overview")
getCBDataAPI(query, field)

message("\n")
