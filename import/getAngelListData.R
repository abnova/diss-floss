#' Module: getAngelListData.R
#'
#' Downloads FLOSS startup data by using AngelList's RESTful API,
#' parses and normalizes JSON response, converts data to data frame format.
#'
#' @author Aleksandr Blekh \email{blekh@@nova.edu}

if (!suppressMessages(require(RCurl))) install.packages('RCurl')
if (!suppressMessages(require(jsonlite)))
  install.packages("jsonlite", repos="http://cran.r-project.org")

library(RCurl)
library(jsonlite)

# Use Jeroen Ooms' "simplify" approach by default:
# jsonlite::fromJSON(..., simplifyVector=FALSE),
# then do.call(c, ...) & jsonlite:::simplify()
# instead of jsonlite::fromJSON() & TWO plyr::rbind.fill() calls
JO <- TRUE

if (JO) {
  if (!suppressMessages(require(plyr))) install.packages('plyr')
  library(plyr)
}


PRJ_HOME <- Sys.getenv("DISS_FLOSS_HOME")

source(file.path(PRJ_HOME, "config/diss-floss-config.R"))
source(file.path(PRJ_HOME, "utils/debug.R"))

# AngelList APIs endpoint URL for FLOSS startups
# ('Market' tag = '1', 'FLOSS' tag = '59')
API_ENDPOINT_URL <- "http://api.angel.co/1/tags/59/startups"

RDATA_DIR <- file.path(PRJ_HOME, "cache/AngelList")


getNumPages <- function () {
  
  startupData <- getURL(API_ENDPOINT_URL)
  
  data <- jsonlite::fromJSON(startupData)

  if (DEBUG) message("\nAPI reply contains: ",
                     data$total, " startups, ",
                     data$last_page, " pages.\n")
  
  return (data$last_page)
}


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
  url <- paste0(API_ENDPOINT_URL, "?page=", page)
  
  # retrieve API reply (JSON data)
  startupData <- getURL(url)
  
  # parse JSON reply (field 'startups' will contain data frame)
  if (JO)
    data <- jsonlite::fromJSON(startupData, simplifyVector = FALSE)
  else
    data <- jsonlite::fromJSON(startupData)
  
  #if (DEBUG && page == 1) str(data$startups, vec.len=12)
  #if (DEBUG) message("Page ", page, " - # of attributes: ",
  #                   length(data$startups))
  
  # collect only NOT hidden rows from the source list (JO) / data frame
  if (JO) {
    # create logical index vector 'hidden'
    hidden <- sapply(data$startups, "[[", "hidden")
    # subset list, based on the index vector
    startups <- data$startups[!hidden]
  }
  else {
    startups <- rbind.fill(data$startups[data$startups$hidden == FALSE, ])
    
    # change the type of column from data frame to list,
    # since NEXT rbind.fill() cannot handle data frame column
    #TODO (the following line currently fails)
    #startups$status <- as.list(startups$status)
  }
  
  #if (DEBUG && page == 1) str(startups, vec.len=12)
  
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
  
  pages <- 1:getNumPages()
  
  # TODO: Dyn. construct URL here: url <- paste(baseURL, ...) 
  startups <- lapply(pages, getDataPaginated)
  
  #if (DEBUG) str(startups)
  
  if (JO) {
    startups <- do.call(c, startups)
    startups <- jsonlite:::simplify(startups)
  }
  else {
    # the following rbind.fill() call produces this error:
    # "Data frame column 'status' not supported by rbind.fill"
    # Tentative solution: convert DF column to list (see TODO above)
    startups <- rbind.fill(startups)
  }
  
  # ldply() might replace combination of lapply() and rbind.fill()
  ##startups <- ldply(pages, getDataPaginated)
  
  if (DEBUG) {
    #print(nrow(startups))
    #print(class(startups))
    #str(startups)
  }
  return (startups)
}


message("\n=== AngelList data collection ===\n")

# Create cache directory, if it doesn't exist
if (!file.exists(RDATA_DIR)) {
  dir.create(RDATA_DIR, recursive = TRUE, showWarnings = FALSE)
}

message("Retrieving AngelList data...\n")

alFlossStartups <- getAngelListData()
#if (DEBUG) str(allData, vec.len=12, list.len=5)
#if (DEBUG) View(allData)

# generate corresponding RDS file name from result data frame name
fileName <- paste0(deparse(substitute(alFlossStartups)), RDS_EXT)
rdataFile <- file.path(RDATA_DIR, fileName)

# save FLOSS startups data frame to RDS file
saveRDS(alFlossStartups, rdataFile)

message("\nAngelList data collection completed successfully.\n")
