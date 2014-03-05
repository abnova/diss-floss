#' Module: getSourceForgeData.R
#'
#' Downloads FLOSS projects data from SourceForge.net repository
#' via SourceForge Research Data Archive (SRDA), by using RCurl
#' to access SRDA Query Form for sending queries and receiving replies.
#'
#' @author Aleksandr Blekh \email{blekh@@nova.edu}

if (!require(RCurl)) install.packages('RCurl')
if (!require(digest)) install.packages('digest')

library(RCurl)
library(digest)

# Users must authenticate to access Query Form
SRDA_HOST_URL  <- "http://zerlot.cse.nd.edu"
SRDA_LOGIN_URL <- "/mediawiki/index.php?title=Special:Userlogin"
SRDA_LOGIN_REQ <- "&action=submitlogin&type=login"

# SRDA URL that Query Form sends POST requests to
SRDA_QUERY_URL <- "/cgi-bin/form.pl"

# SRDA URL that Query Form sends POST requests to
SRDA_QRESULT_URL <- "/qresult/blekh/blekh.txt"

# Parameters for result's format
DATA_SEP <- ":" # data separator
ADD_SQL  <- "0" # add SQL to file

REPLACE_CLAUSE <- "REPLACE(REPLACE(REPLACE(a.details, ':', ';'), CHR(10),' '), CHR(13),' ')"

RDATA_EXT <- ".Rdata"
RDATA_DIR <- "../cache/SourceForge" #TODO: consider passing this via CL args

DEBUG <- TRUE # TODO: retrieve debug flag via CL arguments


#' srdaLogin()
#'
#' Logs into SRDA by submitting login form with username & password.
#'
#' @param page Number of page for JSON response
#' @return TODO
#' @export
#'
#' @author Aleksandr Blekh \email{blekh@@nova.edu}
#'
#' @examples
#' getDataPaginated(1)

srdaLogin <- function (loginURL, username, password) {
  
  curl = getCurlHandle()
  curlSetOpt(cookiejar = 'cookies.txt', curl = curl)
  
  params <- list('wpName1' = username, 'wpPassword1' = password)
  
  if(url.exists(loginURL))
    postForm(loginURL, .params = params, curl = curl, style = "POST")
}


#' srdaConvertRequest()
#'
#' Logs into SRDA by submitting login form with username & password.
#'
#' @param page Number of page for JSON response
#' @return TODO
#' @export
#'
#' @author Aleksandr Blekh \email{blekh@@nova.edu}
#'
#' @examples
#' srdaConvertRequest(1)

srdaConvertRequest <- function (request) {
  
  #TODO
  #lookup table
  #tokenize and filter request values
  
  return (list(select = "*", from = "sf0305.users", where = "user_id < 100"))
}


#' srdaRequestData()
#'
#' Logs into SRDA by submitting login form with username & password.
#'
#' @param page Number of page for JSON response
#' @return TODO
#' @export
#'
#' @author Aleksandr Blekh \email{blekh@@nova.edu}
#'
#' @examples
#' srdaRequestData(1)

srdaRequestData <- function (requestURL, select, from, where, sep, sql) {
  
  curl = getCurlHandle()
  #curlSetOpt(cookiejar = 'cookies.txt', curl = curl)
  
  params <- list('uitems' = select,
                 'utables' = from,
                 'uwhere' = where,
                 'useparator' = sep,
                 'append_query' = sql)
  
  if(url.exists(requestURL))
    postForm(requestURL, .params = params, curl = curl, style = "POST")
}


#' srdaGetData()
#'
#' Logs into SRDA by submitting login form with username & password.
#'
#' @param page Number of page for JSON response
#' @return TODO
#' @export
#'
#' @author Aleksandr Blekh \email{blekh@@nova.edu}
#'
#' @examples
#' srdaGetData(1)

srdaGetData <- function() { #srdaGetResult() might be a better name
  
  curl = getCurlHandle()
  #curlSetOpt(cookiejar = 'cookies.txt', curl = curl)
  
  resultsURL <- paste(SRDA_HOST_URL, SRDA_QRESULT_URL,
                      collapse="", sep="")
  
  results <- readLines(resultsURL)
  results <- lapply(results, function(x) gsub(".$", "", x))
  
  data <- read.table(textConnection(unlist(results)), header = FALSE,
                     sep = DATA_SEP, quote = "\"",
                     colClasses = "character", row.names = NULL)
  
  return (data)
}


#' getSourceForgeData
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
#' getSourceForgeData()
#' 
#' TODO: refactor to more generic function:
#'         getAngelListData(1, 59)
#'         getAngelListData('Market', 'FLOSS')

getSourceForgeData <- function (request) {

  # calculate request's digest and generate corresponding RData file name
  fileDigest <- digest(request, algo="md5", serialize=F)
  rdataFile <- paste(RDATA_DIR, "/", fileDigest, RDATA_EXT, sep = "")
  
  # check if the archive file has already been processed
  if (DEBUG) {message("Checking request \"", request, "\"...")}
  if (file.exists(rdataFile)) {
    if (DEBUG) {message("Processing skipped: .Rdata file found.\n")}
    return()
  }
  
  # Construct SRDA login and query URLs
  loginURL <- paste(SRDA_HOST_URL, SRDA_LOGIN_URL, SRDA_LOGIN_REQ,
                    collapse="", sep="")
  queryURL <- paste(SRDA_HOST_URL, SRDA_QUERY_URL, collapse="", sep="")
  
  # Log into the system 
  try(srdaLogin(loginURL, SRDA_USER, SRDA_PASS))
  #try(srdaLogin(loginURL, getOption("SRDA_USER"), getOption("SRDA_PASS")))
  
  rq <- srdaConvertRequest(request)
  
  REPLACE_CLAUSE <- "" #temp
  rq$select <- paste(rq$select, REPLACE_CLAUSE, collapse="", sep=" ")
  
  srdaRequestData(queryURL,
                  rq$select, rq$from, rq$where, DATA_SEP, ADD_SQL)
  
  data <- srdaGetData()
  if (DEBUG) print(data)
  
  # save current data frame to RData file
  save(data, file = rdataFile)
  
  # clean up
  rm(data)
}


message("\nRetrieving SourceForge data...\n")

getSourceForgeData("SELECT * FROM sf0305.users WHERE user_id < 100 ")