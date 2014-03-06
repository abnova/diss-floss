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

source("../utils/debug.R")

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

curl <<- getCurlHandle()


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
  
  curlSetOpt(curl = curl, cookiejar = 'cookies.txt',
             ssl.verifyhost = FALSE, ssl.verifypeer = FALSE,
             followlocation = TRUE, verbose = TRUE)
  
  params <- list('wpName1' = username, 'wpPassword1' = password)
  
  if(url.exists(loginURL)) {
    reply <- postForm(loginURL, .params = params, curl = curl,
                      style = "POST")
    #if (DEBUG) print(reply)
    info <- getCurlInfo(curl)
    return (ifelse(info$response.code == 200, TRUE, FALSE))
  }
  else {
    error("Can't access login URL!")
  }
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
  
  #return (list(select = "*", from = "sf0305.users", where = "user_id < 100"))
  return (list(select = "*",
               from = "sf1104.users a, sf1104.artifact b",
               where = "b.artifact_id = 304727"))
  
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
  
  #curl = getCurlHandle()
  #curlSetOpt(cookiejar = 'cookies.txt', curl = curl)
  
  params <- list('uitems' = select,
                 'utables' = from,
                 'uwhere' = where,
                 'useparator' = sep,
                 'append_query' = sql)
  
  #opts <- curlOptions(verbose = TRUE, followLocation = TRUE, header = TRUE)
  
  if(url.exists(requestURL)) {
    print("Before second postForm()")
    reply <- postForm(requestURL, .params = params, #.opts = opts,
                      curl = curl, style = "POST")
    #if (DEBUG) print(reply)
  }
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
  
  resultsURL <- paste(SRDA_HOST_URL, SRDA_QRESULT_URL,
                      collapse="", sep="")
  
  results <- readLines(resultsURL)
  results <- lapply(results, function(x) gsub(".$", "", x))
  
  print(results)
  data <- read.table(textConnection(unlist(results)), header = FALSE,
                     sep = DATA_SEP, quote = "\"",
                     colClasses = "character", row.names = NULL)
  
  #if (DEBUG) print(data)

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
  if (!srdaLogin(loginURL, SRDA_USER, SRDA_PASS))
    error("Login failed!")
  #try(srdaLogin(loginURL, getOption("SRDA_USER"), getOption("SRDA_PASS")))
  
  rq <- srdaConvertRequest(request)
  
  REPLACE_CLAUSE <- "" #temp
  rq$select <- paste(rq$select, REPLACE_CLAUSE, collapse="", sep=" ")
  
  srdaRequestData(queryURL,
                  rq$select, rq$from, rq$where, DATA_SEP, ADD_SQL)
  
  data <- srdaGetData()
  
  #if (DEBUG) print(data)
  
  # save current data frame to RData file
  save(data, file = rdataFile)
  
  # clean up
  rm(data)
}


message("\nRetrieving SourceForge data...\n")

#getSourceForgeData("SELECT * FROM sf0305.users WHERE user_id < 100 ")
getSourceForgeData("SELECT * 
FROM sf1104.users a, sf1104.artifact b
WHERE a.user_id = b.submitted_by AND b.artifact_id = 304727")

# clean up
close(curl)