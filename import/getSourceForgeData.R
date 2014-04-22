#' Module: getSourceForgeData.R
#'
#' Downloads FLOSS projects data from SourceForge.net repository
#' via SourceForge Research Data Archive (SRDA), by using RCurl
#' to access SRDA Query Form for sending queries and receiving replies.
#'
#' @author Aleksandr Blekh \email{blekh@@nova.edu}

if (!require(RCurl)) install.packages('RCurl')
if (!require(digest)) install.packages('digest')
if (!require(jsonlite))
  install.packages("jsonlite", repos="http://cran.r-project.org")
if (!require(stringr)) install.packages('stringr')

library(RCurl)
library(digest)
library(jsonlite)
library(stringr)

source("../utils/debug.R")
source("../utils/string.R")

# SRDA data collection configuration file
SRDA_CONFIG <- "./SourceForge.json"

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

cookiesFile <- "cookies.txt"

curl <- getCurlHandle()

invisible(
  curlSetOpt(curl = curl, postredir = 3, #autoreferer = TRUE,
             cookiefile = cookiesFile, cookiejar = cookiesFile,
             ssl.verifyhost = FALSE, ssl.verifypeer = FALSE,
             followlocation = TRUE, verbose = FALSE)
  )


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

  params <- list('wpName' = username, 'wpPassword' = password,
                 'wpRemember' = "1")
  
  if(url.exists(loginURL)) {
    reply <- postForm(loginURL, .params = params, curl = curl,
                      style = "POST")
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
  
  sql <- unlist(strsplit(request, split = "SELECT|FROM|WHERE"))
  names(sql) <- c("dummy", "select", "from", "where")
  sql <- as.list(sql)
  sql <- lapply(sql, trimLT) #str_trim
  
  return (sql)
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
  
  params <- list('uitems' = select,
                 'utables' = from,
                 'uwhere' = where,
                 'useparator' = sep,
                 'append_query' = sql)
  
  if(url.exists(requestURL)) {
    reply <- postForm(requestURL, .params = params, #.opts = opts,
                      curl = curl, style = "POST")
    info <- getCurlInfo(curl)
    return (ifelse(info$response.code == 200, TRUE, FALSE))
  }
  else {
    error("Can't access request URL!")
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
  #if (DEBUG) print(results)
  
  data <- read.table(textConnection(unlist(results)), header = FALSE,
                     sep = DATA_SEP, quote = "\"",
                     colClasses = "character", row.names = NULL)
  #if (DEBUG) print("==========")
  #if (DEBUG) print(data)
  return (data)
}


# Substitute all references to config. variables and parameters
substituteVarParam <- function(config, request) {
  
  #print(config$data$requestSQL)
  VAR_REF   <- "\\$\\{\\w+\\}"
  PARAM_REF <- "\\%\\{\\w+\\}"
  
  vars <- str_locate_all(request, VAR_REF)
  print(vars)
  #str_replace_all(request, VAR_REF, paste(deparse(eval(config)), "$", varName)

  request <- "SELECT * FROM aa WHERE bb = 1"
  
  #var <- gregexpr(VAR_REF, request, fixed = TRUE)
  print("Hello")
  #if (var == -1) return
  #print(var)
  #elem <- substring(request, var, var + attr(var, "match.length") - 1)
  #print(elem)
  #for (e in elem) {
  #  if (nchar(e) == 0) next
  #  varName <- substring(e, 3, nchar(e) - 1)
  #  print(varName)
  #  val <- paste(deparse(eval(config)), "$", varName)
  #  print(e)
  #  print(val)
  #  print(request)
  #  gsub(e, val, request)
  
  #gsub(elem, config$elem, rq)
  #print(request)
  return (request)
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

getSourceForgeData <- function (row, config) { # dataFrame
  
  # $data
  # Extract SQL request from the function's argument
  #request <- dataFrame[row, "requestSQL"]
  request <- config$data[row, "requestSQL"]
  print(request)
  
  # calculate request's digest and generate corresponding RData file name
  fileDigest <- digest(request, algo="md5", serialize=F)
  rdataFile <- paste(RDATA_DIR, "/", fileDigest, RDATA_EXT, sep = "")
  
  # check if the archive file has already been processed
  if (DEBUG) {message("Processing request \"", request, "\"...")}
  if (file.exists(rdataFile)) {
    if (DEBUG) {message("Processing skipped: .Rdata file found.\n")}
    return(invisible())
  }
  
  # Construct SRDA login and query URLs
  loginURL <- paste(SRDA_HOST_URL, SRDA_LOGIN_URL, SRDA_LOGIN_REQ,
                    collapse="", sep="")
  queryURL <- paste(SRDA_HOST_URL, SRDA_QUERY_URL, collapse="", sep="")
  
  # Log into the system
  success <- srdaLogin(loginURL, SRDA_USER, SRDA_PASS)
  if (!success) error("Login failed!")
  
  #try(srdaLogin(loginURL, getOption("SRDA_USER"), getOption("SRDA_PASS")))

  # Substitute all references to config. variables and parameters
  request <- substituteVarParam(config, request)
  
  # Convert (tokenize) SQL request into parts
  rq <- srdaConvertRequest(request)
  
  REPLACE_CLAUSE <- "" #temp
  rq$select <- paste(rq$select, REPLACE_CLAUSE, collapse="", sep=" ")
  
  # Submit data request
  success <- srdaRequestData(queryURL, rq$select, rq$from, rq$where,
                             DATA_SEP, ADD_SQL)
  if (!success) error("Data request failed!")
  
  data <- srdaGetData()
  #if (DEBUG) print(data)
  
  # save current data frame to RData file
  save(data, file = rdataFile)
  
  # clean up
  rm(data)
}


message("\nReading configuration file ...\n")

# Variables in JSON-based config. file are as follows:
# ${elem} - refers to the 'elem' JSON element in the file
# %{val} - refers to the value of the argument passed by caller

config <- jsonlite::fromJSON(SRDA_CONFIG)
msg <- paste("Data ", config$action, " from ", config$source,
             ", using schema \"", config$schema, "\".", sep = "")
if (DEBUG) message(msg)
msg <- paste("Total number of requests to submit:", nrow(config$data))
if (DEBUG) message(msg)

message("\nRetrieving SourceForge data ...\n")

# Collect data, iterating through the request queue
lapply(seq_along(nrow(config$data)),
       function(row) getSourceForgeData(row, config))

message("\nSourceForge data collection finished. Status: SUCCESS.\n")

# clean up, with a side effect of writing cookie file to disk
rm(curl)
x <- gc()