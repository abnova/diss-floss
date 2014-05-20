#' Module: getSourceForgeData.R
#'
#' Downloads FLOSS projects data from SourceForge.net repository
#' via SourceForge Research Data Archive (SRDA), by using RCurl
#' to access SRDA Query Form for sending queries and receiving replies.
#'
#' @author Aleksandr Blekh \email{blekh@@nova.edu}

# TODO:
# Wrap all package installation & loading
# in a function or, at least, do it via vector.

if (!suppressMessages(require(RCurl))) install.packages('RCurl')
if (!suppressMessages(require(jsonlite)))
  install.packages("jsonlite", repos="http://cran.r-project.org")
if (!suppressMessages(require(stringr))) install.packages('stringr')

# INFO: Possible methods of suppressing messages
#suppressMessages(library(RCurl))
#suppressPackageStartupMessages(library(RCurl))
#invisible(capture.output(library(RCurl, quietly=TRUE)))

# library() calls are not needed as require() load packages, too
#library(RCurl)
#library(jsonlite)
#library(stringr)

source("../utils/utils.R")
#source("../utils/debug.R")
#source("../utils/string.R")

skipped <<- 0 # counter for # of times the script skipped processing

# SRDA data collection configuration template file
SRDA_TEMPLATE <- "./SourceForge.cfg.tmpl"

# SRDA data collection configuration file (auto-generated)
SRDA_CONFIG <- "./SourceForge.cfg.json"

# Users must authenticate to access Query Form
SRDA_HOST_URL  <- "http://zerlot.cse.nd.edu"
SRDA_LOGIN_URL <- "/mediawiki/index.php?title=Special:Userlogin"
SRDA_LOGIN_REQ <- "&action=submitlogin&type=login"

# SRDA URL that Query Form sends POST requests to
SRDA_QUERY_URL <- "/cgi-bin/form.pl"

# SRDA URL that Query Form sends POST requests to
SRDA_QRESULT_URL <- "/qresult/blekh/blekh.txt"

RESULTS_URL <- paste0(SRDA_HOST_URL, SRDA_QRESULT_URL)

POLL_TIME <- 5 # polling timeout in seconds

# Parameters for result's format
DATA_SEP <- ":" # data separator
ADD_SQL  <- "0" # add SQL to file

REPLACE_CLAUSE <- "REPLACE(REPLACE(REPLACE(a.details, ':', ';'), CHR(10),' '), CHR(13),' ')"

RDATA_EXT <- ".RData"
RDS_EXT <- ".rds"
RDATA_DIR <- "../cache/SourceForge" #TODO: consider passing this via CL args
ATTR <- "SQL"

# Data source prefix (to construct data object names)
dsPrefix <- ""

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
  
  sql <- strsplit(request, split = "SELECT|FROM|WHERE")
  sql <- sql[[1]][-1] # remove 1st empty element produced by strsplit()
  if (length(sql) == 2) # 'where' is empty, add dummy element
    sql <- c(sql, "")
  names(sql) <- c("select", "from", "where")
  sql <- lapply(sql, str_trim)
  
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
  
  # check and save 'last modified' date and time of the results file
  # before submitting data request, to compare with the same after one
  # for simple polling of results file in srdaGetData() function
  beforeDate <- url.exists(RESULTS_URL, .header=TRUE)["Last-Modified"]
  beforeDate <<- strptime(beforeDate, "%a, %d %b %Y %X", tz="GMT")
  
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
  
  if (DEBUG) message("Waiting for results ...", appendLF = FALSE)
  
  # simple polling of the results file
  repeat {
    afterDate <- url.exists(RESULTS_URL, .header=TRUE)["Last-Modified"]
    afterDate <-  strptime(afterDate, "%a, %d %b %Y %X", tz="GMT")
    delta <- difftime(afterDate, beforeDate, units = "secs")
    if (as.numeric(delta) != 0) { # file modified, results are ready
      if (DEBUG) message(" Ready!\n")
      break
    }
    else { # no results yet, wait the timeout and check again
      if (DEBUG) message(".", appendLF = FALSE)
      Sys.sleep(POLL_TIME)
    }
  }
  
  # Some pre-processing is needed to correctly parse results
  
  # First, read file as a stream of characters, so that later we can
  # replace the CR/LF characters pair with a single space character
  # to prevent incorrect parsing of fields with embedded newline
  # by the read.table() function.
  fileLen <- url.exists(RESULTS_URL, .header=TRUE)["Content-Length"]
  if (is.na(fileLen)) {
    if (DEBUG) message("Empty result for request, nothing to process!\n")
    return (invisible())
  }
  results <- readChar(RESULTS_URL, nchars = fileLen, TRUE)
  
  # Then we need to replace all occurences of ": " with "!@#",
  # "X::Y" with "X@@Y", and "http://" with "http//", as well as
  # "https://" with "https//" (secure HTTP), since we have to
  # use semicolon as a field separator, to prevent incorrect parsing
  # of the data. After the processing, we have to return data
  # (now in a data frame) to the original state (post-processing).
  # Note, that the following substitution code works only for
  # the specific data separator ':'. More universal code is TBD.
  rx <- "([[:alpha:]][^.:]|[[:blank:]])::([[:alpha:]][^:]|[[:blank:]])"
  results <- gsub(rx, "\\1@@\\2", results)
  results <- gsub(": ", "!@#", results) # should be after the ::-gsub
  results <- gsub("http://", "http//", results)
  results <- gsub("https://", "https//", results)
  results <- gsub("mailto:", "mailto@", results)
  
  # Since some results contain fields with embedded newlines,
  # direct use of read.table() parses data incorrectly.
  
  # Then, we have to replace the problematic pair of characters
  # (\0xD\0xA - CR/LF - \r\n) with a single space character (' ').
  # It's important that the next line is executed after the data
  # separator-related (':') substitutions, otherwise results with
  # newlines right after data separator will be parsed incorrectly
  # (":\r\n" => ": " => "!@#" => loss of one data field).
  results <- gsub("-\\r\\n", "-", results) # order is important here
  results <- gsub("\\r\\n", " ", results)
  
  # fix for improperly formatted result data (AniSa, project 7606)
  results <- gsub("\\n:gpl:962356288", ":gpl:962356288", results)
  
  # Then we read intermediate results as text lines, count lines
  # and then delete last character on each line (extra separator)
  results <- readLines(textConnection(unlist(results)))
  numLines <- length(results)
  results <- lapply(results, function(x) gsub(".$", "", x))
  #if (DEBUG) print(head(results))
  
  # Then we can parse the intermediate results as usual
  data <- read.table(textConnection(unlist(results)),
                     header = FALSE, fill = TRUE,
                     sep = DATA_SEP, quote = "",
                     colClasses = "character", row.names = NULL,
                     nrows = numLines, comment.char = "",
                     strip.white = TRUE)
  
  # Now we can safely do post-processing, recovering original data
  data <- replace_all(data, fixed("!@#"), ": ")
  data <- replace_all(data, fixed("@@"), "::")
  data <- replace_all(data, fixed("http//"), "http://")
  data <- replace_all(data, fixed("https//"), "https://")
  data <- replace_all(data, fixed("mailto@"), "mailto:")
  
  #if (DEBUG) print("==========")
  #if (DEBUG) print(head(data))
  
  return (data)
}


# Parse SRAD data collection JSON-like config. template file
# by substituting all references to config. variables with
# corresponding JSON elements' values and generate config. file.
generateConfig <- function(configTemplate, configFile) {
  
  suppressPackageStartupMessages(suppressWarnings(library(tcltk)))
  if (!suppressMessages(require(gsubfn))) install.packages('gsubfn')
  #library(gsubfn)
  
  regexKeyValue <- '"_([^"]*)":"([^"]*)"'
  regexVariable <- "[$]{([[:alpha:]][[:alnum:].]*)}"
  
  cfgTmpl <- readLines(configTemplate)
  
  defns <- strapplyc(cfgTmpl, regexKeyValue, simplify = rbind)
  dict <- setNames(as.list(defns[, 2]), defns[, 1])
  config <- gsubfn(regexVariable, dict, cfgTmpl)
  
  writeLines(config, con = configFile)
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
  
  # Extract indicator's name & SQL query from the function's argument
  indicator <- config$data[row, "indicatorName"]
  request <- config$data[row, "requestSQL"]
  
  # construct name from data source prefix and data ID (see config. file),
  # so that corresponding data object (usually, data frame) will be saved
  # later under that name via save() and/or read into via readRDS()
  dataName <- paste(dsPrefix, "data", indicator, sep = ".")
  
  # calculate request's indicator digest and generate corresponding
  # RData file name; also calculate request's SQL query digest
  fileDigest <- base64(indicator)
  rdataFile <- paste(RDATA_DIR, "/", fileDigest, RDS_EXT, sep = "")
  requestDigest <- base64(request)
  
  # check if the archive file has already been processed
  if (DEBUG) {message("Processing request \"", request, "\" ...")}
  if (file.exists(rdataFile)) {
    # now check if request's SQL query hasn't been modified
    assign(dataName, readRDS(rdataFile))
    if (DEBUG) {
      #message("\nRetrieved object '", dataName, "', containing:\n")
      #message(str(get(dataName)))
    }
    requestAttrib <- attr(get(dataName), ATTR, exact = TRUE)
    if (is.null(requestAttrib)) {
      if (DEBUG)
        message("Object '", dataName, "' doesn't have attribute \"",
                ATTR, "\"\n")
    }
    else {
      #if (DEBUG) message(toString(requestDigest))
      #if (DEBUG) message(toString(requestAttrib))
    }
    if (identical(requestDigest, requestAttrib)) {
      skipped <<- skipped + 1
      if (DEBUG)
        message("Processing skipped: RDS cache file is up-to-date.\n")
      return (invisible())
    }
  }
  
  # Construct SRDA query URL
  queryURL <- paste(SRDA_HOST_URL, SRDA_QUERY_URL, collapse="", sep="")
  
  # Convert (tokenize) SQL request into parts
  rq <- srdaConvertRequest(request)
  
  REPLACE_CLAUSE <- "" #temp
  rq$select <- paste(rq$select, REPLACE_CLAUSE, collapse="", sep=" ")
  
  # Submit data request
  success <- srdaRequestData(queryURL, rq$select, rq$from, rq$where,
                             DATA_SEP, ADD_SQL)
  if (!success) error("Data request failed!")
  
  assign(dataName, srdaGetData())
  data <- get(dataName)
  # alternative to using get(), but more cumbersome:
  # data <- eval(parse(text=dataName))
    
  # save hash of the request's SQL query as data object's attribute,
  # so that we can detect when configuration contains modified query
  attr(data, "SQL") <- base64(request)
  
  # save current data frame to RDS file
  #save(list = dataName, file = rdataFile)
  saveRDS(data, rdataFile)
  # alternatively, use do.call() as in "getFLOSSmoleDataXML.R"
  #do.call(save, list(table, file = rdataFile))
  
  # clean up
  rm(data)
}


# Verify the need to generate configuration file,
# based on files' presence and modification times

updateNeeded <- function () {
  
  if (file.exists(SRDA_CONFIG)) {
    if (file.exists(SRDA_TEMPLATE)) {
      delta <- difftime(file.info(SRDA_CONFIG)$mtime,
                        file.info(SRDA_TEMPLATE)$mtime,
                        units = "secs")
      update <- as.numeric(delta) < 0  # config is older
    }
    else {
      warning("Configuration template file is missing!")
      update <- FALSE
    }
  } else if (file.exists(SRDA_TEMPLATE)) {
    update <- TRUE
  } else {
    stop("Template and configuration files are both missing!")
    # stop execution of the script as critical config info is missing
    quit(status = -1)
  }
  
  return (update)
}


message("\n=== SRDA data collection ===\n")

# Generate configuration file, if needed
if (updateNeeded()) {
  
  message("Parsing configuration template file ...\n")
  
  # Variables in JSON-based config. template file are as follows:
  # ${elem} - refers to the 'elem' JSON element in the file
  # %{val} - refers to the value of the argument passed by caller
  generateConfig(SRDA_TEMPLATE, SRDA_CONFIG)
}

message("Reading configuration file ...\n")

config <- jsonlite::fromJSON(SRDA_CONFIG)

if (DEBUG) {
  msg <- paste("Data ", config["_action"],
               " from ", config["_source"],
               ", using schema \"", config["_schema"], "\".", sep = "")
  message(msg)
  
  msg <- paste("Total number of requests to submit:", nrow(config$data))
  message(msg, "\n")
}

# Save data source prefix in a global variable
dsPrefix <<- config["_prefix"]

# Create cache directory, if it doesn't exist
if (!file.exists(RDATA_DIR)) {
  dir.create(RDATA_DIR, recursive = TRUE, showWarnings = FALSE)
}

message("Authenticating with SRDA ...\n")

# Construct SRDA login URL
loginURL <- paste(SRDA_HOST_URL, SRDA_LOGIN_URL, SRDA_LOGIN_REQ,
                  collapse="", sep="")

# Log into the system
success <- srdaLogin(loginURL, SRDA_USER, SRDA_PASS)
if (!success) error("Authentication failed!")

#try(srdaLogin(loginURL, getOption("SRDA_USER"), getOption("SRDA_PASS")))

message("Retrieving SourceForge data ...\n")

# Collect data, iterating through the request queue
allData <- lapply(seq(nrow(config$data)),
                  function(row) getSourceForgeData(row, config))

if (skipped == 0) message("") # add new line
message("SourceForge data collection completed successfully.\n")

# clean up, with a side effect of writing cookie file to disk
rm(curl)
x <- gc()