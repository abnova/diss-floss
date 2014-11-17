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

# library() calls are still needed in case of a system, lacking
# these packages, as require() load only installed packages
library(RCurl)
library(jsonlite)
library(stringr)

PRJ_HOME  <- Sys.getenv("DISS_FLOSS_HOME")
SRDA_USER <- Sys.getenv("SRDA_USER")
SRDA_PASS <- Sys.getenv("SRDA_PASS")

source(file.path(PRJ_HOME, "config/diss-floss-config.R"))
source(file.path(PRJ_HOME, "utils/string.R"))

skipped <<- 0 # counter for # of times the script skipped processing

# SRDA data collection configuration template file
SRDA_TEMPLATE <- "SourceForge.cfg.tmpl"

# SRDA data collection configuration file (auto-generated)
SRDA_CONFIG <- "SourceForge.cfg.json"

# Users must authenticate to access Query Form
SRDA_HOST_URL  <- "http://zerlot.cse.nd.edu"
SRDA_LOGIN_URL <- "/mediawiki/index.php?title=Special:Userlogin"
SRDA_LOGIN_REQ <- "&action=submitlogin&type=login"

# SRDA URL that Query Form sends POST requests to
SRDA_QUERY_URL <- "/cgi-bin/form.pl"

# SRDA URL that Query Form sends POST requests to
SRDA_QRESULT_URL <- "/qresult/blekh/blekh.txt"

RESULTS_URL <- paste0(SRDA_HOST_URL, SRDA_QRESULT_URL)

POLL_TIME <- 2 # polling timeout in seconds

# Parameters for result's format
DATA_SEP <- ":" # data separator
ADD_SQL  <- "0" # add SQL to file

REPLACE_CLAUSE <- "REPLACE(REPLACE(REPLACE(a.details, ':', ';'), CHR(10),' '), CHR(13),' ')"

RQ_SIZE <- 50000 # number of records returned by a single SQL query

SPECIFY_PROJECT_ID_RANGE <<- TRUE # depends on 'resultSize' config. attr.
PID_LOW <- 1
PID_HIGH <<- 0 # auto-initialized by 'resultSize' config. attribute

#TODO: consider passing this via CL args
RDATA_DIR <- file.path(PRJ_HOME, "cache/SourceForge")

envVarFound <- FALSE

# Data source prefix (to construct data object names)
dsPrefix <- ""

blacklist <- c()

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

  ##temp
  # fix due to fast server, responding within the same second as request
  #Sys.sleep(1)
  
  #if (DEBUG2) {print(select); print(from); print(where)}
  
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

#srdaGetResult() might be a better name

srdaGetData <- function (NUM_ROWS_RQ = FALSE) {
  
  debug2saved <- DEBUG2
  
  ##temp (TODO: uncomment)
  #if (NUM_ROWS_RQ) DEBUG2 <<- FALSE
  
  if (DEBUG2) message("Waiting for results ... ", appendLF = FALSE)
  
  # simple polling of the results file
  repeat {
    afterDate <- url.exists(RESULTS_URL, .header=TRUE)["Last-Modified"]
    afterDate <-  strptime(afterDate, "%a, %d %b %Y %X", tz="GMT")

    delta <- difftime(afterDate, beforeDate, units = "secs")
    if (as.numeric(delta) != 0) { # file modified, results are ready
      if (DEBUG2) message(" Ready!\n")
      break
    }
    else { # no results yet, wait the timeout and check again
      if (DEBUG2) {
        message(">", appendLF = FALSE)
        flush.console()
      }
      Sys.sleep(POLL_TIME)
    }
  }
  
  DEBUG2 <<- debug2saved
  
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
  
  # Then we need to replace all occurences of
  #
  # ": "       with "!@#"
  # "X::Y"     with "X@@Y"
  # "http://"  with "http//"
  # "https://" with "https//"
  # "mailto:"  with "mailto@"
  # "svn://"   with "svn//",
  #
  # since we have to use semicolon as a field separator,
  # to prevent incorrect parsing of the data.
  #
  # After the processing, we have to return data
  # (now in a data frame) to the original state (post-processing).
  # Note, that the following substitution code works only for
  # the specific data separator ':'. More universal code is TBD.
  
  rx <- "([[:alpha:]][^.:]|[[:blank:]])::([[:alpha:]][^:]|[[:blank:]])"
  results <- gsub(rx, "\\1@@\\2", results)
  results <- gsub(": ", "!@#", results) # should be after the ::-gsub
  results <- gsub("http://", "http//", results)
  results <- gsub("https://", "https//", results)
  results <- gsub("mailto:", "mailto@", results)
  results <- gsub("svn://", "svn//", results)
  
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
  
  # remove multiple LF characters within a project record, if any
  rx <- "\\n([^0-9])"
  while(any(grepl(rx, results))) {results <- gsub(rx, "\\1", results)}
  
  # fix for improperly formatted result data (AniSa, project 7606)
  results <- gsub("\\n:gpl:962356288", ":gpl:962356288", results)
  
  # Then we read intermediate results as text lines, count lines
  # and then delete last character on each line (extra separator)
  results <- readLines(textConnection(unlist(results)))
  numLines <- length(results)
  results <- lapply(results, function(x) gsub(".$", "", x))
  #if (DEBUG2) print(head(results))
  
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
  data <- replace_all(data, fixed("svn//"), "svn://")
  
  #if (DEBUG2) print("==========")
  #if (DEBUG2) print(head(data))
  
  return (data)
}


# Parse SRDA data collection JSON-like config. template file
# by substituting all references to config. variables with
# corresponding JSON elements' values and generate config. file.
generateConfig <- function(configTemplate, configFile) {
  
  suppressPackageStartupMessages(suppressWarnings(library(tcltk)))
  if (!suppressMessages(require(gsubfn)))
    install.packages('gsubfn', repos="http://cran.r-project.org")
  library(gsubfn)
  
  regexKeyValue <- '"_([^"]*)":"([^"]*)"'
  regexVariable <- "[$]{([[:alpha:]][[:alnum:].]*)}"

  cfgTmpl <- readLines(configTemplate)
  
  defns <- strapplyc(cfgTmpl, regexKeyValue, simplify = rbind)
  dict <- setNames(as.list(defns[, 2]), defns[, 1])
  config <- gsubfn(regexVariable, dict, cfgTmpl)
  
  # escape JSON's invalid '\n' characters - fix for 'jsonlite'
  #config <- paste(config, collapse = paste0("\\", 'n'))
  config <- paste(config, collapse = '')
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

cfgElementsChanged <- function (rdataFile,
                                dataName, ATTRS, configInfo) {
  
  changed <- FALSE
  
  assign(dataName, readRDS(rdataFile))
  if (DEBUG) {
    #message("\nRetrieved object '", dataName, "', containing:\n")
    #message(str(get(dataName)))
  }
  
  for (attrib in ATTRS) {
    objectAttrib <- attr(get(dataName), attrib, exact = TRUE)
    if (is.null(objectAttrib)) {
      if (DEBUG)
        message("Object '", dataName, "' doesn't have attribute \"",
                attrib, "\"\n")
    }
    else {
      configAttrib <- configInfo[[attrib]]
      #if (DEBUG) message("configAttrib = ", toString(configAttrib))
      #if (DEBUG) message("objectAttrib = ", toString(objectAttrib))
    }
    if (!identical(configAttrib, objectAttrib)) {
      changed <- TRUE
      break
    }
  }
  
  return (changed)
}


getSourceForgeData <- function (row, config) { # dataFrame
  
  dfList <- list()
  
  # Extract indicator's name, SQL query and result's variable names
  # from the function's argument
  indicator <- config$data[row, "indicatorName"]
  request <- config$data[row, "requestSQL"]
  varNames <- config$data[row, "resultNames"]
  
  # construct name from data source prefix and data ID (see config. file),
  # so that corresponding data object (usually, data frame) will be saved
  # later under that name via save() and/or read into via readRDS()
  dataName <- paste(dsPrefix, "data", indicator, sep = ".")
  
  # generate corresponding RDS file name;
  # calculate request's SQL query digest as well as
  # digests for indicator name and data object names
  fileName <- paste0(indicator, RDS_EXT)
  rdataFile <- file.path(RDATA_DIR, fileName)
  
  # construct configuration-based attribute info for verification
  ATTRS <- c("indicatorName", "resultNames", "SQL")
  configAttrs <- list(indicator, varNames, request)
  configInfo <- lapply(configAttrs, base64)
  names(configInfo) <- ATTRS
  
  # check if the archive file has already been processed
  if (DEBUG) {
    message("Collecting data for \'", indicator, "\' ... ",
            appendLF = FALSE)
  }
  if (DEBUG2) {message("Processing request \"", request, "\" ...")}
  
  if (file.exists(rdataFile)) {
    # now check if any indicator's attributes have been modified
    if (!cfgElementsChanged(rdataFile, dataName, ATTRS, configInfo)) {
      skipped <<- skipped + 1
      if (DEBUG)
        message("Skipped.")
      if (DEBUG2)
        message("Processing skipped: RDS cache file is up-to-date.\n")
      return (invisible())
    }
    else {
      # change is detected, delete file and continue with processing
      unlink(rdataFile)
    }
  }
  
  # Construct SRDA query URL
  queryURL <- paste0(SRDA_HOST_URL, SRDA_QUERY_URL)
  
  # Convert (tokenize) SQL request into parts
  rq <- srdaConvertRequest(request)

  REPLACE_CLAUSE <- "" #temp
  rq$select <- paste(rq$select, REPLACE_CLAUSE)
  
  if (rq$where == '')
    where <- ''
  else
    where <- rq$where
  
  # synchronize this flag with corresponding config. attribute
  SPECIFY_PROJECT_ID_RANGE <<- 
    ifelse(config$data[row, "resultSize"] == 'all', FALSE, TRUE)
  
  # Setup Project ID range specification, if needed
  if (SPECIFY_PROJECT_ID_RANGE) {
    
    PID_HIGH <<- config$data[row, "resultSize"]
    
    if (grepl("GROUP BY", where)) { # handle GROUP BY special case
      parts <- strsplit(where, "GROUP BY")
      leftOfGB <- parts[[1]][1]
      rightOfGB <- parts[[1]][2]
      whereBeforeGB <- paste(leftOfGB, 'AND',
                             'group_id BETWEEN', PID_LOW, 'AND', PID_HIGH)
      where <- paste(whereBeforeGB, "GROUP BY", rightOfGB)
    } else {
      if (where != '') where <- paste(where, 'AND')
      where <- paste(where,
                     'group_id BETWEEN', PID_LOW, 'AND', PID_HIGH)
    }
  }
  
  # First, retrieve total number of rows for the request
  # (we use subselect here as some queries use aggregate functions)
  myWhere <- paste(where, ")", "AS MyAlias")
  myFrom <- paste("(", "SELECT", rq$select, "FROM", rq$from)
  myFrom <- ifelse(where == '', paste(myFrom, myWhere), myFrom)
  myWhere <- ifelse(where == '', '', myWhere)
  
  success <- 
    srdaRequestData(queryURL, "COUNT(*)", myFrom, myWhere,
                    DATA_SEP, ADD_SQL)
  if (!success) error("Data request failed!")
  
  assign(dataName, srdaGetData(TRUE))
  data <- get(dataName)
  
  # local function (consider moving to larger scope)
  numPages <- function (lines, linesPerPage) {
    numPages <- as.numeric(lines) %/% linesPerPage
    if (as.numeric(lines) %% linesPerPage > 0)
      numPages <- numPages + 1
    return (numPages)
  }
  
  # Determine number of requests, based on the whole result
  numRequests <- numPages(data, RQ_SIZE)
  
  # Determine whether we have a configuration limit
  # for the result size and handle the request accordingly
  resultSize <- config$data[row, "resultSize"]
  resultSizeNum <- suppressWarnings(as.numeric(resultSize))
  
  # is.finite() is needed since is.numeric(NA) is TRUE (per R docs)
  if (is.numeric(resultSizeNum) && is.finite(resultSizeNum)) {
    numRequestsCfg <- numPages(resultSize, RQ_SIZE)
    # make sure we don't request more data than there exist
    if (numRequests > numRequestsCfg)
      numRequests <- numRequestsCfg
  }
  else {
    if (resultSize != "all")
      warning("Result size incorrectly specified, assuming <all>!")
  }
  
  if (DEBUG) message("Page: ", appendLF = FALSE)

  # Now, we can request & retrieve data via SQL pagination
  for (i in 1:numRequests) {
    
    # use previously generated WHERE clause for re-initializing 'whereLoop'
    whereLoop <- where
    
    # Prepare and setup SQL pagination
    if (whereLoop == '') whereLoop <- '1=1'
    
    # Re-use here the already prepared (Project ID range specified)
    # WHERE clause from the Count request code block above
    whereLoop <- paste(whereLoop,
                   'LIMIT', RQ_SIZE, 'OFFSET', RQ_SIZE*(i-1))
    
    # Submit data request
    success <- srdaRequestData(queryURL, rq$select, rq$from, whereLoop,
                               DATA_SEP, ADD_SQL)
    if (!success) error("Data request failed!")
    
    assign(dataName, srdaGetData())
    data <- get(dataName)
    # alternative to using get(), but more cumbersome:
    # data <- eval(parse(text=dataName))
    
    # save hash of the request's SQL query as data object's attribute,
    # so that we can detect when configuration contains modified query
    attr(data, "SQL") <- base64(request)
    
    # set other attributes for the data object
    attr(data, "indicatorName") <- base64(indicator)
    attr(data, "resultNames") <- base64(varNames)
    
    # specify names for the current data object per configuration
    varNamesModif <- strsplit(varNames, split = ",")
    varNamesModif <- lapply(varNamesModif, str_trim)
    names(data) <- unlist(varNamesModif)
    
    # remove projects from the blacklist
    data <- data[!(data[["Project ID"]] %in% blacklist), ]
    
    # add current data frame to the list
    dfList[[i]] <- data
    if (DEBUG) message(i, " ", appendLF = FALSE)
  }
  
  # merge all the result pages' data frames
  data <- do.call(rbind, dfList)
  
  # save current data frame to RDS file
  saveRDS(data, rdataFile)
  
  if (DEBUG) message("=> Done.")
  
  # clean up
  rm(data)
  gc()
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

numRequests <- nrow(config$data)

if (DEBUG) {
  msg <- paste0("Data ", config["_action"],
                " from ", config["_source"],
                ", using schema \"", config["_schema"], "\".")
  message(msg)
  
  msg <- paste("Total number of requests to submit:", numRequests)
  message(msg, "\n")
}

# Save data source prefix in a global variable
dsPrefix <<- config["_prefix"]

# Process projects' blacklist
blacklist <<- config["_blacklist"]

blacklistPIDs <- strsplit(unlist(blacklist), split = ",")
blacklistPIDs <- lapply(blacklistPIDs, str_trim)
blacklist <<- unlist(blacklistPIDs)

# initialize env. var. for outlier control
setEnvVar <- function (envVarName, configAttrName) {
  
  configVal <- unlist(config[configAttrName])
  fileVal <- ""
  valueChanged <- FALSE
  
  # read in the data
  #lines <- scan(R_ENV_FILE, what="", sep="\n", comment.char = "#")
  lines <- readLines(R_ENV_FILE)
  
  # separate elements by one or more whitepace
  y <- strsplit(lines, "[[:space:]]+")
  
  # check whether the value has been changed
  for (i in seq_len(length(y))) {
    if (envVarName %in% y[[i]][1]) { # env. var. found
      envVarFound <- TRUE
      fileVal <- y[[i]][3]
      if (fileVal != configVal) { # value changed
        lines[i] <- paste(envVarName, '=', configVal)
        valueChanged <- TRUE
      }
    }
  }
  
  if (envVarFound) {
    if (valueChanged) {
      unlink(R_ENV_FILE)
      writeLines(lines, R_ENV_FILE)
    }
  }
  else { # no env. var. found - just add corresponding line
    strToAdd <- paste(envVarName, '=', configVal, "\n")
    cat(strToAdd, file = R_ENV_FILE, append = TRUE)
  }
}

# initialize env. var. for outlier control
setEnvVar("OUTLIER_LIM_DEV_TEAM_SIZE", "_outlier_limit_DevTeamSize")

# Create cache directory, if it doesn't exist
if (!file.exists(RDATA_DIR)) {
  dir.create(RDATA_DIR, recursive = TRUE, showWarnings = FALSE)
}

message("Authenticating with SRDA ...\n")

# Construct SRDA login URL
loginURL <- paste0(SRDA_HOST_URL, SRDA_LOGIN_URL, SRDA_LOGIN_REQ)

# Log into the system
success <- srdaLogin(loginURL, SRDA_USER, SRDA_PASS)
if (!success) error("Authentication failed!")

#try(srdaLogin(loginURL, getOption("SRDA_USER"), getOption("SRDA_PASS")))

message("Retrieving SourceForge data ...\n")

# Collect data, iterating through the request queue
allData <- lapply(seq(numRequests),
                  function(row) getSourceForgeData(row, config))

if (skipped == 0) message("") # add new line
if (DEBUG)
  message("Data elements/requests statistics - processed: ",
          numRequests - skipped, ", skipped: ", skipped, ".\n")

message("SourceForge data collection completed successfully.\n")

# clean up, with a side effect of writing cookie file to disk
rm(curl)
x <- gc()