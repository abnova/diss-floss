#' Module: getFLOSSmoleData.R
#'
#' Downloads FLOSS projects data from FLOSSmole repository
#' (in CSV format) and converts it to data frame format.
#'
#' @author Aleksandr Blekh \email{blekh@@nova.edu}

if (!suppressMessages(require(RCurl))) install.packages('RCurl')
if (!suppressMessages(require(XML))) install.packages('XML')

library(RCurl)
library(XML)

PRJ_HOME <- Sys.getenv("DISS_FLOSS_HOME")

source(file.path(PRJ_HOME, "config/diss-floss-config.R"))
source(file.path(PRJ_HOME, "utils/debug.R"))

#warnings(file = "./FLOSSmole.warn.log")

# URL of FLOSSmole repository root directory
FLOSSMOLE_REPO_BASE <- "http://flossdata.syr.edu/data"

# Create a lookup table to construct correct paths to files
REPO_CODE  <- c("fc",   "fsf",  "gc",   "gh",   "lpd",  "sv",   "tig")
REPO_YEAR  <- c("2014", "2012", "2012", "2013", "2012", "2014", "2014")
REPO_MONTH <- c("Mar",  "Nov",  "Nov",  "Feb",  "Sep",  "Mar",  "Mar")
REPO_NAME  <- c("FreeCode",
                "Free Software Foundation",
                "Google Code",
                "GitHub",
                "LaunchPad",
                "Savannah",
                "Tigris")

repos <- data.frame(code = REPO_CODE, year = REPO_YEAR,
                    month = REPO_MONTH, name = REPO_NAME,
                    stringsAsFactors = FALSE)

BZIP_EXT  <- ".txt\\.bz2"

RDATA_DIR <- file.path(PRJ_HOME, "cache/FLOSSmole")


importRepoFiles <- function(repos, row) {
  
  message("* Verifying repository: ", repos$name[row], " ...",
          ifelse(DEBUG, "\n", ""))
  
  # construct URL for current FLOSS repository in FLOSSmole
  url <- paste0(FLOSSMOLE_REPO_BASE, "/",
                repos$code[row], "/",
                repos$year[row], "/",
                repos$year[row], "-", repos$month[row])

  htmlPage = rawToChar(getBinaryURL(url, followlocation = TRUE))

  doc <- htmlParse(htmlPage, asText = TRUE)

  # current method
  if (TRUE) { # via HTML tables
    
    tables <- getNodeSet(doc, "//table")
    
    filenames <- readHTMLTable(tables[[1]], trim = TRUE,
                               stringsAsFactors = FALSE)
    
    filenames <- list(filenames[,2])
    
    filenames <- lapply(filenames,
                        function(x) grep(BZIP_EXT, x, value = TRUE))
  }
  else { # via XML elements
    
    links <- xpathSApply(doc, "//*/a[@class='href']", xmlValue)
    
    xpathSApply(doc, '//*[@class="href"]', xmlAttrs)
    
    links <- lapply(links, function(x) grep(BZIP_EXT, x, links))
  }
  
  # currently not used - does it make sense to use it perf.-wise?
  curlHandle <- getCurlHandle()
  
  # generate list of current repository files' FULL (absolute) URLs
  links <- lapply(filenames, function(x) paste(url, x, sep="/"))
  

  # Retrieve current repository's archived file and extract its
  # contents ("broken" CSV) into a data frame for further analysis
  getData <- function(i) {

    url <- links[[1]][i]
    
    # extract table name from URL so that corresponding data object
    # (data frame) will later be saved under that name via save()
    splitURL <- strsplit(url, "/|-")
    tableNameYear <-  splitURL[[1]][length(splitURL[[1]])-1]
    tableName <- substr(tableNameYear, 1, nchar(tableNameYear)-4)
    
    # generate corresponding RDS file name from FLOSSmole table name
    fileName <- paste0(tableName, RDS_EXT)
    rdataFile <- file.path(RDATA_DIR, fileName)
    
    # check if the archive file has already been processed
    if (DEBUG) {message("Checking file \"", url, "\"...")}
    if (file.exists(rdataFile)) {
      if (DEBUG) {message("Processing skipped: RDS file found.\n")}
      return()
    }
    
    # current method
    if (TRUE) { # via local file
      
      file <- tempfile(pattern = "tmp", tmpdir = ".", fileext = ".bz2")    
      download.file(url, destfile = file, mode = "w")
      data <- bzfile(file, open = "r")
      try(assign(tableName, read.table(data, header = TRUE, fill = TRUE,
                                       sep = "\t", quote = "",
                                       stringsAsFactors = FALSE)),
          silent = FALSE)
      
      # save hash of the indicator's URL as data object's attribute,
      # so that we can restore and use the URL later, when needed
      attr(data, "URL") <- base64(url)
      
      # save current data frame to RDS file
      table <- get(tableName)
      saveRDS(table, rdataFile)
      
      # clean up
      rm(table)
      close(data)
      unlink(file, force = TRUE)
    }
    else { # via RCurl
      
      bConn <- getBinaryURL(url, followlocation = TRUE)
      bzConn <- gzcon(rawConnection(bConn, "rb"))
      tConn <- bzfile(bzConn)
      try(read.table(tConn, sep = ",", row.names = NULL), silent = TRUE)
      close(tConn)
    }
  } #end_of_getData()

  # get data by iterating through list
  # of full URLs of the current repository files
  data <- lapply(seq_along(links[[1]]), getData)
}


# Retrieve all data by iterating through requested repositories
getFLOSSmoleData <- function(repos) {
  lapply(row <- 1:nrow(repos), function(row) importRepoFiles(repos, row))
}


message("\n=== FLOSSmole data collection ===\n")

# Create cache directory, if it doesn't exist
if (!file.exists(RDATA_DIR)) {
  dir.create(RDATA_DIR, recursive = TRUE, showWarnings = FALSE)
}

message("Retrieving FLOSSmole data ...\n")

allData <- getFLOSSmoleData(repos)

#print(system.time(getFLOSSmoleData(repos)))
#system.time(getFLOSSmoleData(repos))

message("\nFLOSSmole data collection completed successfully.\n")
