#' Module: getFLOSSmoleData.R
#'
#' Downloads FLOSS projects data from FLOSSmole repository
#' (in CSV format) and converts it to data frame format.
#'
#' @author Aleksandr Blekh \email{blekh@@nova.edu}

if (!require(RCurl)) install.packages('RCurl')
if (!require(XML)) install.packages('XML')
if (!require(digest)) install.packages('digest')

library(RCurl)
library(XML)
library(digest)

source("../utils/debug.R")

# URL of FLOSSmole repository root directory
FLOSSMOLE_REPO_BASE <- "http://flossdata.syr.edu/data"

# Create a lookup table to construct correct paths to files
REPO_CODE  <- c("fc",   "fsf",  "gc",   "gh",   "lpd",  "sv",   "tig")
REPO_YEAR  <- c("2013", "2012", "2012", "2013", "2012", "2013", "2013")
REPO_MONTH <- c("Dec",  "Nov",  "Nov",  "Feb",  "Sep",  "Dec",  "Dec")
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
RDATA_EXT <- ".Rdata"
RDATA_DIR <- "../cache/FLOSSmole" #TODO: consider passing this via CL args

lookup <- data.frame(digest = "", url = "", stringsAsFactors = FALSE)

DATA_LOOKUP <- FALSE
LOOKUP_FILE <- "DataLookup"

DATA_ATTRIB <- TRUE
ATTRIB_NAME <- "DataSource"

DEBUG <- TRUE # TODO: retrieve debug flag via CL arguments


importRepoFiles <- function(repos, row) {
  
  message("* Verifying repository: ", repos$name[row], " *",
          ifelse(DEBUG, "\n", ""))
  
  # construct URL for current FLOSS repository in FLOSSmole
  url <- paste(FLOSSMOLE_REPO_BASE, "/",
               repos$code[row], "/",
               repos$year[row], "/",
               repos$year[row], "-", repos$month[row],
               collapse="", sep="")

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
    
    # calculate URL's digest and generate corresponding RData file name
    fileDigest <- digest(url, algo="md5", serialize=F)
    rdataFile <- paste(RDATA_DIR, "/", fileDigest, RDATA_EXT, sep = "")
    
    # check if the archive file has already been processed
    if (DEBUG) {message("Checking file \"", url, "\"...")}
    if (file.exists(rdataFile)) {
      if (DEBUG) {message("Processing skipped: .Rdata file found.\n")}
      if (DEBUG) {
        print(load(rdataFile))
        print(class(fileData))
      }
      return()
    }
    
    # current method
    if (TRUE) { # via local file
      
      file <- tempfile(pattern = "tmp", tmpdir = ".", fileext = ".bz2")    
      download.file(url, destfile = file, mode = "w")
      data <- bzfile(file, open = "r")
      try(fileData <- read.table(data, header = TRUE, fill = TRUE,
                                 sep = "\t"),
          silent = FALSE)
      
      if (DATA_ATTRIB) {

        # set URL as DF's attribute to be stored as metadata
        # for future lookups when restoring data from R objects
        attr(fileData, ATTRIB_NAME, exact = TRUE) <- url
        
      } else if (DATA_LOOKUP) {
        
        # for convinience we use full file name instead of digest
        rbind(lookup, rdataFile, url)
      }
      
      # save current data frame to RData file
      save(fileData, file = rdataFile)
      
      # clean up
      rm(fileData)
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
  
  if (DATA_LOOKUP) {
    # save lookup data frame to a separate RData file
    save(lookup, file = LOOKUP_FILE)
  }
}


# Retrieve all data by iterating through requested repositories
getFLOSSmoleData <- function(repos) {
  lapply(row <- 1:nrow(repos), function(row) importRepoFiles(repos, row))
}


message("\nRetrieving FLOSSmole data...\n")
if (DEBUG) {
  print(system.time(getFLOSSmoleData(repos)))
} else {
  system.time(getFLOSSmoleData(repos))
}
message("\n")