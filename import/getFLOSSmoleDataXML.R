#' Module: getFLOSSmoleData.R
#'
#' Downloads FLOSS projects data from FLOSSmole repository
#' (in CSV format) and converts it to data frame format.
#'
#' @author Aleksandr Blekh \email{blekh@@nova.edu}

if (!require(RCurl)) install.packages('RCurl')
if (!require(XML)) install.packages('XML')

library(RCurl)
library(XML)


# URL of FLOSSmole repository root directory
FLOSSMOLE_REPO_BASE <- "http://flossdata.syr.edu/data"

# Create a lookup table to construct correct paths to files
REPO_CODE  <- c("fc",   "fsf",  "gc",   "gh",   "lpd",  "sv",   "tig")
REPO_YEAR  <- c("2013", "2012", "2012", "2013", "2012", "2013", "2013")
REPO_MONTH <- c("Dec",  "Nov",  "Nov",  "Feb",  "Sep",  "Dec",  "Dec")

repos <- data.frame(code = REPO_CODE, year = REPO_YEAR, month = REPO_MONTH,
                    stringsAsFactors=FALSE)

BZIP_EXT <- ".txt\\.bz2"
BZIP_FNAME <- ".*\\.txt\\.bz2"


importRepoFiles <- function(repos, row){
  
  url <- paste(FLOSSMOLE_REPO_BASE, "/",
               repos$code[row], "/",
               repos$year[row], "/",
               repos$year[row], "-", repos$month[row],
               collapse="", sep="")

  htmlPage = rawToChar(getBinaryURL(url, followlocation = TRUE))

  doc <- htmlParse(htmlPage, asText = TRUE)
  
  if (TRUE) { # via HTML tables
    
    tables <- getNodeSet(doc, "//table")
    
    filenames <- readHTMLTable(tables[[1]],
                               trim = TRUE,
                               stringsAsFactors = FALSE)
    
    filenames <- list(filenames[,2])
    
    filenames <- lapply(filenames,
                        function(x) grep(BZIP_EXT, x, value = TRUE))
  }
  
  
  if (FALSE) { # via XML elements
    
    links <- xpathSApply(doc, "//*/a[@class='href']", xmlValue)
    
    xpathSApply(doc, '//*[@class="href"]', xmlAttrs)

    links <- lapply(links, function(x) grep(BZIP_EXT, x, links))
  }
  
  curlHandle <- getCurlHandle()
  
  # generate list of files' FULL (absolute) URLs
  links <- lapply(filenames, function(x) paste(url, x, sep="/"))
  
  if (FALSE) {
    repoFiles = rawToChar(getBinaryURL(url, followlocation = TRUE))
  }

  getData <- function(url) {

    print("Entered getData()!")
    print(url)
    
    if (TRUE) { # via local file
      
      file <- tempfile(pattern = "tmp", tmpdir = ".", fileext = ".bz2")    
      download.file(url, destfile = file, mode = "w")
      conn <- gzcon(bzfile(file, open = "r"))
      try(read.table(conn, sep = ",", row.names = NULL), silent = TRUE)
      close(conn)
      unlink(file)
    }
    else { # via RCurl
      
      bConn <- getBinaryURL(url, followlocation = TRUE)
      bzConn <- gzcon(rawConnection(bConn, "rb"))
      tConn <- bzfile(bzConn)
      #tConn <- textConnection(readLines(bzConn))
      try(read.table(tConn, sep = ",", row.names = NULL), silent = TRUE)
      close(tConn)
    }
    
    #try(read.table(bzfile(gzcon(url(x))), sep = ",", row.names = NULL))
    #try(read.table(x, sep = ",", row.names = NULL))
  }

  if (FALSE) {
    data <- lapply(seq_along(links), function(i) {getData(links[[i]])})
  }
  else {
    data <- lapply(links, function(x) {getData(x)})
  }
}


getFLOSSmoleData <- function(repos) {
  lapply(row <- 1:nrow(repos), function(row) importRepoFiles(repos, row))
}


getFLOSSmoleData(repos)