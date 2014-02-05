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

repo <- data.frame(code = REPO_CODE, year = REPO_YEAR, month = REPO_MONTH,
                   stringsAsFactors=FALSE)

BZIP_EXT <- ".txt\\.bz2"
BZIP_FNAME <- ".*\\.txt\\.bz2"


importRepoFiles <- function(row){
  
  url <- paste(FLOSSMOLE_REPO_BASE, "/",
               repo$code[row], "/",
               repo$year[row], "/",
               repo$year[row], "-", repo$month[row],
               collapse="", sep="")

  # Moved the next line to both places (via HTML tables & XML elements)
  #htmlPage <- getURL(url, followlocation = TRUE)
  
  if (TRUE) { # via HTML tables
    
    #htmlPage = rawToChar(getBinaryURL(url, followlocation = TRUE))
    htmlPage = rawToChar(getURLContent(url, followlocation = TRUE,
                                       binary = TRUE))
    
    #doc <- htmlParse(htmlPage, asText = TRUE)
    doc <- htmlTreeParse(htmlPage, useInternalNodes = TRUE, asText=TRUE)
    
    tables <- getNodeSet(doc, "//table")
    
    filenames <- readHTMLTable(tables[[1]],
                               trim = TRUE,
                               stringsAsFactors = FALSE)
    
    filenames <- list(filenames[,2])
    
    filenames <- lapply(filenames,
                        function(x) grep(BZIP_EXT, x, value = TRUE))
  }
  
  if (FALSE) { # via XML elements
    
    htmlPage <- getURL(url, followlocation = TRUE)
    
    doc <- htmlParse(htmlPage)
    
    links <- xpathSApply(doc, "//*/a[@class='href']", xmlValue)
    print(doc)
    print(links)
    
    xpathSApply(doc, '//*[@class="href"]', xmlAttrs)

    links <- lapply(links, function(x) grep(BZIP_EXT, x, links))
  }
  
  curlHandle <- getCurlHandle()
  
  # 'links' is a list of files' FULL URLs
  links <- lapply(filenames, function(x) paste(url, x, sep="/"))
  print(links)
  
  repoFiles <- lapply(links,
                      function(url) try(getURL(url,
                                               curl = curlHandle,
                                               followlocation = TRUE)))
  print(repoFiles)

  data <- lapply(links,
                 function(url) try(read.table(bzfile(links),
                                              #allowEscapes=TRUE,
                                              #header=TRUE,
                                              #encoding="latin1",
                                              sep=",", row.names=NULL)))
}


getFLOSSmoleData <- function() {
  #by(repo, 1:nrow(repo), function(row) importRepoFiles)
  #lapply(repo, function(row) importRepoFiles(1:nrow(repo)))
  lapply(row <- 1:nrow(repo), function(row) importRepoFiles(row))
}


getFLOSSmoleData()