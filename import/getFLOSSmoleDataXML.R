#' Module: getFLOSSmoleData.R
#'
#' Downloads FLOSS projects data from FLOSSmole repository
#' (in CSV format) and converts it to data frame format.
#'
#' @author Aleksandr Blekh \email{blekh@@nova.edu}

if (!require(RCurl)) install.packages('RCurl')
if (!require(XML)) install.packages('XML')
#if (!require(RODBC)) install.packages('RODBC')

library(RCurl)
library(XML)
#library(RODBC)


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
  
  # This is unneeded, since link to each file is an absolute URL
  # Remove, if/when made sure that correct link is being processed
  url <- paste(FLOSSMOLE_REPO_BASE, "/",
               repo$code[row], "/",
               repo$year[row], "/",
               repo$year[row], "-", repo$month[row],
               collapse="", sep="")

  htmlPage <- getURL(url, followlocation = TRUE)
  
  if (TRUE) { # via HTML tables
    doc <- htmlParse(htmlPage)
    tables <- getNodeSet(doc, "//table")
    filenames <- readHTMLTable(tables[[1]], skip.rows = 7,
                               trim = TRUE,
                               stringsAsFactors = FALSE)
    
    filenames <- list(filenames[,2])
    
    filenames <- lapply(filenames,
                        function(x) grep(BZIP_EXT, x, value = TRUE))
  }
  
  if (FALSE) { # via XML elements
    doc <- htmlParse(htmlPage)
    #links = getNodeSet(doc, "//table[@class='a href']")
    ##links = getNodeSet(doc, "//a/@href")
    ##sapply(links, xmlGetAttr, "href")
    ###links <- xpathSApply(doc, "//link[@rel='alternate']", xmlAttrs)
    
    #links <- xpathSApply(doc, "//a/@href")
    links <- xpathSApply(doc, "//*/a[@class='href']", xmlValue)
    print(doc)
    
    #links <- grepl(".txt.bz2", links)
    print(links)
    xpathSApply(doc, '//*[@class="href"]', xmlAttrs)

    links <- lapply(links, function(x) grep(".txt.bz2", x, links))
  }
  
  curlHandle <- getCurlHandle()
  #opts = curlOptions(wildcardmatch = "\\.txt.bz2$")  
  #curlSetOpt(.opts <- list(wildcardmatch = "\\.txt.bz2$"), curl = curlHandle)  
  
  # 'links' is a list of files' FULL URLs
  links <- lapply(filenames, function(x) paste(url, x, sep="/"))
  print(links)
  
  repoFiles <- lapply(links,
                      function(url) try(getURL(url,
                                               curl = curlHandle,
                                               followlocation = TRUE)))
  print(repoFiles)

  # Tried to call URLdecode(links), but still produces two errors:
  # "Error in curlMultiPerform(multiHandle) : embedded nul in string:"
  # "Error in bzfile(links) : invalid 'description' argument"
  data <- lapply(links,
                 function(url) try(read.table(bzfile(links),
                                              header=TRUE, sep=",", row.names=NULL)))
  
  #for (file in repoFiles) {
    #data <- read.table(bzfile(file, open = "r"),
    #                   header=TRUE, sep=",", row.names=NULL)
  #}
}


getFLOSSmoleData <- function() {
  #by(repo, 1:nrow(repo), function(row) importRepoFiles)
  #lapply(repo, function(row) importRepoFiles(1:nrow(repo)))
  lapply(row <- 1:nrow(repo), function(row) importRepoFiles(row))
}


getFLOSSmoleData()