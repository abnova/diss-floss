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


importRepoFiles <- function(repos, row){
  
  print(paste("importRepoFiles -", row))
  
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
    
    # current method
    if (TRUE) { # via local file
      
      file <- tempfile(pattern = "tmp", tmpdir = ".", fileext = ".bz2")    
      download.file(url, destfile = file, mode = "w")
      conn <- gzcon(bzfile(file, open = "r"))
      #tConn <- textConnection(readLines(conn))
      try(fileData <- read.table(conn, sep = ",", row.names = NULL),
          silent = FALSE)
      #head(fileData)
      close(conn)
      #close(tConn)
      unlink(file)
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


getFLOSSmoleData(repos)