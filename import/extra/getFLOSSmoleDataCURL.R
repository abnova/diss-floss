#' Module: getFLOSSmoleData.R
#'
#' Downloads FLOSS projects data from FLOSSmole repository
#' (in CSV format) and converts it to data frame format.
#'
#' @author Aleksandr Blekh \email{blekh@@nova.edu}

if (!require(RCurl)) install.packages('RCurl')
#if (!require(RODBC)) install.packages('RODBC')

library(RCurl)
#library(RODBC)


# URL of FLOSSmole repository root directory
FLOSSMOLE_REPO_BASE <- "http://flossdata.syr.edu/data"

# Create a lookup table to construct correct paths to files
REPO_CODE   <- c("fc",   "fsf",  "gc",   "gh",   "lpd",  "sv",   "tig")
REPO_YEAR  <- c("2013", "2012", "2012", "2013", "2012", "2013", "2013")
REPO_MONTH <- c("Dec",  "Nov",  "Nov",  "Feb",  "Sep",  "Dec",  "Dec")

repo <- data.frame(code = REPO_CODE, year = REPO_YEAR, month = REPO_MONTH,
                   stringsAsFactors=FALSE)


importRepoFiles <- function(row){
  
  url <- paste(FLOSSMOLE_REPO_BASE, "/",
               repo$code[row], "/",
               repo$year[row], "/",
               repo$year[row], "-", repo$month[row],
               collapse="", sep="")

  print(url)
  
  filesPage <- try(getURL(url, dirlistonly = TRUE, followlocation = TRUE))
  print(filesPage)
  filenames <- grep("href=\\*txt.bz2", filesPage, value = TRUE)
  print(filenames)
  #for (line in filenames) {
  #  urlStart <- grep("href\? = txt.bz2", line)
  #  urlEnd <- grep("txt.bz2", line)
  #  filenames <- c()
    #grep("\\.txt.bz2$", line, value = TRUE)
  #}
  
  curlHandle <- getCurlHandle()
  opts = curlOptions(wildcardmatch = "\\.txt.bz2$")  
  #curlSetOpt(.opts <- list(wildcardmatch = "\\.txt.bz2$"), curl = curlHandle)  
  
  repoFiles <- list()
  repoFiles <- lapply(filenames,
                      function(url) try(getURL(url,
                                               .opts = opts,
                                               curl = curlHandle)))
  
  for (file in repoFiles) {
    #file <- file(url)
    data <- read.table(bzfile(file), header=TRUE, sep=",", row.names=NULL)
  }
}


getFLOSSmoleData <- function() {
  #by(repo, 1:nrow(repo), function(row) importRepoFiles)
  #lapply(repo, function(row) importRepoFiles(1:nrow(repo)))
  lapply(row <- 1:nrow(repo), function(row) importRepoFiles(row))
}


getFLOSSmoleData()