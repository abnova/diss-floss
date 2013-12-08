if (!require(RCurl)) install.packages('RCurl')
if (!require(RJSONIO)) install.packages('RJSONIO')

library(RCurl)
library(RJSONIO)

baseURL <- "http://api.angel.co/1/tags/59/startups"

getDataPaginated<-function(page){
  url <- paste(baseURL, "?page=", page, collapse="", sep="")
  startupData <- getURL(url)
  #$list
  fromJSON(startupData)
}

#unlist(lapply...) can/should be replaced to sapply()?
# OR to rapply() for nested lists?
# OR even better use the 'plyr' package?
startups <- unlist(lapply(1:4, getDataPaginated), recursive=F)

#startupNames <- sapply(startups, function(x) x$name)
#startupURL <- sapply(startups, function(x) x$company_url)
