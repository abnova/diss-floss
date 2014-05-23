if (!suppressMessages(require(RCurl))) install.packages('RCurl')


RDATA_DIR <- "../cache/SourceForge"
RDS_EXT <- ".rds"


##### GENERIC TRANSFORMATION FUNCTION #####

transformResult <- function (indicator, handler) {
  
  fileDigest <- base64(indicator)
  rdataFile <- paste0(RDATA_DIR, "/", fileDigest, RDS_EXT)
  if (file.exists(rdataFile)) {
    data <- readRDS(rdataFile)
    result <- do.call(handler, list(indicator, data))
  }
  else {
    error("RDS file for \'", indicator, "\' not found!")
  }
  
  return (result)
}


##### HANDLER FUNCTION DEFINITIONS #####

getProjectAge <- function (indicator, data) {
  
  print(head(data))
}


##### MAIN #####

# construct list of indicators & corresponding transform. functions
indicators <- c("prjAge")
transforms <- list(getProjectAge)


# sequentially call all previously defined transformation functions
lapply(seq_along(indicators),
       function(i) {transformResult(indicators[[i]], transforms[[i]])})
