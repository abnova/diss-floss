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
    saveRDS(result, rdataFile)
    rm(result)
  }
  else {
    error("RDS file for \'", indicator, "\' not found!")
  }
}


##### HANDLER FUNCTION DEFINITIONS #####

getProjectAge <- function (indicator, data) {
  
  # delete target column if exists
  if ("Project Age" %in% names(data))
    data[,c("Project Age")] <- list(NULL)
  
  transformColumn <- as.numeric(data[["Registration Time"]])
  regTime <- as.POSIXct(transformColumn, origin="1970-01-01")
  prjAge <- difftime(Sys.Date(), as.Date(regTime), units = "weeks")
  result <- cbind(data, round(prjAge))
  names(result)[3] <- "Project Age"
  print(head(result))
  return (result)
}


##### MAIN #####

# construct list of indicators & corresponding transform. functions
indicators <- c("prjAge")
transforms <- list(getProjectAge)


# sequentially call all previously defined transformation functions
lapply(seq_along(indicators),
       function(i) {transformResult(indicators[[i]], transforms[[i]])})
