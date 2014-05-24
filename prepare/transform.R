if (!suppressMessages(require(RCurl))) install.packages('RCurl')


RDATA_DIR <- "../cache/SourceForge"
RDS_EXT <- ".rds"

DEBUG <- TRUE # TODO: retrieve debug flag via CL arguments


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
    error("RDS file for \'", indicator, "\' not found! Run 'make' first.")
  }
}


## Preserve object's special attributes:
## use a class with a "as.data.frame" and "[" method

#as.data.frame.avector <- as.data.frame.vector

#`[.avector` <- function (x, i, ...) {
#  r <- NextMethod("[")
#  mostattributes(r) <- attributes(x)
#  return (r)
#}


##### HANDLER FUNCTION DEFINITIONS #####

getProjectAge <- function (indicator, data) {

  # do not process, if target column already exists
  if ("Project Age" %in% names(data)) {
    stop("\nNot processing - Transformation already performed!\n")
  }
  
  # save object's attributes
  ##attrs <- attributes(data)

  transformColumn <- as.numeric(unlist(data["Registration Time"]))
  regTime <- as.POSIXct(transformColumn, origin="1970-01-01")
  prjAge <- difftime(Sys.Date(), as.Date(regTime), units = "weeks")
  data[["Project Age"]] <- as.numeric(round(prjAge)) / 4 # in months
  #result <- cbind(data, as.numeric(round(prjAge)))
  #names(result)[3] <- "Project Age"
  
  # now we can delete the source column 
  if ("Registration Time" %in% names(data))
    data[,c("Registration Time")] <- list(NULL)
  
  if (DEBUG) print(summary(data))
  
  ##attributes(data) <- attrs
  
  return (data)
}


##### MAIN #####

# construct list of indicators & corresponding transform. functions
indicators <- c("prjAge")
transforms <- list(getProjectAge)


# sequentially call all previously defined transformation functions
lapply(seq_along(indicators),
       function(i) {transformResult(indicators[[i]], transforms[[i]])})
