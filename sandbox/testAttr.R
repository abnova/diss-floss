library(RCurl)

info <- "Important data"
ATTR <- "SQL"
request <- "SELECT info FROM topSecret"
dataName <- "sf.data.devLinks"
rdataFile <- "/tmp/testAttr.rds"

save <- TRUE

getData <- function() {
  return (info)
}

requestDigest <- base64(request)

# check if the archive file has already been processed
message("\nProcessing request \"", request, "\" ...\n")

# read back the object with the attribute
if (file.exists(rdataFile)) {
  # now check if request's SQL query hasn't been modified
  #data <- readRDS(rdataFile)
  assign(dataName, readRDS(rdataFile))
  message("Retrieved object '", dataName, "', containing:\n")
  message(str(get(dataName)))
  
  requestAttrib <- attr(data, ATTR, exact = TRUE)
  if (is.null(requestAttrib)) {
    message("Object '", dataName, "' doesn't have attribute \"",
            ATTR, "\"\n")
  }
  else {
    message("Object '", dataName, "' contains attribute \"",
            ATTR, "\":\n\"", base64(requestAttrib), "\"\n")

    if (identical(requestDigest, requestAttrib)) {
      message("Processing skipped: RDS file is up-to-date.\n")
      save <- FALSE
      return
    }
  }
  rm(data)
}

if (save) {
  message("Saving results of request \"",
          request, "\" as R data object ...\n")
  
  assign(dataName, getData())
  print(dataName)
  #data <- as.name(dataName)
  #data <- eval(parse(text=dataName))
  data <- get(dataName)
  print(data)
  #names(data) <- dataName
  print(str(data))
  #eval(substitute(assign(dataName, getData()),
  #                list(data <- as.name(dataName))))
  
  # save hash of the request's SQL query as data object's attribute,
  # so that we can detect when configuration contains modified query
  attr(data, ATTR) <- base64(request)
  
  # save current data frame to RDS file
  saveRDS(data, rdataFile)
}
