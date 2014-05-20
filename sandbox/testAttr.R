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
  assign(dataName, readRDS(rdataFile))
  message("Retrieved object '", dataName, "', containing:\n")
  message(str(get(dataName)))
  
  requestAttrib <- attr(get(dataName), ATTR, exact = TRUE)
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
}

if (save) {
  message("Saving results of request \"",
          request, "\" as R data object ...\n")
  
  assign(dataName, getData())
  cat(str(dataName))
  data <- get(dataName)
  # alternative to using get(), but more cumbersome:
  # data <- eval(parse(text=dataName))
  
  # save hash of the request's SQL query as data object's attribute,
  # so that we can detect when configuration contains modified query
  attr(data, ATTR) <- base64(request)
  
  cat(str(data))

  # save current data frame to RDS file
  saveRDS(data, rdataFile)
  print("")
}
