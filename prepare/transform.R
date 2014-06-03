if (!suppressMessages(require(RCurl))) install.packages('RCurl')


CACHE_DIR <- "../cache"
RDS_EXT <- ".rds"

DEBUG <- TRUE  # TODO: retrieve debug flag via CL arguments
DEBUG2 <- TRUE # output more detailed debug information


##### GENERIC TRANSFORMATION FUNCTION #####

transformResult <- function (dataSource, indicator, handler) {
  
  fileDigest <- base64(indicator)
  rdataFile <- paste0(CACHE_DIR, "/", dataSource, "/",
                      fileDigest, RDS_EXT)
  if (file.exists(rdataFile)) {
    data <- readRDS(rdataFile)
    
    # Preserve user-defined attributes for data frame's columns
    # via defining new class 'avector' (see code below)). Also,
    # preserve attributes (comments) for the data frame itself.
    data2 <- data.frame(lapply(data, function(x) 
      { structure(x, class = c("avector", class(x))) } ))
    #mostattributes(data2) <- attributes(data)
    attributes(data2) <- attributes(data)
    
    result <- do.call(handler, list(indicator, data2))
    if (!is.null(result)) saveRDS(result, rdataFile)
    rm(result)
  }
  else {
    error("RDS file for \'", indicator, "\' not found! Run 'make' first.")
  }
}


## Preserve object's special attributes:
## use a class with a "as.data.frame" and "[" method

as.data.frame.avector <- as.data.frame.vector

`[.avector` <- function (x, ...) {
  #attr <- attributes(x)
  r <- NextMethod("[")
  mostattributes(r) <- attributes(x)
  #attributes(r) <- attr
  return (r)
}


##### HANDLER FUNCTION DEFINITIONS #####

#dataTypeTransform <- function (indicator, data) {}


projectAge <- function (indicator, data) {

  # do not process, if target column already exists
  if ("Project Age" %in% names(data)) {
    message("Project Age: ", appendLF = FALSE)
    message("Not processing - Transformation already performed!\n")
    return (invisible())
  }
  
  # the next line doesn't keep attributes:
  transformColumn <- as.numeric(unlist(data["Registration Time"]))
  
  # but the following line does:
  #mode(data[["Registration Time"]]) <- 'numeric'
  #storage.mode(unlist(data["Registration Time"])) <- 'numeric'
  #transformColumn <- data[["Registration Time"]]
  
  regTime <- as.POSIXct(transformColumn, origin="1970-01-01")
  prjAge <- difftime(Sys.Date(), as.Date(regTime), units = "weeks")
  data[["Project Age"]] <- as.numeric(round(prjAge)) / 4 # in months
  #result <- cbind(data, as.numeric(round(prjAge)))
  #names(result)[3] <- "Project Age"
  
  # now we can delete the source column
  if ("Registration Time" %in% names(data))
    data <- data[setdiff(names(data), "Registration Time")]  
    #data[,c("Registration Time")] <- list(NULL)

    if (DEBUG2) {print(summary(data)); message("")}
  
  return (data)
}


projectLicense <- function (indicator, data) {
  
  # do not process, if target column (type) already exists
  if (is.factor(data[["Project License"]])) {
    message("Project License: ", appendLF = FALSE)
    message("Not processing - Transformation already performed!\n")
    return (invisible())
  }
  
  data[["Project License"]] <- 
    factor(data[["Project License"]],
           levels = c('gpl', 'lgpl', 'bsd', 'other',
                      'artistic', 'public', '(Other)'),
           labels = c('GPL', 'LGPL', 'BSD', 'Other',
                      'Artistic', 'Public', 'Unknown'))

  if (DEBUG2) {print(summary(data)); message("")}
  
  return (data)
}


devTeamSize <- function (indicator, data) {
  
  var <- data[["Development Team Size"]]
  
  # do not process, if target column (type) already exists
  if (is.numeric(var)) {
    message("Development Team Size: ", appendLF = FALSE)
    message("Not processing - Transformation already performed!\n")
    return (invisible())
  }
  
  # convert data type from 'character' to 'numeric'
  data[["Development Team Size"]] <- as.numeric(var)
  
  if (DEBUG2) {print(summary(data)); message("")}
  
  return (data)
}


##### MAIN #####

# construct list of indicators & corresponding transform. functions
indicators <- c("prjAge", "prjLicense", "devTeamSize")
transforms <- list(projectAge, projectLicense, devTeamSize)

# transform result data types as specified in configuration 
#lapply(seq_along(indicators),
#       function(i) {
#         transformResult("SourceForge",
#                         indicators[[i]], dataTypeTransform)
#       })

# sequentially call all previously defined transformation functions
silent <- lapply(seq_along(indicators),
                 function(i) {
                   transformResult("SourceForge",
                                   indicators[[i]], transforms[[i]])
                   })
