if (!suppressMessages(require(RCurl))) install.packages('RCurl')

library(RCurl)

CACHE_DIR <- "../cache"
TRANSFORM_DIR <- "../data/transform"
RDS_EXT <- ".rds"

DEBUG <- TRUE  # TODO: retrieve debug flag via CL arguments
DEBUG2 <- TRUE # output more detailed debug information


##### GENERIC TRANSFORMATION FUNCTION #####

transformResult <- function (dataSource, indicator, handler = NULL) {
  
  fileDigest <- base64(indicator)
  fileName <- paste0(fileDigest, RDS_EXT)
  cacheFile <- file.path(CACHE_DIR, dataSource, fileName)
  transformFile <- file.path(TRANSFORM_DIR, dataSource, fileName)
  
  if (is.null(handler)) {
    if (DEBUG) message("Copying data for '", indicator, "' ...",
                       appendLF = FALSE)
    copyCommand <- paste("cp", cacheFile, transformFile)
    try(system(copyCommand))
    if (DEBUG) message(" Done.")
    return()
  }

  if (file.exists(cacheFile)) {
    data <- readRDS(cacheFile)
    
    # Preserve user-defined attributes for data frame's columns
    # via defining new class 'avector' (see code below)). Also,
    # preserve attributes (comments) for the data frame itself.
    data2 <- data.frame(lapply(data, function(x) 
      { structure(x, class = c("avector", class(x))) } ))
    #mostattributes(data2) <- attributes(data)
    attributes(data2) <- attributes(data)
    
    result <- do.call(handler, list(indicator, data2))
    if (!is.null(result)) saveRDS(result, transformFile)
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

  if (DEBUG) message("Transforming '", indicator, "' ...",
                     appendLF = FALSE)

  transformColumn <- as.numeric(unlist(data["Registration Time"]))
  regTime <- as.POSIXct(transformColumn, origin="1970-01-01")
  prjAge <- difftime(Sys.Date(), as.Date(regTime), units = "weeks")
  data[["Project Age"]] <- as.numeric(round(prjAge)) / 4 # in months
  
  # now we can delete the source column
  if ("Registration Time" %in% names(data))
    data <- data[setdiff(names(data), "Registration Time")]  

  if (DEBUG) message(" Done.")
  if (DEBUG2) {message(""); print(summary(data)); message("")}
  
  return (data)
}


projectLicense <- function (indicator, data) {
  
  if (DEBUG) message("Transforming '", indicator, "' ...",
                     appendLF = FALSE)
  
  classification <- 
    c(lgpl='Restrictive', bsd='Permissive', gpl='Highly Restrictive',
      website='Unknown', zlib='Permissive', public='Permissive',
      other='Unknown', ibmcpl='Restrictive', rpl='Restrictive',
      mpl11='Restrictive', mit='Permissive', afl='Permissive',
      python='Permissive', mpl='Restrictive', apache='Permissive',
      osl='Permissive', w3c='Permissive', iosl='Permissive',
      artistic='Permissive', apsl='Restrictive', ibm='Restrictive',
      plan9='Restrictive', php='Restrictive', qpl='Restrictive',
      psfl='Permissive', ncsa='Permissive', rscpl='Restrictive',
      sunpublic='Restrictive', zope='Permissive', eiffel='Restrictive',
      nethack='Restrictive', sissl='Permissive', none='Unknown',
      opengroup='Permissive', sleepycat='Restrictive', nokia='Restrictive',
      attribut='Restrictive', xnet='Permissive', eiffel2='Restrictive',
      wxwindows='Restrictive', motosoto='Restrictive', vovida='Permissive',
      jabber='Restrictive', cvw='Restrictive', historical='Unknown',
      nausite='Permissive', real='Restrictive')
  
  data[["Project License"]] <- factor(data[["Project License"]])
  
  data[["License Restrictiveness"]] <- 
    as.factor(classification[as.character(data[["Project License"]])])

  if (DEBUG) message(" Done.")
  if (DEBUG2) {message(""); print(summary(data)); message("")}
  
  return (data)
}


prjMaturity <- function (indicator, data) {
  
  if (DEBUG) message("Transforming '", indicator, "' ...",
                     appendLF = FALSE)
  
  var <- data[["Latest Release"]]
  
  rx <- "^([^[:digit:]]*)([[:digit:]]+)(\\.|-)+(.*)$"
  major <- gsub(rx, "\\2", var)
  # suppress "NAs introduced by coercion" warnings
  major <- suppressWarnings(as.numeric(major))

  data[["Project Maturity"]] <- 
    cut(major, breaks = c(0, 1, 2, Inf), include.lowest = TRUE,
        right = FALSE, labels=c("Alpha/Beta", "Stable", "Mature"))

  if (DEBUG) message(" Done.")
  if (DEBUG2) {message(""); print(summary(data)); message("")}
  
  return (data)
}


devTeamSize <- function (indicator, data) {
  
  if (DEBUG) message("Transforming '", indicator, "' ...",
                     appendLF = FALSE)

  var <- data[["Development Team Size"]]
  
  # convert data type from 'character' to 'numeric'
  data[["Development Team Size"]] <- as.numeric(var)

  if (DEBUG) message(" Done.")
  if (DEBUG2) {message(""); print(summary(data)); message("")}
  
  return (data)
}


##### MAIN #####

# construct list of indicators & corresponding transform. functions
indicators <- c(); transforms <- list()

indicators[["SourceForge"]] <- c("prjAge", "prjLicense",
                                 "devTeamSize", "prjMaturity",
                                 "devLinks", "devSupport",
                                 "pubRoadmap", "dmProcess",
                                 "contribPeople", "userCommunitySize",
                                 "softwareType")

transforms[["SourceForge"]] <- list(projectAge, projectLicense,
                                    devTeamSize, prjMaturity,
                                    NULL, NULL, NULL, NULL,
                                    NULL, NULL, NULL)

indicators[["FLOSSmole"]] <- c()

transforms[["FLOSSmole"]] <- list()

dataSourcesList <- c("SourceForge", "FLOSSmole")

if (DEBUG) message("===== Data Transformation started\n")

for (dataSource in dataSourcesList) {
  
  if (DEBUG) message("Transforming ", dataSource, " data:\n")
  
  # TBD here - transform result data types as specified in config.
  
  transformDir <- file.path(TRANSFORM_DIR, dataSource)
  if (!file.exists(transformDir))
    dir.create(transformDir, recursive = TRUE)
  
  # sequentially call all previously defined transformation functions
  silent <- lapply(seq_along(indicators[[dataSource]]),
                   function(i) {
                     transformResult(dataSource,
                                     indicators[[dataSource]][[i]],
                                     transforms[[dataSource]][[i]])
                   })
  message("")
}

if (DEBUG) message("===== Data Transformation sucessfully completed.\n")