# Start session with a clean R environment
rm(list = ls(all.names = TRUE))

if (!suppressMessages(require(RCurl))) install.packages('RCurl')

library(RCurl)

PRJ_HOME <- Sys.getenv("DISS_FLOSS_HOME")

source(file.path(PRJ_HOME, "config/diss-floss-config.R"))


##### GENERIC TRANSFORMATION FUNCTION #####

transformResult <- function (dataSource, indicator, handler = NULL) {
  
  fileName <- paste0(indicator, RDS_EXT)
  cacheFile <- file.path(CACHE_DIR, dataSource, fileName)
  transformFile <- file.path(TRANSFORMED_DIR, dataSource, fileName)

  # single point for making syntactically valid R names
  df <- readRDS(cacheFile)
  names(df) <- make.names(names(df))
  saveRDS(df, cacheFile)

  # copy data files that doesn't require transformation
  if (is.null(handler)) {
    if (DEBUG) message("Copying data '", indicator, "' ...",
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

sfDevLinks <- function (indicator, data) {
  
  if (DEBUG) message("Transforming '", indicator, "' ...",
                     appendLF = FALSE)
  
  # convert presence of Repo URL to integer
  data[["Repo.URL"]] <- as.integer(data[["Repo.URL"]] != "")
  
  if (DEBUG) message(" Done.")
  if (DEBUG2) {message(""); print(summary(data)); message("")}
  
  return (data)
}


sfProjectAge <- function (indicator, data) {

  if (DEBUG) message("Transforming '", indicator, "' ...",
                     appendLF = FALSE)

  transformColumn <- as.numeric(unlist(data["Registration.Time"]))
  regTime <- as.POSIXct(transformColumn, origin = "1970-01-01")
  prjAge <- difftime(Sys.Date(), as.Date(regTime), units = "weeks")
  data[["Project.Age"]] <- as.numeric(round(prjAge)) / 4 # in months
  
  # now we can delete the source column
  if ("Registration.Time" %in% names(data))
    data <- data[setdiff(names(data), "Registration.Time")]  

  if (DEBUG) message(" Done.")
  if (DEBUG2) {message(""); print(summary(data)); message("")}
  
  return (data)
}


sfProjectLicense <- function (indicator, data) {
  
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
      nausite='Permissive', real='Restrictive',
      # licenses below are those expecting classification
      miroslicense='Unknown', multicslicense='Unknown', catosl='Unknown',
      ipafontlicense='Unknown', fair='Unknown', isclicense='Unknown',
      classpath='Unknown', artisticv2='Unknown',
      boostlicense='Unknown', cddl='Unknown',
      ccanclv2='Unknown', ccaslv2='Unknown', ccaslv3='Unknown',
      mpl20='Unknown', 'ms-rl'='Unknown', ntplicense='Unknown',
      entessa='Unknown', sybase='Unknown', eclipselicense='Unknown',
      nasalicense='Unknown', oclc='Unknown', rpl15='Unknown',
      frameworx='Unknown', agpl='Unknown', apache2='Unknown',
      openfontlicense11='Unknown', nposl3='Unknown', cua='Unknown',
      lgplv3='Unknown', latexppl='Unknown', eupublicense='Unknown',
      cpal='Unknown', 'ms-pl'='Unknown', splicense2='Unknown',
      gplv3='Unknown', educom='Unknown', adaptive='Unknown',
      datagrid='Unknown', public102='Unknown')
  
  data[["License.Restrictiveness"]] <- 
    factor(classification[as.character(data[["Project.License"]])],
           ordered = TRUE,
           levels = c("Highly Restrictive", "Restrictive",
                      "Permissive")) # set definite order
  
  data[["Project.License"]] <- factor(data[["Project.License"]])
  levels(data[["Project.License"]]) <- 
    list(LGPL="lgpl", BSD="bsd", GPL="gpl", Website="website",
         ZLib="zlib", Public="public", Other="other", IBMCPL="ibmcpl",
         RPL="rpl", MPL11="mpl11", MIT="mit", AFL="afl",
         Python="python", MPL="mpl", Apache="apache", OSL="osl",
         W3C="w3c", IOSL="iosl", Artistic="artistic", APSL="apsl",
         IBM="ibm", Plan9="plan9", PHP="php", QPL="qpl", PSFL="psfl",
         NCSA="ncsa", RSCPL="rscpl", SunPublic="sunpublic", Zope="zope",
         Eiffel="eiffel", Nethack="nethack", SISSL="sissl",
         Unknown="none", OpenGroup="opengroup", SleepyCat="sleepycat",
         Nokia="nokia", Attribut="attribut", XNet="xnet",
         Eiffel2="eiffel2", WxWindows="wxwindows", MotoSoto="motosoto",
         Vovida="vovida", Jabber="jabber", CVW="cvw",
         Historical="historical", Nausite="nausite", Real="real",
         # licenses below are those expecting classification
         MirOS="miroslicense", Multics="multicslicense", CAtosl="catosl",
         IPAfl="ipafontlicense", fair="fair", ISC="isclicense",
         GNUclasspath="classpath", Artistic2="artisticv2",
         Boost="boostlicense", CDDL="cddl",
         CCANC2="ccanclv2", CCASA2="ccaslv2", CCASA3="ccaslv3",
         MPL2="mpl20", MSRL="ms-rl", NTP="ntplicense",
         Entessa="entessa", Sybase="sybase", Eclipse="eclipselicense",
         NASA="nasalicense", OCLC="oclc", RPL15="rpl15",
         Frameworx="frameworx", Affero="agpl", Apache2="apache2",
         OpenFont11="openfontlicense11", NPOSL3="nposl3", CUA="cua",
         LGPL3="lgplv3", LaTeX="latexppl", EUPL="eupublicense",
         CPAL="cpal", MSPL="ms-pl", Simple2="splicense2",
         GPL3="gplv3", EDUCOM="educom", Adaptive="adaptive",
         EUDataGrid="datagrid", Lucent102="public102")
  
  data[["License.Category"]] <- factor(data[["License.Category"]])
  levels(data[["License.Category"]]) <- 
    list(OSI="osi", Other="license", CCAL="ccal")
  
  classification <- c(OSI="OSI", Other="Non-OSI", CCAL="Non-OSI")
  data[["License.Category"]] <- 
    factor(classification[as.character(data[["License.Category"]])],
           ordered = TRUE,
           levels = c("OSI", "Non-OSI")) # set definite order
  
  if (DEBUG) message(" Done.")
  if (DEBUG2) {message(""); print(summary(data)); message("")}
  
  return (data)
}


sfPrjMaturity <- function (indicator, data) {
  
  if (DEBUG) message("Transforming '", indicator, "' ...",
                     appendLF = FALSE)
  
  classification <- 
    c(planning='Alpha/Beta', prealpha='Alpha/Beta', alpha='Alpha/Beta',
      beta='Alpha/Beta', production='Stable', mature='Mature',
      inactive='Inactive')
  
  data[["Project.Stage"]] <- 
    factor(classification[as.character(data[["Development.Stage"]])],
           ordered = TRUE,
           levels = c("Alpha/Beta", "Stable", "Mature", "Inactive"))
  
  data[["Development.Stage"]] <- factor(data[["Development.Stage"]])
  levels(data[["Development.Stage"]]) <- 
    list(Planning="planning", Pre.Alpha="prealpha", Alpha="alpha",
         Beta="beta", Production="production", Mature="mature",
         Inactive="inactive")
  
  if (DEBUG) message(" Done.")
  if (DEBUG2) {message(""); print(summary(data)); message("")}
  
  return (data)
}


sfDevTeamSize <- function (indicator, data) {
  
  if (DEBUG) message("Transforming '", indicator, "' ...",
                     appendLF = FALSE)
  
  var <- data[["Development.Team.Size"]]
  
  # convert data type from 'character' to 'numeric'
  data[["Development.Team.Size"]] <- as.numeric(var)
  
  if (DEBUG) message(" Done.")
  if (DEBUG2) {message(""); print(summary(data)); message("")}
  
  return (data)
}


sfUserCommunitySize <- function (indicator, data) {
  
  if (DEBUG) message("Transforming '", indicator, "' ...",
                     appendLF = FALSE)

  # convert User Community Size from character to integer
  data[["User.Community.Size"]] <- 
    as.integer(data[["User.Community.Size"]])
  
  if (DEBUG) message(" Done.")
  if (DEBUG2) {message(""); print(summary(data)); message("")}
  
  return (data)
}


sfDevSupport <- function (indicator, data) {
  
  if (DEBUG) message("Transforming '", indicator, "' ...",
                     appendLF = FALSE)
  
  # convert data type from character to integer
  # suppress "NAs introduced by coercion" warnings
  data[["Preferred.Support.Type"]] <- 
    suppressWarnings(as.integer(data[["Preferred.Support.Type"]]))
  
  # recode Preferred Support Type: 6 to 1, other (1) to 0
  data[["Preferred.Support.Type"]] <- 
    ifelse(data[["Preferred.Support.Type"]] == 6, 1, 0)
  
  # recode Preferred Support Resource: empty to 0, non-empty to 1
  # contains a workaround due to unknown nature of received data

  data[["Preferred.Support.Resource"]] <- 
    ifelse(nchar(data[["Preferred.Support.Resource"]]) == 0, 0, 1)
  
  data[["Preferred.Support.Resource"]] <- 
    suppressWarnings(as.integer(data[["Preferred.Support.Resource"]]))
  
  # convert to factors
  data[["Preferred.Support.Type"]] <- 
    factor(data[["Preferred.Support.Type"]], ordered = TRUE)
  
  data[["Preferred.Support.Resource"]] <- 
    factor(data[["Preferred.Support.Resource"]], ordered = TRUE)
  
  if (DEBUG) message(" Done.")
  if (DEBUG2) {message(""); print(summary(data)); message("")}
  
  return (data)
}


sfSoftwareType <- function (indicator, data) {
  
  if (DEBUG) message("Transforming '", indicator, "' ...",
                     appendLF = FALSE)
  
  data[["Software.Type"]] <- factor(data[["Software.Type"]],
                                    ordered = TRUE)
  levels(data[["Software.Type"]]) <- 
    list(General="Desktop", General="Games",
         General="VoIP", General="Multimedia",
         Special="Enterprise", Specialized="Financial",
         System="Clustering", System="Databases", System="Hardware",
         System="Networking", System="Security", System="Storage",
         System="SysAdmin",
         DevTools="Development")
  
  if (DEBUG) message(" Done.")
  if (DEBUG2) {message(""); print(summary(data)); message("")}
  
  return (data)
}


##### FLOSSmole transformation functions #####

fmLaunchPad <- function (indicator, data) {
  
  # rename 'name' column in select tables to match
  # corresponding column in the other LaunchPad tables
  if (indicator == 'lpdLicenses' ||
        indicator == 'lpdOfficialBugTags' ||
        indicator == 'lpdProgrammingLanguages' ||
        indicator == 'lpdProjects') {
    
    if (DEBUG) message("Transforming '", indicator, "' ...",
                       appendLF = FALSE)
    
    colnames(data)[colnames(data) == 'name'] <- 'project_name'
  }
  else return (NULL)
  
  if (DEBUG) message(" Done.")
  if (DEBUG2) {message(""); print(summary(data)); message("")}
  
  return (data)
}


##### MAIN #####

# ===== Begin Transformation Initialization =====

# construct list of indicators & corresponding transform. functions
indicators <- c(); transforms <- list()

# ===== SourceForge segment initialization =====

indicators[["SourceForge"]] <- c(#"prjAge",
                                 "prjLicense",
                                 "prjMaturity",
                                 "devLinks",
                                 "devTeamSize",
                                 "userCommunitySize",
                                 "devSupport",
                                 "softwareType",
                                 "pubRoadmap",
                                 "dmProcess")

transforms[["SourceForge"]] <- list(#sfProjectAge,
                                    sfProjectLicense,
                                    sfPrjMaturity,
                                    sfDevLinks,
                                    sfDevTeamSize,
                                    sfUserCommunitySize,
                                    sfDevSupport,
                                    sfSoftwareType,
                                    NULL,
                                    NULL)

# ===== FLOSSmole segment initialization =====

# initialize FLOSSmole indicators with repos information file names
fNames <- strsplit(dir(file.path(CACHE_DIR, "FLOSSmole")), ".rds")
indicators[["FLOSSmole"]] <- unlist(fNames)

# initially set all transformation function entries to NULL
transforms[["FLOSSmole"]] <- 
  replicate(length(fNames), NULL, simplify = FALSE)

# re-initialize the list with actual transformation functions
#transforms[["FLOSSmole"]][[1]] <- fmLaunchPad
# TODO: consider wrapping this in a generic repo-based function
for (i in seq(indicators[["FLOSSmole"]])) {
  if (grepl('^lpd', indicators[["FLOSSmole"]][[i]]))
    transforms[["FLOSSmole"]][[i]] <- fmLaunchPad
}

# set data sources
dataSourcesList <- c("SourceForge", "FLOSSmole")

# ===== End Transformation Initialization =====

if (DEBUG) message("===== Data Transformation started\n")

for (dataSource in dataSourcesList) {
  
  if (DEBUG) message("Transforming ", dataSource, " data:")
  if (DEBUG) message("=============",
                     rep.int('=', nchar(dataSource)), "======\n")
  
  # TBD here - transform result data types as specified in config.
  
  transformDir <- file.path(TRANSFORMED_DIR, dataSource)
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