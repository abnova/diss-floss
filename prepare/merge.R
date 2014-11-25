# into a single data frame for further analysis

# Start session with a clean R environment
rm(list = ls(all.names = TRUE))

if (!suppressMessages(require(psych))) install.packages('psych')
library(psych)

PRJ_HOME <- Sys.getenv("DISS_FLOSS_HOME")

# prevents "Error: invalid multibyte string at '<b5>Backu'" & similar
invisible(Sys.setlocale('LC_ALL', 'C'))

source(file.path(PRJ_HOME, "config/diss-floss-config.R"))
source(file.path(PRJ_HOME, "utils/data.R"))

# SRDA internal codes 
PRJ_INACTIVE_ <- 21

# default values for limits, etc.
TEAM_SIZE <- 100

commonColumn <- data.frame(
  prefix  = c("fc",         "fsf",      "gc",        "lpd",
              "sv",         "tig"),
  mergeBy = c("project_id", "proj_num", "proj_name", "project_name",
              "",           ""))


mergeDataSets <- function (datasets, prefix = "",
                           method = "plyr") {

  flossData <<- data.frame()
  
  if (prefix == "")
    mergeBy <- "Project.ID"
  else {
    # lookup a column to merge data by
    index <- match(prefix , commonColumn$prefix)
    mergeBy <- as.character(commonColumn[index,]$mergeBy)
  }
  
  lapplyMerge <- function (dataSets) {
    
    flossData <<- data.frame(dataSets[[1]][1])
    
    # merge all loaded datasets by common column ("Project.ID")
    silent <- lapply(seq(2, length(dataSets)),
                     function(i) {merge(flossData, dataSets[[1]][i],
                                        by = "Project.ID",
                                        all = TRUE)})
  }
  
  
  lapplyMerge2 <- function (dataSets) {
    
    pids <- which(sapply(dataSets,
                         FUN=function(x) {'Project.ID' %in% names(x)}))
    
    flossData <<- dataSets[[pids[1]]]
    
    for (id in pids[2:length(pids)]) {
      flossData <- merge(flossData, dataSets[[id]],
                         by='Project.ID', all = TRUE)
    }
  }
  
  
  reduceMerge <- function (dataSets) {
    
    flossData <<- Reduce(function(...) 
      merge(..., by.x = "row.names", by.y = "Project.ID", all = TRUE),
      dataSets)
  }
  
  
  # http://r.789695.n4.nabble.com/merge-multiple-data-frames-tt4331089.html#a4333772
  reduceMerge <- function (dataSets) {
    
    mergeAll <- function(..., by = "Project.ID", all = TRUE) {
      dotArgs <- list(...)
      dotNames <- lapply(dotArgs, names)
      repNames <- Reduce(intersect, dotNames)
      repNames <- repNames[repNames != by]
      for(i in seq_along(dotArgs)){
        wn <- which( (names(dotArgs[[i]]) %in% repNames) &
                       (names(dotArgs[[i]]) != by))
        names(dotArgs[[i]])[wn] <- paste(names(dotArgs[[i]])[wn],
                                         names(dotArgs)[[i]], sep = ".")
      }
      Reduce(function(x, y) merge(x, y, by = by, all = all), dotArgs)
    }
    
    flossData <<- mergeAll(dataSets)
  }
  
  
  reshapeMerge <- function (dataSets) {
    
    if (!suppressMessages(require(reshape))) install.packages('reshape')
    library(reshape)
    flossData <<- reshape::merge_all(dataSets)
  }
  
  
  plyrMerge <- function (dataSets) {
    
    if (!suppressMessages(require(plyr))) install.packages('plyr')
    library(plyr)
    
    # We could use join_all() here to recursively list of data frames,
    # but because this functionality exists in 'plyr' since ver. 1.8,
    # we opt for manual merge solution for wider platform support.
    
    flossData <<- dataSets[[1]]
    
    # check if we're dealing with a single data set
    if (length(dataSets) == 1) return
    else
    for (i in seq.int(2, length(dataSets), 1)) {
      flossData <<- plyr::join(flossData, dataSets[[i]],
                               by = mergeBy,
                               type = 'full', match = 'first')
    }
  }
  
  
  dplyrMerge <- function (dataSets) {
    
    if (!suppressMessages(require(dplyr))) install.packages('dplyr')
    library(dplyr)
    
    flossData <<- dataSets[[1]][1]
    flossData <<- lapply(dataSets[[1]][-1],
                         function(x) {dplyr::left_join(x, flossData)})
  }
  
  
  dataTableMerge <- function (dataSets) {
    
    if (!suppressMessages(require(data.table))) 
      install.packages('data.table')
    library(data.table)
    
    flossData <<- data.table(dataSets[[1]], key="Project.ID")
    
    for (id in 2:length(dataSets)) {
      flossData <<- merge(flossData, data.table(dataSets[[id]]),
                         by='Project.ID') # , all = TRUE
    }
  }
  
  
  # http://stackoverflow.com/a/17458887/2872891
  dataTableMerge2 <- function (dataSets) {
    
    if (!suppressMessages(require(data.table))) 
      install.packages('data.table')
    library(data.table)
    
    DT <- data.table(dataSets[[1]], key="Project.ID")
    flossData <<- lapply(dataSets[[1]][-1], function(x) DT[.(x)])
  }

  
  switch(method,
         "lapply"      = lapplyMerge (datasets),
         "lapply2"     = lapplyMerge2 (datasets),
         "reduce"      = reduceMerge (datasets),
         "reduce2"     = reduceMerge2 (datasets),
         "reshape"     = reshapeMerge (datasets),
         "plyr"        = plyrMerge (datasets),
         "dplyr"       = dplyrMerge (datasets),
         "data.table"  = dataTableMerge (datasets),
         "data.table2" = dataTableMerge2 (datasets)
  )
  
  return (flossData)
}


mergeData <- function (dataSource, prefix = "", fileName = "Merged") {
  
  prefixMsg <- ''
  msg <- ''
  
  if (prefix != '') prefixMsg <- paste0(" '", prefix, "'")
  if (DEBUG) msg <- '\n'
  msg <- paste0(msg, "Merging ", dataSource, prefixMsg, " data...")
  if (DEBUG) msg <- paste0(msg, '\n')
  message(msg)
  
  transformedDir <- file.path(TRANSFORMED_DIR, dataSource)
  mergedDir <- file.path(MERGED_DIR, dataSource)
  if (!file.exists(mergedDir))
    dir.create(mergedDir, recursive = TRUE)

  fileName <- paste0(prefix, fileName, RDS_EXT)
  mergedFile <- file.path(mergedDir, fileName)
  
  # load datasets of transformed data
  dataSets <- loadDataSets(transformedDir, prefix)
  
  # merge loaded datasets
  flossData <- mergeDataSets(dataSets, prefix) # method "plyr" is default

  # exclude inactive projects
  if (dataSource == "SourceForge")
    flossData <- flossData[is.na(flossData[["Active"]]) | 
                             flossData[["Active"]] != PRJ_INACTIVE_, ]

  # exclude outliers
  if (dataSource == "SourceForge") {
    
    outLim_DevTeamSize <- Sys.getenv("OUTLIER_LIM_DEV_TEAM_SIZE")
    if (outLim_DevTeamSize == "") {
      warning("Cannot find environment variable ",
              "OUTLIER_LIM_DEV_TEAM_SIZE, defaulting to 100!")
      outLim_DevTeamSize <- TEAM_SIZE
    }
    
    outLim_DevTeamSize <- as.numeric(outLim_DevTeamSize)
    if (is.na(outLim_DevTeamSize)) {
      warning("Value of environment variable OUTLIER_LIM_DEV_TEAM_SIZE",
              " is not numeric, defaulting to 100!")
      outLim_DevTeamSize <- TEAM_SIZE
    }
    
    flossData <- 
      flossData[is.na(flossData[["Development.Team.Size"]]) | 
                  flossData[["Development.Team.Size"]] <= outLim_DevTeamSize, ]
  }

  # exclude projects with low number of indicators with data
  if (FALSE) {
    x <- rowSums(is.na(flossData))
    i <- min(which(cumsum(table(x)) > MIN_NUM_PROJECTS))
    number <- names(table(x))[i]
  }

  flossData <- flossData[rowSums(!is.na(flossData)) > MIN_NUM_INDICATORS, ]

  # sub-sampling only if the sample is large enough
  if (nrow(flossData) > MIN_NUM_PROJECTS * 2)
    flossData <- sampleDF(flossData, MIN_NUM_PROJECTS)

  # verify the data frame structure
  if (DEBUG) str(flossData)
  
  # suppress "NAs introduced by coercion" warnings
  suppressWarnings(describe(flossData))
  
  # save merged data to a separate directory
  saveRDS(flossData, mergedFile)
}


message("===== STARTING DATA MERGING...")
if (!DEBUG) message("")

mergeData("SourceForge")

if (FALSE) {
  mergeData("FLOSSmole", "fc")
  mergeData("FLOSSmole", "fsf")
  mergeData("FLOSSmole", "gc")
  mergeData("FLOSSmole", "lpd")
  mergeData("FLOSSmole", "svProjectInfo")
  mergeData("FLOSSmole", "tigProjects")
}

message("\n===== DATA MERGING SUCCESSFULLY COMPLETED.\n")
