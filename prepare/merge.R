# into a single data frame for further analysis

# Start session with a clean R environment
rm(list = ls(all.names = TRUE))

if (!suppressMessages(require(psych))) install.packages('psych')
library(psych)

PRJ_HOME <- Sys.getenv("DISS_FLOSS_HOME") # getwd()

source(file.path(PRJ_HOME, "utils/data.R"))


TRANDFORMED_DIR <- file.path(PRJ_HOME, "data/transformed")
MERGED_DIR      <- file.path(PRJ_HOME, "data/merged")
RDS_EXT <- ".rds"

commonColumn <- data.frame(
  prefix  = c("fc",         "fsf",      "gc",        "lpd",  "sv",           "tig"),
  mergeBy = c("project_id", "proj_num", "proj_name", "name", "project_name", ""))


mergeDataSets <- function (datasets, prefix = "",
                           method = "plyr") {

  flossData <<- data.frame()
  
  if (prefix == "")
    mergeBy <- "Project ID"
  else {
    # lookup a column to merge data by
    index <- match(prefix , commonColumn$prefix)
    mergeBy <- commonColumn[index,]$mergeBy
  }
  
  lapplyMerge <- function (dataSets) {
    
    flossData <<- data.frame(dataSets[[1]][1])
    
    # merge all loaded datasets by common column ("Project ID")
    silent <- lapply(seq(2, length(dataSets)),
                     function(i) {merge(flossData, dataSets[[1]][i],
                                        by = "Project ID",
                                        all = TRUE)})
  }
  
  
  lapplyMerge2 <- function (dataSets) {
    
    pids <- which(sapply(dataSets,
                         FUN=function(x) {'Project ID' %in% names(x)}))
    
    flossData <<- dataSets[[pids[1]]]
    
    for (id in pids[2:length(pids)]) {
      flossData <- merge(flossData, dataSets[[id]],
                         by='Project ID', all = TRUE)
    }
  }
  
  
  reduceMerge <- function (dataSets) {
    
    flossData <<- Reduce(function(...) 
      merge(..., by.x = "row.names", by.y = "Project ID", all = TRUE),
      dataSets)
  }
  
  
  # http://r.789695.n4.nabble.com/merge-multiple-data-frames-tt4331089.html#a4333772
  reduceMerge <- function (dataSets) {
    
    mergeAll <- function(..., by = "Project ID", all = TRUE) {
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
    
    flossData <<- dataSets[[1]]
    for (i in seq.int(2, length(dataSets), 1)) {
      flossData <<- plyr::join(flossData, dataSets[[i]],
                               #by = 'Project ID',
                               by = mergeBy,
                               type = 'full', match = 'first') # 'left' 'all'
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
    
    flossData <<- data.table(dataSets[[1]], key="Project ID")
    
    for (id in 2:length(dataSets)) {
      flossData <<- merge(flossData, data.table(dataSets[[id]]),
                         by='Project ID') # , all = TRUE
    }
  }
  
  
  # http://stackoverflow.com/a/17458887/2872891
  dataTableMerge2 <- function (dataSets) {
    
    if (!suppressMessages(require(data.table))) 
      install.packages('data.table')
    library(data.table)
    
    DT <- data.table(dataSets[[1]], key="Project ID")
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
  
  transformedDir <- file.path(TRANDFORMED_DIR, dataSource)
  mergedDir <- file.path(MERGED_DIR, dataSource)
  if (!file.exists(mergedDir))
    dir.create(mergedDir, recursive = TRUE)

  fileName <- paste0(prefix, fileName, RDS_EXT)
  mergedFile <- file.path(mergedDir, fileName)
  
  # load datasets of transformed data
  dataSets <- loadDataSets(transformedDir, prefix)
  
  # merge loaded datasets
  flossData <- mergeDataSets(dataSets, prefix) # method "plyr" is default
  
  # verify the data frame structure
  str(flossData)
  
  # suppress "NAs introduced by coercion" warnings
  suppressWarnings(describe(flossData))
  
  # save merged data to a separate directory
  saveRDS(flossData, mergedFile)
}


message("\nMerging SourceForge data...\n")

mergeData("SourceForge")


message("\nMerging FLOSSmole data...\n")

mergeData("FLOSSmole", "fc")
mergeData("FLOSSmole", "fsf")
mergeData("FLOSSmole", "gc")
mergeData("FLOSSmole", "lpd")
mergeData("FLOSSmole", "sv")
mergeData("FLOSSmole", "tig")
