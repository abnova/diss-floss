# Module responsible for merging already transformed data
# into a single data frame for further analysis

# Start session with a clean R environment
rm(list = ls(all.names = TRUE))

if (!suppressMessages(require(psych))) install.packages('psych')
library(psych)

SRDA_DIR <- "~/diss-floss/data/transformed/SourceForge"


loadData <- function (dataFile) {
  
  if (file.exists(dataFile)) {
    data <- readRDS(dataFile)
  }
  else { # error() undefined - replaced for stop() for now
    stop("Data file \'", dataFile, "\' not found! Run 'make' first.")
  }
  return (data)
}


loadDataSets <- function (dataDir) {
  
  dataSets <- list()
  
  dataFiles <- dir(dataDir, pattern='\\.rds$')
  dataSets <- lapply(seq_along(dataFiles),
                     function(i) {
                       nameSplit <- strsplit(dataFiles[i], "\\.")
                       dataset <- nameSplit[[1]][1]
                       assign(dataset,
                              loadData(file.path(dataDir, dataFiles[i])))
                       return (get(dataset))
                     })
  return (dataSets)
}


mergeDataSets <- function (datasets, method = "plyr") {

  flossData <<- data.frame()
  
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
                               by = 'Project ID',
                               type = 'left', match = 'first')
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


# load datasets of transformed data
dataSets <- loadDataSets(SRDA_DIR)

# merge loaded datasets
flossData <- mergeDataSets(dataSets) # method "plyr" is default

# verify the data frame structure
str(flossData)

# suppress "NAs introduced by coercion" warnings
suppressWarnings(describe(flossData))
