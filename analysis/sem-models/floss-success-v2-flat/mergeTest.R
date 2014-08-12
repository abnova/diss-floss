testData <- "https://github.com/abnova/test/blob/master/mergeTestData.zip?raw=true"

tmpFile <- tempfile()
tmpDir <- tempdir()

download.file(testData, tmpFile, method = 'curl',
              extra = '-L', quiet = TRUE)
testFiles <- unzip(tmpFile, exdir = tmpDir)

# To enable desired merge option, uncomment corresponding line

#MERGE_OPTION <- "lapply_merge"
#MERGE_OPTION <- "lapply_merge2" # advice by Alexey G.
#MERGE_OPTION <- "reduce_merge"
#MERGE_OPTION <- "reduce_merge2"
#MERGE_OPTION <- "reshape"
MERGE_OPTION <- "plyr"
#MERGE_OPTION <- "dplyr"
#MERGE_OPTION <- "data.table"
#MERGE_OPTION <- "data.table2"


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


# load the datasets of transformed data
dataSets <- loadDataSets(tmpDir)


if (MERGE_OPTION == "lapply_merge") { # Option 1
  
  flossData <- data.frame(dataSets[[1]][1])
  
  # merge all loaded datasets by common column ("Project ID")
  silent <- lapply(seq(2, length(dataSets)),
                   function(i) {merge(flossData, dataSets[[1]][i],
                                      by = "Project ID",
                                      all = TRUE)})
}


if (MERGE_OPTION == "lapply_merge2") { # Option 1
  
  pids <- which(sapply(dataSets,
                       FUN=function(x) {'Project ID' %in% names(x)}))
  
  flossData <- dataSets[[pids[1]]]
  
  for (id in pids[2:length(pids)]) {
    flossData <- merge(flossData, dataSets[[id]],
                       by='Project ID', all = TRUE)
  }
}


if (MERGE_OPTION == "reduce_merge") { # Option 2
  
  flossData <- Reduce(function(...) 
    merge(..., by.x = "row.names", by.y = "Project ID", all = TRUE),
    dataSets)
}
  

# http://r.789695.n4.nabble.com/merge-multiple-data-frames-tt4331089.html#a4333772
if (MERGE_OPTION == "reduce_merge2") { # Option 2
    
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
  
  flossData <- mergeAll(dataSets)
}


if (MERGE_OPTION == "reshape") { # Option 3
  
  if (!suppressMessages(require(reshape))) install.packages('reshape')
  library(reshape)
  flossData <- reshape::merge_all(dataSets)
}


if (MERGE_OPTION == "plyr") { # Option 4
  
  if (!suppressMessages(require(plyr))) install.packages('plyr')
  library(plyr)
  
  flossData <- dataSets[[1]]
  for (i in seq.int(2, length(dataSets), 1)) {
    flossData <- plyr::join(flossData, dataSets[[i]],
                            by = 'Project ID',
                            type = 'left', match = 'first')
  }
}


if (MERGE_OPTION == "dplyr") { # Option 5
  
  if (!suppressMessages(require(dplyr))) install.packages('dplyr')
  library(dplyr)
  
  flossData <- dataSets[[1]][1]
  flossData <- lapply(dataSets[[1]][-1],
                      function(x) {dplyr::left_join(x, flossData)})
}


if (MERGE_OPTION == "data.table") { # Option 6
  
  if (!suppressMessages(require(data.table))) 
    install.packages('data.table')
  library(data.table)
  
  flossData <- data.table(dataSets[[1]])
  
  for (id in 2:length(dataSets)) {
    flossData <- merge(flossData, data.table(dataSets[[id]]),
                       by='Project ID') # , all = TRUE
  }
}


# http://stackoverflow.com/a/17458887/2872891
if (MERGE_OPTION == "data.table2") { # Option 6
  
  if (!suppressMessages(require(data.table))) 
    install.packages('data.table')
  library(data.table)
  
  DT <- data.table(dataSets[[1]], key="Project ID")
  flossData <- lapply(dataSets[[1]][-1], function(x) DT[.(x)])
}

# Additional Transformations (see TODO above)

# convert presence of Repo URL to integer
flossData[["Repo URL"]] <- as.integer(flossData[["Repo URL"]] != "")

# convert License Restrictiveness' factor levels to integers
#flossData[["License Restrictiveness"]] <- 
#  as.integer(flossData[["License Restrictiveness"]])

# convert User Community Size from character to integer
flossData[["User Community Size"]] <- 
  as.integer(flossData[["User Community Size"]])

# remove NAs
#flossData <- flossData[complete.cases(flossData[,3]),]
#rowsNA <- apply(flossData, 1, function(x) {any(is.na(x))})
#flossData <- flossData[!rowsNA,]

print(str(flossData))