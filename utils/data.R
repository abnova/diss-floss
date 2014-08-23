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
