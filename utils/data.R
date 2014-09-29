loadData <- function (dataFile) {
  
  if (file.exists(dataFile)) {
    data <- readRDS(dataFile)
  }
  else { # error() undefined - replaced for stop() for now
    stop("Data file \'", dataFile, "\' not found! Run 'make' first.")
  }
  return (data)
}


loadDataSets <- function (dataDir, prefix) {
  
  dataSets <- list()
  
  # '^' in regex is not necessary here, but this emphasizes the position
  dataFiles <- dir(dataDir, pattern=paste0('^', prefix, '.*', '\\.rds$'))
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


# sample() replacement function for vectors
# - excludes NAs from the sampling 
# - also handles cases with all NAs
sampleVector <- function (x, ...) {
  
  if (all(is.na(x))) {
    return(NA)
  }
  return (sample(x[!is.na(x)], ...))
}


# sample() replacement function for data frames
# - excludes NAs from the sampling 
sampleDF <- function (df, n) {
  
  df <- na.omit(df)
  if (n <= nrow(df))
    return (df[sample(nrow(df), n),])
  else
    stop("Attempting to sample more cases than available!")
}
