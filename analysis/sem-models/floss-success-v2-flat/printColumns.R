SRDA_DIR <- "~/diss-floss/data/transform/SourceForge"


printInfo <- function (dataFile) {
  
  if (file.exists(dataFile)) {
    data <- readRDS(dataFile)
    print(paste0("Object '", dataFile, "' has the following columns:"),
          quote = FALSE)
    print(names(data), quote = FALSE)
    print("", quote = FALSE)
  }
  else { # error() undefined - replaced for stop() for now
    stop("Data file \'", dataFile, "\' not found! Run 'make' first.")
  }
}


printColumns <- function (dataDir) {
  
  dataSets <- list()
  
  dataFiles <- dir(dataDir, pattern='\\.rds$')
  dataSets <- lapply(seq_along(dataFiles),
                     function(i) {
                       printInfo(file.path(dataDir, dataFiles[i]))
                     })
}


# print columns of the datasets of transformed data
printColumns(SRDA_DIR)
