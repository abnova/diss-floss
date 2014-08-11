# Function (utility) that converts indicator-named RDS files
# into a single RDA file with correspondingly named objects.

rm(list=ls(all=TRUE)) # Clear R environment from previous sessions

rds2rda <- function(dataDir = ".", archiveFile = "mergeTestData.rda") {
  
  workDir <- getwd() # save working directory
  setwd(dataDir) # set new working directory
  
  objects <- c()
  
  for (file in dir(pattern='\\.rds$')) {
    nameSplit <- strsplit(file, "\\.")
    objName <- nameSplit[[1]][1]
    assign(objName, readRDS(file))
    #objects <- c(objects, get(objName))
    objects <- c(objects, objName)
  }

  #save(objects, file = archiveFile)
  save(list = objects, file = archiveFile)
  
  setwd(workDir) # restore the original working directory
}