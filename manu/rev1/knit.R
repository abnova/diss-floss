library(knitr)
inFile = commandArgs(trailingOnly=TRUE)[1]
outFile = commandArgs(trailingOnly=TRUE)[2]
knit(inFile,output=outFile)
