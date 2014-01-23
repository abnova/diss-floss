if (!require(rPython)) install.packages('rPython')
if (!require(RCurl)) install.packages('RCurl')
#if (!require(RJSONIO)) install.packages('RJSONIO')

library(rPython)
library(RCurl)
#library(RJSONIO)

baseURL <- "http://api.angel.co/1/tags/59/startups"

#url <- paste(baseURL, "?page=", page, collapse="", sep="")
#startupData <- getURL(url)
#fromJSON(startupData)

url <- baseURL
python.assign("pyURL", url)

python.load("rPython.py")
#python.load(system.file("json.py", package="pandas"))
#python.exec("import pandas as pd")
#python.exec("from pandas.io import json_normalize")
#python.exec("df = pd.DataFrame(json_normalize(read_json(pyURL)))")
python.get("data")
