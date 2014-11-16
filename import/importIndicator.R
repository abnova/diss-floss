# Quickly hacked solution for manual import of an indicator
# (until the issue on the server side is fixed;
#  see lines with '*****' for specifying a particular indicator).

if (!suppressMessages(require(RCurl))) install.packages('RCurl')
if (!suppressMessages(require(jsonlite)))
install.packages("jsonlite", repos="http://cran.r-project.org")
if (!suppressMessages(require(stringr))) install.packages('stringr')
# INFO: Possible methods of suppressing messages
#suppressMessages(library(RCurl))
#suppressPackageStartupMessages(library(RCurl))
#invisible(capture.output(library(RCurl, quietly=TRUE)))
# library() calls are still needed in case of a system, lacking
# these packages, as require() load only installed packages
library(RCurl)
library(jsonlite)
library(stringr)
PRJ_HOME  <- Sys.getenv("DISS_FLOSS_HOME")
SRDA_USER <- Sys.getenv("SRDA_USER")
SRDA_PASS <- Sys.getenv("SRDA_PASS")
source(file.path(PRJ_HOME, "utils/string.R"))
#source("../utils/utils.R")
#source("../utils/debug.R")
skipped <<- 0 # counter for # of times the script skipped processing
# SRDA data collection configuration template file
SRDA_TEMPLATE <- "SourceForge.cfg.tmpl"
# SRDA data collection configuration file (auto-generated)
SRDA_CONFIG <- "SourceForge.cfg.json"
# Users must authenticate to access Query Form
SRDA_HOST_URL  <- "http://zerlot.cse.nd.edu"
SRDA_LOGIN_URL <- "/mediawiki/index.php?title=Special:Userlogin"
SRDA_LOGIN_REQ <- "&action=submitlogin&type=login"
# SRDA URL that Query Form sends POST requests to
SRDA_QUERY_URL <- "/cgi-bin/form.pl"
# SRDA URL that Query Form sends POST requests to
SRDA_QRESULT_URL <- "/qresult/blekh/blekh.txt"
RESULTS_URL <- paste0(SRDA_HOST_URL, SRDA_QRESULT_URL)
POLL_TIME <- 2 # polling timeout in seconds
# Parameters for result's format
DATA_SEP <- ":" # data separator
ADD_SQL  <- "0" # add SQL to file
REPLACE_CLAUSE <- "REPLACE(REPLACE(REPLACE(a.details, ':', ';'), CHR(10),' '), CHR(13),' ')"
RQ_SIZE <- 50000 # number of records returned by a single SQL query
SPECIFY_PROJECT_ID_RANGE <<- TRUE # depends on 'resultSize' config. attr.
PID_LOW <- 1
PID_HIGH <<- 0 # auto-initialized by 'resultSize' config. attribute
RDATA_EXT <- ".RData"
RDS_EXT <- ".rds"
#TODO: consider passing this via CL args
RDATA_DIR <- file.path(PRJ_HOME, "cache/SourceForge")
R_ENV_FILE <- "~/.Renviron"
envVarFound <- FALSE
# Data source prefix (to construct data object names)
dsPrefix <- ""
blacklist <- c()
DEBUG <- TRUE # TODO: retrieve debug flag via CL arguments
DEBUG2 <- TRUE ##temp (TODO: switch to FALSE)

# First, read file as a stream of characters, so that later we can
# replace the CR/LF characters pair with a single space character
# to prevent incorrect parsing of fields with embedded newline
# by the read.table() function.
fileLen <- url.exists(RESULTS_URL, .header=TRUE)["Content-Length"]
if (is.na(fileLen)) {
if (DEBUG) message("Empty result for request, nothing to process!\n")
return (invisible())
}
results <- readChar(RESULTS_URL, nchars = fileLen, TRUE)
# Then we need to replace all occurences of
#
# ": "       with "!@#"
# "X::Y"     with "X@@Y"
# "http://"  with "http//"
# "https://" with "https//"
# "mailto:"  with "mailto@"
# "svn://"   with "svn//",
#
# since we have to use semicolon as a field separator,
# to prevent incorrect parsing of the data.
#
# After the processing, we have to return data
# (now in a data frame) to the original state (post-processing).
# Note, that the following substitution code works only for
# the specific data separator ':'. More universal code is TBD.
rx <- "([[:alpha:]][^.:]|[[:blank:]])::([[:alpha:]][^:]|[[:blank:]])"
results <- gsub(rx, "\\1@@\\2", results)
results <- gsub(": ", "!@#", results) # should be after the ::-gsub
results <- gsub("http://", "http//", results)
results <- gsub("https://", "https//", results)
results <- gsub("mailto:", "mailto@", results)
results <- gsub("svn://", "svn//", results)
# Since some results contain fields with embedded newlines,
# direct use of read.table() parses data incorrectly.
# Then, we have to replace the problematic pair of characters
# (\0xD\0xA - CR/LF - \r\n) with a single space character (' ').
# It's important that the next line is executed after the data
# separator-related (':') substitutions, otherwise results with
# newlines right after data separator will be parsed incorrectly
# (":\r\n" => ": " => "!@#" => loss of one data field).
results <- gsub("-\\r\\n", "-", results) # order is important here
results <- gsub("\\r\\n", " ", results)
# remove multiple LF characters within a project record, if any
rx <- "\\n([^0-9])"
while(any(grepl(rx, results))) {results <- gsub(rx, "\\1", results)}
# fix for improperly formatted result data (AniSa, project 7606)
results <- gsub("\\n:gpl:962356288", ":gpl:962356288", results)
# Then we read intermediate results as text lines, count lines
# and then delete last character on each line (extra separator)
results <- readLines(textConnection(unlist(results)))
numLines <- length(results)
results <- lapply(results, function(x) gsub(".$", "", x))
#if (DEBUG2) print(head(results))
# Then we can parse the intermediate results as usual
data <- read.table(textConnection(unlist(results)),
header = FALSE, fill = TRUE,
sep = DATA_SEP, quote = "",
colClasses = "character", row.names = NULL,
nrows = numLines, comment.char = "",
strip.white = TRUE)
# Now we can safely do post-processing, recovering original data
data <- replace_all(data, fixed("!@#"), ": ")
data <- replace_all(data, fixed("@@"), "::")
data <- replace_all(data, fixed("http//"), "http://")
data <- replace_all(data, fixed("https//"), "https://")
data <- replace_all(data, fixed("mailto@"), "mailto:")
data <- replace_all(data, fixed("svn//"), "svn://")

dataName <- "a"
assign(dataName, data)

# ***** TODO: adjust names for needed indicator
varNames <- "Project ID, Registration Time"

# specify names for the current data object per configuration
varNamesModif <- strsplit(varNames, split = ",")
varNamesModif <- lapply(varNamesModif, str_trim)
names(data) <- unlist(varNamesModif)

config <- jsonlite::fromJSON(file.path(PRJ_HOME, "import", SRDA_CONFIG))

message("Reading configuration file ...\n")
config <- jsonlite::fromJSON(SRDA_CONFIG)
numRequests <- nrow(config$data)
if (DEBUG) {
msg <- paste0("Data ", config["_action"],
" from ", config["_source"],
", using schema \"", config["_schema"], "\".")
message(msg)
msg <- paste("Total number of requests to submit:", numRequests)
message(msg, "\n")
}
# Save data source prefix in a global variable
dsPrefix <<- config["_prefix"]
# Process projects' blacklist
blacklist <<- config["_blacklist"]
blacklistPIDs <- strsplit(unlist(blacklist), split = ",")
blacklistPIDs <- lapply(blacklistPIDs, str_trim)
blacklist <<- unlist(blacklistPIDs)

# remove projects from the blacklist
data <- data[!(data[["Project ID"]] %in% blacklist), ]

# ***** TODO: adjust name for needed indicator
fileName <- "prjAge.rds"

rdataFile <- file.path(RDATA_DIR, fileName)
saveRDS(data, rdataFile)
# clean up
rm(data)
gc()
