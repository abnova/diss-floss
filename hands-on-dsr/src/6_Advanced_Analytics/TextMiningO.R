
## ----module, echo=FALSE, results="asis"----------------------------------
Module <- "TextMiningO"
cat(paste0("\\newcommand{\\Module}{", Module, "}"))
set.seed(42)


## ----setup, child="mycourse.Rnw"-----------------------------------------

## ----setup_options, include=FALSE----------------------------------------
library(knitr)
library(xtable)

opts_chunk$set(cache=FALSE)

opts_chunk$set(out.width='0.8\\textwidth')
opts_chunk$set(fig.align='center')

opts_chunk$set(src.top=NULL)
opts_chunk$set(src.bot=NULL)
opts_chunk$set(out.lines=4)
opts_chunk$set(out.truncate=80)

opts_chunk$set(fig.path=sprintf("figures/%s/", Module))
opts_chunk$set(cache.path=sprintf("cache/%s/", Module))
opts_chunk$set(bib.file=paste0(Module, ".bib"))

# Leave code as I have formatted it.

opts_chunk$set(tidy=FALSE)

# Hooks

# Allow auto crop of base graphics plots when crop=TRUE.

knit_hooks$set(crop=hook_pdfcrop)

# Truncate long lines and long output

hook_output <- knit_hooks$get("output")
hook_source <- knit_hooks$get("source")
knit_hooks$set(output=function(x, options) 
{
  if (options$results != "asis")
  {
    # Split string into separate lines.
    x <- unlist(stringr::str_split(x, "\n"))
    # Trim to the number of lines specified.
    if (!is.null(n <- options$out.lines)) 
    {
      if (length(x) > n) 
      {
        # Truncate the output.
        x <- c(head(x, n), "....\n")
      }
    }
    # Truncate each line to length specified.
    if (!is.null(m <- options$out.truncate))
    {
      len <- nchar(x)
      x[len>m] <- paste0(substr(x[len>m], 0, m-3), "...")
    }
    # Paste lines back together.
    x <- paste(x, collapse="\n")
    # Replace ' = ' with '=' - my preference. Hopefully won't 
    # affect things inappropriately.
    x <- gsub(" = ", "=", x)
  }
  hook_output(x, options)
},
source=function(x, options)
{
  # Split string into separate lines.
  x <- unlist(stringr::str_split(x, "\n"))
  # Trim to the number of lines specified.
  if (!is.null(n <- options$src.top)) 
  {
    if (length(x) > n) 
    {
      # Truncate the output.
      if (is.null(m <-options$src.bot)) m <- 0
      x <- c(head(x, n+1), "\n....\n", tail(x, m+2)) 
   }
  }
  # Paste lines back together.
  x <- paste(x, collapse="\n")
  hook_source(x, options)
})

# Optionally allow R Code chunks to be environments so we can refer to them.

knit_hooks$set(rcode=function(before, options, envir) 
{
  if (before)
    sprintf('\\begin{rcode}\\label{%s}\\hfill{}', options$label)
  else
    '\\end{rcode}'
})



## ----load_packages, message=FALSE----------------------------------------
library(tm)		# Framework for text mining.
library(SnowballC)	# Provides wordStem() for stemming.
library(RColorBrewer)   # Generate palette of colours for plots.
library(ggplot2)        # Plot word frequencies.
library(Rgraphviz)      # Correlation plots.


## ----child-demo, child="documentation.Rnw", eval=TRUE--------------------


## ----help_library, eval=FALSE, tidy=FALSE--------------------------------
## ?read.csv


## ----help_package, eval=FALSE--------------------------------------------
## library(help=rattle)


## ----record_start_time, echo=FALSE---------------------------------------
start.time <- proc.time()


## ----generate_bib, echo=FALSE, message=FALSE, warning=FALSE--------------
# Write all packages in the current session to a bib file
if (is.null(opts_chunk$get("bib.file"))) opts_chunk$set(bib.file="Course.bib")
write_bib(sub("^.*/", "", grep("^/", searchpaths(), value=TRUE)),
          file=opts_chunk$get("bib.file"))
system(paste("cat extra.bib >>", opts_chunk$get("bib.file")))
# Fix up specific issues.
# R-randomForest
system(paste("perl -pi -e 's|Fortran original by Leo Breiman",
             "and Adele Cutler and R port by|Leo Breiman and",
             "Adele Cutler and|'", opts_chunk$get("bib.file")))
# R-C50
system(paste("perl -pi -e 's|. C code for C5.0 by R. Quinlan|",
             " and J. Ross Quinlan|'", opts_chunk$get("bib.file")))
# R-caret
system(paste("perl -pi -e 's|. Contributions from|",
             " and|'", opts_chunk$get("bib.file")))
# Me
system(paste("perl -pi -e 's|Graham Williams|",
             "Graham J Williams|'", opts_chunk$get("bib.file")))




## ----cli_convert_pdf, eval=FALSE-----------------------------------------
## system("for f in *.pdf; do pdftotext -enc ASCII7 -nopgbrk $f; done")


## ----cli_convert_doc, eval=FALSE-----------------------------------------
## system("for f in *.doc; do antiword $f; done")


## ----list_sources--------------------------------------------------------
getSources()


## ----list_readers, out.lines=NULL----------------------------------------
getReaders()


## ----location_of_txt_docs------------------------------------------------
cname <- file.path(".", "corpus", "txt")
cname


## ----folder_of_txt_docs--------------------------------------------------
length(dir(cname))
dir(cname)


## ----load_corpus---------------------------------------------------------
library(tm)
docs <- Corpus(DirSource(cname))
docs
class(docs)
class(docs[[1]])


## ----read_pdf, eval=FALSE------------------------------------------------
## docs <- Corpus(DirSource(cname), readerControl=list(reader=readPDF))


## ----read_doc, eval=FALSE------------------------------------------------
## docs <- Corpus(DirSource(cname), readerControl=list(reader=readDOC))


## ----read_doc_options, eval=FALSE----------------------------------------
## docs <- Corpus(DirSource(cname), readerControl=list(reader=readDOC("-r -s")))


## ----summary_docs, out.lines=NULL----------------------------------------
summary(docs)


## ----out.lines=26--------------------------------------------------------
inspect(docs[16])


## ------------------------------------------------------------------------
getTransformations()


## ----transform_slash-----------------------------------------------------
for (j in seq(docs))
{
  docs[[j]] <- gsub("/",   " ", docs[[j]])
  docs[[j]] <- gsub("@",   " ", docs[[j]])
  docs[[j]] <- gsub("\\|", " ", docs[[j]])
}


## ----out.lines=26--------------------------------------------------------
inspect(docs[16])


## ------------------------------------------------------------------------
docs <- tm_map(docs, tolower)


## ----out.lines=26--------------------------------------------------------
inspect(docs[16])


## ------------------------------------------------------------------------
docs <- tm_map(docs, removeNumbers)


## ----out.lines=26--------------------------------------------------------
inspect(docs[16])


## ------------------------------------------------------------------------
docs <- tm_map(docs, removePunctuation)


## ----out.lines=26--------------------------------------------------------
inspect(docs[16])


## ------------------------------------------------------------------------
docs <- tm_map(docs, removeWords, stopwords("english"))


## ----out.lines=26--------------------------------------------------------
inspect(docs[16])


## ----list_stopwords------------------------------------------------------
length(stopwords("english"))
stopwords("english")


## ----remove_own_stopwords------------------------------------------------
docs <- tm_map(docs, removeWords, c("department", "email"))


## ----out.lines=26--------------------------------------------------------
inspect(docs[16])


## ------------------------------------------------------------------------
docs <- tm_map(docs, stripWhitespace)


## ----out.lines=26--------------------------------------------------------
inspect(docs[16])


## ----specific_transforms-------------------------------------------------
for (j in seq(docs)) 
{
  
  docs[[j]] <- gsub("harbin institute technology", "HIT", docs[[j]])
  docs[[j]] <- gsub("shenzhen institutes advanced technology", "SIAT", docs[[j]])
  docs[[j]] <- gsub("chinese academy sciences", "CAS", docs[[j]])
}


## ----out.lines=26--------------------------------------------------------
inspect(docs[16])


## ------------------------------------------------------------------------
library(SnowballC)
docs <- tm_map(docs, stemDocument)


## ----out.lines=26--------------------------------------------------------
inspect(docs[16])


## ----create_document_term_matrix, out.lines=20---------------------------
dtm <- DocumentTermMatrix(docs)
dtm


## ----inspect_dtm---------------------------------------------------------
inspect(dtm[1:5, 1000:1005])


## ----dtm_matrix----------------------------------------------------------
class(dtm)
dim(dtm)


## ----create_term_document_matrix, out.lines=20---------------------------
tdm <- TermDocumentMatrix(docs)
tdm


## ------------------------------------------------------------------------
freq <- colSums(as.matrix(dtm))
length(freq)


## ----out.lines=10--------------------------------------------------------
ord <- order(freq)

# Least frequent terms
freq[head(ord)]


## ------------------------------------------------------------------------
# Most frequent terms
freq[tail(ord)]


## ------------------------------------------------------------------------
# Frequency of frequencies.
head(table(freq), 15)
tail(table(freq), 15)


## ----dtm_to_m------------------------------------------------------------
m <- as.matrix(dtm)
dim(m)


## ----save_csv, eval=FALSE------------------------------------------------
## write.csv(m, file="dtm.csv")


## ----remove_sparse_terms-------------------------------------------------
dim(dtm)
dtms <- removeSparseTerms(dtm, 0.1)
dim(dtms)


## ------------------------------------------------------------------------
inspect(dtms)


## ------------------------------------------------------------------------
freq <- colSums(as.matrix(dtms))
freq
table(freq)


## ----freq_terms_1000-----------------------------------------------------
findFreqTerms(dtm, lowfreq=1000)


## ----freq_terms_100------------------------------------------------------
findFreqTerms(dtm, lowfreq=100)


## ----assoc---------------------------------------------------------------
findAssocs(dtm, "data", corlimit=0.6)


## ----plot_correlations, echo=FALSE, out.width="\\textwidth", warning=FALSE, message=FALSE----
plot(dtm, 
     terms=findFreqTerms(dtm, lowfreq=100)[1:50], 
     corThreshold=0.5)


## ----plot_correlations, eval=FALSE---------------------------------------
## plot(dtm,
##      terms=findFreqTerms(dtm, lowfreq=100)[1:50],
##      corThreshold=0.5)


## ----plot_correlations_optoins, echo=FALSE, out.width="\\textwidth", warning=FALSE----
plot(dtm, 
     terms=findFreqTerms(dtm, lowfreq=100)[1:50], 
     corThreshold=0.5,
     weighting=TRUE)


## ----plot_correlations, eval=FALSE---------------------------------------
## plot(dtm,
##      terms=findFreqTerms(dtm, lowfreq=100)[1:50],
##      corThreshold=0.5)


## ----word_count----------------------------------------------------------
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
head(freq, 14)
wf   <- data.frame(word=names(freq), freq=freq)
head(wf)


## ----plot_freq, fig.width=12, out.width="\\textwidth"--------------------
library(ggplot2)
p <- ggplot(subset(wf, freq>500), aes(word, freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p


## ----wordcloud, echo=FALSE, warning=FALSE, message=FALSE, out.width="0.75\\textwidth", crop=TRUE----
library(wordcloud)
set.seed(123)
wordcloud(names(freq), freq, min.freq=40)


## ----wordcloud, eval=FALSE-----------------------------------------------
## library(wordcloud)
## set.seed(123)
## wordcloud(names(freq), freq, min.freq=40)


## ----wordcloud_max_words, echo=FALSE, out.width="0.75\\textwidth", crop=TRUE----
set.seed(142)
wordcloud(names(freq), freq, max.words=100)


## ----wordcloud_max_words, eval=FALSE-------------------------------------
## set.seed(142)
## wordcloud(names(freq), freq, max.words=100)


## ----wordcloud_higher_freq, echo=FALSE, out.width="0.75\\textwidth", crop=TRUE----
set.seed(142)
wordcloud(names(freq), freq, min.freq=100)


## ----wordcloud_higher_freq, eval=FALSE-----------------------------------
## set.seed(142)
## wordcloud(names(freq), freq, min.freq=100)


## ----wordcloud_colour, echo=FALSE, out.width="0.75\\textwidth", crop=TRUE----
set.seed(142)
wordcloud(names(freq), freq, min.freq=100, colors=brewer.pal(6, "Dark2"))


## ----wordcloud_colour, eval=FALSE----------------------------------------
## set.seed(142)
## wordcloud(names(freq), freq, min.freq=100, colors=brewer.pal(6, "Dark2"))


## ----wordcloud_scale, echo=FALSE, warning=FALSE, out.width="0.75\\textwidth", crop=TRUE----
set.seed(142)
wordcloud(names(freq), freq, min.freq=100, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))


## ----wordcloud_scale, eval=FALSE-----------------------------------------
## set.seed(142)
## wordcloud(names(freq), freq, min.freq=100, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))


## ----wordcloud_rotate, echo=FALSE, warning=FALSE, out.width="0.75\\textwidth", crop=TRUE----
set.seed(142)
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(freq), freq, min.freq=100, rot.per=0.2, colors=dark2)


## ----wordcloud_rotate, eval=FALSE----------------------------------------
## set.seed(142)
## dark2 <- brewer.pal(6, "Dark2")
## wordcloud(names(freq), freq, min.freq=100, rot.per=0.2, colors=dark2)


## ----review_prepare_corpus, eval=FALSE-----------------------------------
## # Required packages
## 
## library(tm)
## library(wordcloud)
## 
## # Locate and load the Corpus.
## 
## cname <- file.path(".", "corpus", "txt")
## docs <- Corpus(DirSource(cname))
## 
## docs
## summary(docs)
## inspect(docs[[1]])
## 
## # Transforms
## 
## for (j in seq(docs))
## {
##   docs[[j]] <- gsub("/",   " ", docs[[j]])
##   docs[[j]] <- gsub("@",   " ", docs[[j]])
##   docs[[j]] <- gsub("\\|", " ", docs[[j]])
## }
## 
## docs <- tm_map(docs, tolower)
## docs <- tm_map(docs, removeNumbers)
## docs <- tm_map(docs, removePunctuation)
## docs <- tm_map(docs, removeWords, stopwords("english"))
## docs <- tm_map(docs, removeWords, c("own", "stop", "words"))
## docs <- tm_map(docs, stripWhitespace)
## 
## for (j in seq(docs))
## {
##   docs[[j]] <- gsub("specific transform", "ST", docs[[j]])
##   docs[[j]] <- gsub("other specific transform", "OST", docs[[j]])
## }
## 
## docs <- tm_map(docs, stemDocument)
## 


## ----review_analyse_corpus, eval=FALSE-----------------------------------
## # Document term matrix.
## 
## dtm <- DocumentTermMatrix(docs)
## inspect(dtm[1:5, 1000:1005])
## 
## # Explore the corpus.
## 
## findFreqTerms(dtm, lowfreq=100)
## findAssocs(dtm, "data", corlimit=0.6)
## 
## freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
## wf   <- data.frame(word=names(freq), freq=freq)
## 
## library(ggplot2)
## 
## p <- ggplot(subset(wf, freq>500), aes(word, freq))
## p <- p + geom_bar(stat="identity")
## p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
## 
## # Generate a word cloud
## 
## library(wordcloud)
## wordcloud(names(freq), freq, min.freq=100, colors=brewer.pal(6, "Dark2"))


## ----common_outtro, child="finale.Rnw", eval=TRUE------------------------


## ----syinfo, child="sysinfo.Rnw", eval=TRUE------------------------------

## ----echo=FALSE, message=FALSE-------------------------------------------
require(Hmisc)
pkg <- "knitr"
pkg.version <- installed.packages()[pkg, 'Version']
pkg.date <- installed.packages(fields="Date")[pkg, 'Date']
pkg.info <- paste(pkg, pkg.version, pkg.date)

rev <- system("bzr revno", intern=TRUE)
cpu <- system(paste("cat /proc/cpuinfo | grep 'model name' |",
                    "head -n 1 | cut -d':' -f2"), intern=TRUE)
ram <- system("cat /proc/meminfo | grep MemTotal: | awk '{print $2}'",
              intern=TRUE)
ram <- paste0(round(as.integer(ram)/1e6, 1), "GB")
user <- Sys.getenv("LOGNAME")
node <- Sys.info()[["nodename"]]
user.node <- paste0(user, "@", node)
gcc.version <- system("g++ -v 2>&1 | grep 'gcc version' | cut -d' ' -f1-3",
                      intern=TRUE)
os <- system("lsb_release -d | cut -d: -f2 | sed 's/^[ \t]*//'", intern=TRUE)





