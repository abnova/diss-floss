if (FALSE) {
  library(wordcloud)
  library(tm)
  data(SOTU)
  corp <- SOTU
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, removePunctuation)
  #corp <- tm_map(corp, tolower)
  corp <- tm_map(corp, content_transformer(tolower)) # FIX
  corp <- tm_map(corp, removeNumbers)
  corp <- tm_map(corp, function(x)removeWords(x,stopwords()))
  
  term.matrix <- TermDocumentMatrix(corp)
  term.matrix <- as.matrix(term.matrix)
  colnames(term.matrix) <- c("SOTU 2010","SOTU 2011")
  comparison.cloud(term.matrix,max.words=300,random.order=FALSE)
  commonality.cloud(term.matrix,random.order=FALSE)
}


if (FALSE) {
  library(tm)
  library(wordcloud)
  library(stringr)
  library(RColorBrewer)
  repub <- paste(readLines("repub_debate.txt"),collapse="\n")
  r2 <- strsplit(repub,"GREGORY\\:")[[1]]
  splitat <- str_locate_all(repub,
                            "(PAUL|HILLER|DISTASOS|PERRY|HUNTSMAN|GINGRICH|SANTORUM|ROMNEY|ANNOUNCER|GREGORY)\\:")[[1]]
  speaker <- str_sub(repub,splitat[,1],splitat[,2])
  content <- str_sub(repub,splitat[,2]+1,c(splitat[-1,1]-1,nchar(repub)))
  names(content) <- speaker
  tmp <- list()
  for(sp in c("GINGRICH:"  ,"ROMNEY:"  ,  "SANTORUM:","PAUL:" ,     "PERRY:",     "HUNTSMAN:")){
    tmp[sp] <- paste(content[sp==speaker],collapse="\n")
  }
  collected <- unlist(tmp)
  
  rcorp <- Corpus(VectorSource(collected))
  rcorp <- tm_map(rcorp, removePunctuation)
  rcorp <- tm_map(rcorp, removeNumbers)
  rcorp <- tm_map(rcorp, stripWhitespace)
  rcorp <- tm_map(rcorp, tolower)
  rcorp <- tm_map(rcorp, function(x)removeWords(x,stopwords()))
  rterms <- TermDocumentMatrix(rcorp)
  rterms <- as.matrix(rterms)
  comparison.cloud(rterms,max.words=Inf,random.order=FALSE)
  commonality.cloud(rterms)
}


if (TRUE) {
  library(wordcloud)
  #data(SOTU)

  #corp <- tm_map(corp, content_transformer(tolower)) # FIX
  #corp <- tm_map(corp, function(x) removeWords(tolower(x), stopwords()))
  #term.matrix <- TermDocumentMatrix(corp)
  #term.matrix <- as.matrix(term.matrix)
  
  words <- paste(readLines("~/latex//diss_ultraclean.tex"), collapse="\n")
  wordcloud(words, colors=brewer.pal(6, "Dark2"), random.order=FALSE)
}
