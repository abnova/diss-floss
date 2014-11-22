# script to produce citations for all used R packages

# Preliminary steps:
#
# 1. grep -r "library(" */*.R | awk -F: '{print $2}' > ~/diss-floss/bib/aa.txt
# 2. manual cleaning
# 3. duplicate lines/names removal: sort aa.txt | uniq > aaa.txt

packagesToCite <- readLines("bib/aaa.txt")

citationsFile <- "bib/rPackages.bib"


citeRpackages <- function (packagesToCite, citationsFile, bibtex = FALSE) {
  
  citFile <- file(citationsFile, "w+")
  
  text <- lapply(packagesToCite, function (x) citation(package = x))
  text <- unlist(text, recursive = FALSE)
  
  if (bibtex) {  # BibTex, currently not implemented
    
  } else {  # assume APA style
    
    citations <- 
      lapply(text, function (x) attr(x, "textVersion", exact = TRUE))
    
    lapply(citations, 
           function (x) write(paste0(x, "\n"), citFile, append = TRUE))
  }
  
  close(citFile)
}


citeRpackages(packagesToCite, citationsFile)