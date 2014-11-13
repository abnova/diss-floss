# Dynamically generate knit-ready chunk code for objects (figures/tables),
# ready for cross-referencing in a report document (supports .Rmd to LaTeX).

# Currently supports figures only,
# as tables generation is context- and target-specific (TBD).

genDynChunks <- function (refInfo, multi = FALSE, hold = TRUE) {
  
  latexObjLabel <- paste0("{{caption}}", "\\\\label{", refInfo$objType, ":{{varName}}", "}")
  
  chunkName <- "{{name2}}"
  chunkHeader <- paste0("```{r ", chunkName, ",")
  chunkOptions <- paste0("include=TRUE, results='asis', fig.height=3.5, fig.width=7, fig.cap='", latexObjLabel, "'")
  chunkHeaderFull <- paste(chunkHeader, chunkOptions, "}")
  chunkBody <- "print(get('{{name}}'))"
  
  chunkText <- c(chunkHeaderFull,
                 chunkBody,
                 "```", "\n")
  
  objChunks <- lapply(refInfo$objs, function (x) 
    knit_expand(text = chunkText,
                name = x,
                name2 = gsub('\\.', '_', x),
                varName = strsplit(x, refInfo$objTypePrefix)[[1]][2],
                caption = attr(get(x), 'title')))
  
  return (unlist(objChunks))
}
