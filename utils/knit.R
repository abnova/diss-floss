# Dynamically generate knit-ready chunk code for objects (figures/tables),
# ready for cross-referencing in a report document (supports .Rmd to LaTeX).

# Currently supports figures only,
# as tables generation is context- and target-specific (TBD).

genDynChunks <- function (refInfo, multi = FALSE, hold = TRUE,
                          fig.height, fig.width) {
  
  figOptions <- ""
  
  latexObjLabel <- paste0("{{caption}}", "\\\\label{", refInfo$objType, ":{{varName}}", "}")
  
  chunkName <- "{{name2}}"
  chunkHeader <- paste0("```{r ", chunkName, ",")
  
  if (!(missing(fig.height) && missing(fig.width)))
    figOptions <- paste0("fig.height=", fig.height, ", fig.width=", fig.width)
  
  chunkOptions <- paste0("include=TRUE, results='asis', ", figOptions, ", fig.cap='", latexObjLabel, "'")
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
