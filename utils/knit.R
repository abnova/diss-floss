# Dynamically generate knit-ready chunk code for objects (figures/tables),
# ready for cross-referencing in a report document (supports .Rmd to LaTeX).

genDynChunk <- function (obj, multi = FALSE, hold = TRUE) {
  
  latexObjLabel <- paste0("{{name}}\\\\label{", bcRefStr$objType, ":{{name}}", "}")
  
  chunkName <- "{{name}}"
  chunkHeader <- paste0("```{r ", chunkName, ", ")
  chunkOptions <- paste0("include=TRUE, results='asis', fig.height=4, fig.width=4, fig.cap='", latexObjLabel, "'")
  chunkHeaderFull <- paste0(chunkHeader, chunkOptions, "}")
  chunkBody <- "print(get('{{name}}'))"
  
  chunkText <- c(chunkHeaderFull,
                 chunkBody,
                 "```", "\n")
  
  figReportParts <- lapply(bcRefStr$objs, function (x) knit_expand(text = chunkText, name = x))
}
