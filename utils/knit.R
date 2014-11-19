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


genEDAdescStatsTable <- function (df, label = "edaDescStats",
                                  caption = "EDA descriptive statistics",
                                  digits = 2) {
  
  df <- df[, sapply(df, is.numeric)]
  df <- psych::describe(df)
  
  df <- as.data.frame(round(df, digits))
  
  df$vars <- rownames(df)
  colsToInclude <- c("n", "mean", "sd", "median",
                     "min", "max", "skew", "kurtosis")
  tableCols <- c("N", "Mean", "SD", "Median",
                 "Min", "Max", "Skew", "Kurtosis")
  
  df <- df[-1, ]  # remove Project.ID
  df <- df[, colsToInclude]
  names(df) <- tableCols
  
  edaDescStatsTable <- as.tabular(df)
  
  # set the caption (specific for 'tables' package)
  latexCap <- paste0("\\caption{", caption, ".}\\\\", "\n",
                     "\\toprule",
                     "\\label{tab:", label, "}")
  
  # set tabular settings
  booktabs()
  
  # output LaTeX table
  latex(edaDescStatsTable,
        mathmode = FALSE, # output dash instead of LaTeX minus sign character
        options = list(tabular = "longtable",
                       toprule = latexCap))
}


# produces string with LaTeX reference labels for figures/tables of specific type
genObjRefs <- function (objType, objTypePrefix) {
  
  objs <- ls(pattern = objTypePrefix, envir = .GlobalEnv)
  if (length(objs) == 0)
    stop(paste("No objects of type", objTypePrefix, "found!"))
  
  split <- strsplit(objs, objTypePrefix)
  objRefs <- sapply(split, `[[`, 2)
  #objRefs <- split[[seq(split)]][2]
  
  objAllRefs <- c()
  for (i in seq(objRefs)) objAllRefs <- c(objAllRefs, objRefs[[i]])
  refKeyword <- ifelse(objType == "fig", "\\ref{fig:", "\\ref{tab:")
  refStr <- sapply(objAllRefs, function (x) {paste0(refKeyword, x, "}")})
  
  colFlag <- ""; refStrTemp <- ""
  objWord <- ifelse(objType == "fig", "Figures ", "Tables ")
  
  if (length(refStr) < 2) {
    objWord <- ifelse(objType == "fig", "Figure ", "Table")
    refStrFinal <- paste(objWord, refStr[length(refStr)])
  }
  else {
    if (length(refStr) == 2) colFlag <- " and "
    else if (length(refStr) > 2) colFlag <- ", "
    
    refStrTemp <- paste(refStr[-length(refStr)], collapse = colFlag)
    refStrFinal <- paste(objWord, refStrTemp, " and ", refStr[length(refStr)])
  }
  
  list(objs = objs, str = refStrFinal,
       objType = objType, objTypePrefix = objTypePrefix)
}


sanitize <- function(str) {
  result <- str
  result <- gsub("\\\\", "SANITIZE.BACKSLASH", result)
  result <- gsub("$", "\\$", result, fixed = TRUE)
  result <- gsub(">", "$>$", result, fixed = TRUE)
  result <- gsub("<", "$<$", result, fixed = TRUE)
  result <- gsub("|", "$|$", result, fixed = TRUE)
  result <- gsub("{", "\\{", result, fixed = TRUE)
  result <- gsub("}", "\\}", result, fixed = TRUE)
  result <- gsub("%", "\\%", result, fixed = TRUE)
  result <- gsub("&", "\\&", result, fixed = TRUE)
  result <- gsub("_", "\\_", result, fixed = TRUE)
  result <- gsub("#", "\\#", result, fixed = TRUE)
  result <- gsub("^", "\\verb|^|", result, fixed = TRUE)
  result <- gsub("~", "\\~{}", result, fixed = TRUE)
  result <- gsub("SANITIZE.BACKSLASH", "$\\backslash$", 
                 result, fixed = TRUE)
  return(result)
}


# TBD, for now will use pandoc.table()
#print.pander <- function (x, ...) UseMethod("pander")
