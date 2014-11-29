#
# This module contains functions, implementing RR-related functionality:
#
# 1. General functions (dynamic chunks, etc.)
# 2. Specialized functions (creating tables/figures for various analyses)
# 3. Miscellaneous (utility) functions


##### GENERAL FUNCTIONS

## CHUNKS

# Dynamically generates knit-ready chunk code for objects (figures/tables),
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
    figOptions <- paste0("fig.height=myScale*", fig.height, ", fig.width=myScale*", fig.width)
  
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


## REFERENCES

# Produces string with LaTeX ref. labels for figures/tables of specific type

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


##### SPECIALIZED FUNCTIONS - EDA

## TABLES

genEDAdescStatsTable <- function (df, label = "edaDescStats",
                                  caption = "EDA descriptive statistics",
                                  digits = 2) {
  
  is.numericORfactor <- function (x) { is.numeric(x) || is.factor(x) }
  
  df <- df[, sapply(df, is.numericORfactor)]
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


##### SPECIALIZED FUNCTIONS - EFA


##### SPECIALIZED FUNCTIONS - CFA


##### SPECIALIZED FUNCTIONS - SEM

## TABLES

# Generate R Markdown table with results of SEM analysis ('pander' 2nd ver.)

plspm.innerprint <- function(object, digits = DIGITS) {
  
  res1 <- do.call(rbind, lapply(names(object$inner_model), function(n) {
    data.frame(Outcome = n, object$inner_model[[n]])
  }))
  
  colnames(res1)[3:5] <- c("SE", "Tvalue", "Pvalue")
  res1$Pvalue <- format.pval(res1$Pvalue, digits = digits)

  pander(res1, split.tables = 200, round = digits)
}


# Generate R Markdown table with results of SEM analysis ('pander' 1st ver.)

# currently uses pandoc.table(); using methods is TBD:
# print.pander <- function (x, ...) UseMethod("pander")

genSEMtable <- function (obj, caption, label,
                         type = "1", format = "latex") {
  
  # if LaTeX, add label to the caption for cross-referencing
  if (format == "latex")
    caption <- paste0(caption, "\\label{tab:", label, "}")
  
  # set the caption, but don't re-use for next table(s)
  set.caption(caption, permanent = FALSE)
  
  # don't split tables
  ##panderOptions("table.split.table", Inf)
  
  # create table in R Markdown format
  ##pandoc.table(obj)  # more flexible alternative: pander()
  
  ##pander(obj, split.tables = 200, round = DIGITS)
  
  # return both caption/label and table in a list
  list(caption = caption, table = obj)
}


## FIGURES

genSEMfigure <- function (obj, caption, label) {
  
  # add label to the caption for cross-referencing
  caption <- paste0(caption, "\\label{fig:", label, "}")
  
  # return both caption/label and plot in a list
  list(caption = caption, plot = obj)
}


##### MISC FUNCTIONS #####


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
