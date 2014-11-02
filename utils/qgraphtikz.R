


qgraph.tikz <- function(input,...)
{
  arguments <- list(...)
  if(is.null(arguments$filename)) filename="qgraph" else filename=arguments$filename
  if(is.null(arguments$standAlone)) standAlone=TRUE else standAlone=arguments$standAlone
  if(is.null(arguments$width)) width <- 7 else width <- arguments[['width']]
  if(is.null(arguments$height)) height <- 7 else height <- arguments[['height']]
  if(is.null(arguments$tooltips)) tooltips=NULL else tooltips=arguments$tooltips
  
  class(arguments) <- "qgraph"
  
  # Special thanks to Charlie Sharpsteen for supplying these tikz codes on stackoverflow.com !!!
  
  if (!suppressPackageStartupMessages(require(tikzDevice,quietly=TRUE))) stop("tikzDevice must be installed to use filetype='tex'")
  opt= c( 
    getOption('tikzLatexPackages'),  
    "\\def\\tooltiptarget{\\phantom{\\rule{1mm}{1mm}}}",
    "\\newbox\\tempboxa\\setbox\\tempboxa=\\hbox{}\\immediate\\pdfxform\\tempboxa \\edef\\emptyicon{\\the\\pdflastxform}",
    "\\newcommand\\tooltip[1]{\\pdfstartlink user{/Subtype /Text/Contents  (#1)/AP <</N \\emptyicon\\space 0 R >>}\\tooltiptarget\\pdfendlink}"
    )
  
  place_PDF_tooltip <- function(x, y, text)
  {
    
    # Calculate coordinates
    tikzX <- round(grconvertX(x, to = "device"), 2)
    tikzY <- round(grconvertY(y, to = "device"), 2)
    # Insert node
    tikzAnnotate(paste(
      "\\node at (", tikzX, ",", tikzY, ") ",
      "{\\tooltip{", text, "}};",
      sep = ''
      ))
    invisible()
  }
  
  #print("NOTE: Using 'tex' as filetype will take longer to run than other filetypes")
  
  tikzDevice:::tikz(paste(filename,".tex",sep=""), standAlone = standAlone, width=width, height=height, packages=opt)
  
  Q <- qgraph(input,filetype="",arguments)
  layout <- Q$layout
  for (i in 1:nrow(layout))
  {
      if (!is.null(tooltips)) if (!is.na(tooltips[i])) place_PDF_tooltip(layout[i,1],layout[i,2],tooltips[i])
  }
  
  dev.off()
  
  invisible(Q)
}



