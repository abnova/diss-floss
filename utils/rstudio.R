# Utility functions, specific to the RStudio platform


# preview HTML code in RStudio Viewer Pane
# (initially planned for use for EFA/CFA/SEM tables; currently unused)

htmlPreview <- function (htmlLines) {
  htmlFile <- tempfile(fileext = ".html")
  writeLines(htmlLines, htmlFile)
  rstudio::viewer(htmlFile)
}