# Based on the StackOverflow Q&A: http://stackoverflow.com/q/23114654/2872891
# Adapted by Aleksandr Blekh (module-based structure; syntax & style updates)

# knitr hook function to allow an output.lines option
# e.g., 
#   output.lines=12 prints lines 1:12 ...
#   output.lines=1:12 does the same
#   output.lines=3:15 prints lines ... 3:15 ...
#   output.lines=-(1:8) removes lines 1:8 and prints ... 9:n ...
#   No allowance for anything but a consecutive range of lines

hook_output <- knit_hooks$get("output")

knit_hooks$set(output = function(x, options) {
  
  lines <- options$output.lines
  
  if (is.null(lines)) {
    return(hook_output(x, options))  # pass to default hook
  }
  
  x <- unlist(strsplit(x, "\n"))
  more <- "..."
  
  if (length(lines) == 1) {  # first n lines
    if (length(x) > lines) {
      # truncate the output, but add "..."
      x <- c(head(x, lines), more)
    }
  } else {
    x <- c(more, x[lines], more)
    #x <- c(ifelse(abs(lines[1]) > 1, more, NULL),
    #       x[lines],
    #       ifelse(length(x) > lines[abs(length(lines))], more, NULL))
  }
  
  # paste these lines together
  x <- paste(c(x, ""), collapse = "\n")
  hook_output(x, options)
})
