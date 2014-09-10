## @knitr CleanEnv
rm(list = ls(all.names = TRUE))

## @knitr LoadPackages
library(psych)
library(ggplot2)
library(hexbin)

## @knitr PrepareData

set.seed(100) # for reproducibility
data(diamonds, package='ggplot2')  # use built-in data


## @knitr PerformEDA

generatePlot <- function (df, colName) {
  
  df <- df
  df$var <- df[[colName]]
  
  g <- ggplot(data.frame(df)) +
    scale_fill_continuous("Density", low="#56B1F7", high="#132B43") +
    scale_x_log10("Diamond Price [log10]") +
    scale_y_continuous("Density") +
    geom_histogram(aes(x = var, y = ..density..,
                       fill = ..density..),
                   binwidth = 0.01)
  return (g)
}


generatePlotBinHex <- function (df, colName) {
  
  df <- df
  myData <- df$var <- df[[colName]]
  
  # setting up manual QQ plot used to plot with and with out hexbins
  #xSamp <- rgamma(1000,8,.5) # sample data
  len <- length(myData)
  i <- seq(1, len, by = 1)
  probSeq <- (i-.5) / len # probability grid
  invCDF <- qnorm(probSeq, 0, 1) # theoretical quantiles for standard normal
  orderGam <- myData[order(myData)] # ordered sample
  df <- data.frame(invCDF, orderGam)

  g <- ggplot(df, aes(invCDF, orderGam)) + 
    stat_binhex(geom = "point", size = 1, bins = 300) + 
    geom_smooth(method = "lm")
  
  #g <- ggplot(df, aes(invCDF, orderGam)) + 
  #  stat_ecdf(geom = "hex")
  
  return (g)
}


performEDA <- function (data) {
  
  d_var <- paste0("d_", deparse(substitute(data)))
  assign(d_var, describe(data), envir = .GlobalEnv)
  
  for (colName in names(data)) {
    if (is.numeric(data[[colName]]) || is.factor(data[[colName]])) {
      t_var <- paste0("t_", colName)
      assign(t_var, summary(data[[colName]]), envir = .GlobalEnv)

      g_var <- paste0("g_", colName)
      assign(g_var, generatePlot(data, colName), envir = .GlobalEnv)

      h_var <- paste0("h_", colName)
      assign(h_var, generatePlotBinHex(data, colName), envir = .GlobalEnv)
    }
  }
}


performEDA(diamonds)
