# Adaptive QQ function for reducing number of plotting points
# From whuber's answer on CrossValidated:
# http://stats.stackexchange.com/questions/35220/removing-extraneous-points-near-the-centre-of-a-qq-plot

qq <- function(x0, y0, t.y=0.0005, use.shortest=FALSE) {
  qq.int <- function(x,y, i.min,i.max) {
    # x, y are sorted and of equal length
    n <-length(y)
    if (n==1) return(c(x=x, y=y, i=i.max))
    if (n==2) return(cbind(x=x, y=y, i=c(i.min,i.max)))
    beta <- ifelse( x[1]==x[n], 0, (y[n] - y[1]) / (x[n] - x[1]))
    alpha <- y[1] - beta*x[1]
    fit <- alpha + x * beta
    i <- median(c(2, n-1, which.max(abs(y-fit))))
    if (abs(y[i]-fit[i]) > thresh) {
      assemble(qq.int(x[1:i], y[1:i], i.min, i.min+i-1), 
               qq.int(x[i:n], y[i:n], i.min+i-1, i.max))
    } else {
      cbind(x=c(x[1],x[n]), y=c(y[1], y[n]), i=c(i.min, i.max))
    }
  }
  assemble <- function(xy1, xy2) {
    rbind(xy1, xy2[-1,])
  }
  #
  # Pre-process the input so that sorting is done once
  # and the most detail is extracted from the data.
  #
  is.reversed <- length(y0) < length(x0)
  if (use.shortest) is.reversed <- !is.reversed
  if (is.reversed) {
    y <- sort(x0)
    n <- length(y)
    x <- quantile(y0, prob=(1:n-1)/(n-1))    
  } else {
    y <- sort(y0)
    n <- length(y)
    x <- quantile(x0, prob=(1:n-1)/(n-1))    
  }
  #
  # Convert the relative threshold t.y into an absolute.
  #
  thresh <- t.y * diff(range(y))
  #
  # Recursively obtain points on the QQ plot.
  #
  xy <- qq.int(x, y, 1, n)
  if (is.reversed) cbind(x=xy[,2], y=xy[,1], i=xy[,3]) else xy
}