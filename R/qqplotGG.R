############################################
##
##
#
#
#
#
#
#
#




qqplotGG <- function(y, distribution=qnorm) {
  require(ggplot2)
  x <- distribution(ppoints(y))
  d <- data.frame(x=x, y=sort(y))
  p <- ggplot(d, aes(x=x, y=y)) +
    geom_point(colour="lightblue4") +
    geom_line(aes(x=x, y=x),colour="magenta2",size=1) +
    xlab("Theoretical Quantiles") +
    ylab("Sample Quantiles")
  return(p)
}
