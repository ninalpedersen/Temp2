#' @export
#' @return List of true correlation, estimated correlation, standard error, bias and confidence interval
#' @title Bootstrap
#' @usage bootstrap(x,y,n=100,color="gold")
#'
#' @keywords bootstrap
#'
#' @description This function estimates the correlation using bootstrap.
#'
#' @param x variable from dataset
#' @param y variable from dataset
#' @param n number of bootstap replicates
#' @param plot defines whether a is wanted
#' @param color defines the color of the confidence interval on plot
#'
#' @author Nina Louise Pedersen \cr
#' Institute of Mathematics and Computer Science (IMADA) \cr
#' University of Sourthern Denmark, (SDU) \cr
#' \email{npede14@student.sdu.dk} \cr
#'
#' @examples
#' bootstrap(cars$speed,cars$dist)

bootstrap <- function(x,y,n=100,plot = TRUE, color="gold"){

  # =====================================================
  if(missing(x)){
    stop("argument x is missing")
  }
  if(missing(y)){
    stop("argument y is missing")
  }
  if(missing(n)){
    warning("argument n is missing. Default set n = 100")
  }
  if(n != as.integer(n) || n < 1|| is.logical(n)){
    stop("n must be an positive integer")
  }
  if(plot != TRUE && plot != FALSE){
    stop("plot must be either 'TRUE' or 'FALSE'")
  }


  # =====================================================

  corr <- rep(0,n) # Make room for n correlations
  for (i in 1:n){
    s <- sample(1:length(x), size = length(x), replace = TRUE)
    x.Sport <- x[s]
    y.Grade <- y[s]
    corr[i] <- cor(x.Sport,y.Grade)
  }
  corr.mean <- mean(corr) # Mean of the estimated correlations
  c.x.y <- cor(x,y) # True correlation
  standard.err <- sd(corr) # Standard error
  bias <- corr.mean - c.x.y # Bias
  # Confidence interval
  a = 0.05 # Alpha
  con.max <- corr.mean + qnorm(1-a/2)*standard.err
  con.min <- corr.mean - qnorm(1-a/2)*standard.err
  con.int <- c(con.min,con.max)
  return.list <- list("True correlation" = c.x.y, "Estimated correlation" = corr.mean,"Standard Error" = standard.err,"Bias" = bias,"Confidence interval" = con.int)
  # PLOT
  if(plot == TRUE){
    hist(corr,breaks = seq(min(corr)-0.02,max(corr)+0.02,0.02),main = "Plot of correlations", xlab = "Estimated Correlations")
    abline(v = con.int, col = color)
  }
  return(return.list)
}
