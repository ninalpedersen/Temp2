#' @export
#' @return Density plot
#' @title Density estimation plot
#' @usage Density.plot(x,n = 500,method,from,to)
#'
#' @description This function estimates the density of the given data.
#'
#' @param x numeric variable
#' @param n point you are evaluating to obtain its density
#' @param method method for estimating the density. Is either "naive" or "kernel". Set default as "naive".
#' @param from r
#' @param to r
#'
#' @author Nina Louise Pedersen \cr
#' Institute of Mathematics and Computer Science (IMADA) \cr
#' University of Sourthern Denmark, (SDU) \cr
#' \email{npede14@student.sdu.dk} \cr
#'
#' @examples
#' Density.plot(cars$speed,n = 500,method = "kernel")

Density.plot <- function(x, n = 500, method ="naive", from, to){

  # =====================================================
  if(method != "naive" && method != "kernel"){
    stop("Method must be either 'naive' or 'kernel'")
  }
  if(!is.numeric(x)){
    stop("x must be a numeric")
  }
  if(is.integer(n) || n <= 0){
    stop("n must be a positive integer")
  }
  if(missing(from)){
    from <- min(x)-1/3*sd(x)
  }
  if(missing(to)){
    to <- max(x)+1/3*sd(x)
  }


  # =====================================================

  if(method == "naive"){
    X <- rep(0,n)
    Y <- rep(0,n)
    j <- 1
    for(i in seq(from = from, to = to, by = (to-from)/(n-1))){
      X[j] <- i
      Y[j] <- Density(x, i, method = "naive")
      j <- j + 1
    }
    plot(X,Y, type = "s")
  }
  #if method is kernel
  else{
    X <- rep(0,n)
    Y <- rep(0,n)
    j <- 1
    for(i in seq(from = from, to = to, by = (to-from)/(n-1))){
      X[j] <- i
      Y[j] <- Density(x, i, method = "kernel")
      j <- j + 1
    }
    plot(X,Y, type = "l")
  }
}
