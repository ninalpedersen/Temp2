#' @export
#' @return Goodness of fit value
#' @title Chi Probability
#' @usage chi.GOF(x,p = 1/length(x))
#'
#' @keywords goodness of fit chi
#'
#' @description This function computes the chi squared goodness of fit.
#'
#' @param x categorical variable
#' @param p expected probability
#'
#' @author Nina Louise Pedersen \cr
#' Institute of Mathematics and Computer Science (IMADA) \cr
#' University of Sourthern Denmark, (SDU) \cr
#' \email{npede14@student.sdu.dk} \cr
#'
#' @examples
#' chi.GOF(cars$speed)


chi.GOF <- function(x,p = 1/length(x)){

  # =====================================================
  if(missing(x)){
    stop("x is missing")
  }
  if(missing(p)){
    warning("p is missing, default is set to uniform distributed")
    p = rep(1/length(x),length(x))
  }
  if(!(is.numeric(x))){
    stop("x must be numeric")
  }
  if(!(is.numeric(p))){
    stop("p must be numeric")
  }

  if(length(p)==1){
    if(!(p*length(x) == 1)){
      stop("the expected does not give 100%.")
    }
  }
  else{
    if(!(sum(p)==1)){
        stop("the expected does not give 100%")
    }
  }
  if(!(length(p) == length(x))){
    stop("p and x must have the same length")
  }
  # =====================================================

  E <- p*sum(x)
  Q <- sum((x-E)^2/E)
  return(Q)
}

