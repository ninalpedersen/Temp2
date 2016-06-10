#' @export
#' @return The probability that chi squared are larger than x
#' @title Chi Probability
#' @usage chi.probability(x, df = 1, n = 1)
#'
#' @keywords chi probability
#'
#' @description This function calculates the probability chi squared is larger than the specified x value
#'
#' @param x value to be evaluated
#' @param df degrees of freedom
#' @param n number of random numbers
#' @param plot specifies whether a plot should be created or not
#'
#' @author Nina Louise Pedersen \cr
#' Institute of Mathematics and Computer Science (IMADA) \cr
#' University of Sourthern Denmark, (SDU) \cr
#' \email{npede14@student.sdu.dk} \cr
#'
#' @examples
#' chi.probability(4, df = 4, n = 2000)

chi.probability <- function(x, df = 1, n = 1){

  # =====================================================

  if(missing(x)){
    stop("x is missing")
  }
  if(!is.numeric(x)){
    stop("x must be numeric")
  }
  if(!(df == as.integer(df) || df > 0)){
    stop("df must be a posive integer")
  }
  if(!(n == as.integer(n) || n < 0)){
    stop("n must be a posive integer")
  }

  # =====================================================


  C <- rnorm(n)^2

  if(df > 1){
    for(i in 2:df){
      C <- C + rnorm(n)^2
    }
  }

  cdf <- ecdf(C)
  return.List <- list("Chi square probability" = 1-cdf(x))
  return(return.List)
}
chi.probability(2, df=4,1000)
