#' @export
#' @return Estimated pi and the probability of hit
#' @title Buffon's Needle
#' @usage buffon.needle(N = 1000, l = 1, d = 1)
#'
#' @keywords buffon needle pi
#'
#' @description The function estimates pi by simulating a throw with needles.
#'
#' @param N the number of needles thrown
#' @param l the length of the needle
#' @param d the distance between the stripes on the floor
#'
#' @author Nina Louise Pedersen \cr
#' Institute of Mathematics and Computer Science (IMADA) \cr
#' University of Sourthern Denmark, (SDU) \cr
#' \email{npede14@student.sdu.dk} \cr
#'
#' @examples
#' buffon.needle(3000,l = 2, d = 4)

buffon.needle <- function(N = 1000,l = 1, d = 1){

  # =====================================================
  if(!(N == as.integer(N) && N > 0)){
    stop("N must be a positive integer")
  }
  if(!(is.double(l) && l > 0)){
    stop("l must be a positive double")
  }
  if(!(is.double(d) && d > 0)){
    stop("d must be a positive double")
  }


  # =====================================================


  hit <- 0
  for (i in 1:N){
    U <- runif(1,0,d/2)
    theta <- runif(1,0,pi)
    if (U <= 0.5*l*sin(theta)){
      hit <- hit + 1
    }
  }
  P.hit <- hit/N
  PI <- (N*2*l)/(hit*d)
  return.list <- list("Estimated pi" = PI, "Probability of hit" = P.hit)
  return(return.list)
}
buffon.needle(3408,2.5,3)
