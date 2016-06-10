#' @export
#' @return Stationary probabilities for each state
#' @title Markov Chain
#' @usage MarkovChain(p,k,n)
#'
#' @keywords stationary markov chain
#'
#' @description This function simulates the stationary probabilities from a transition probability matrix.
#'
#' @param p transition probability matrix
#' @param k integer indicating the initial state of the chain
#' @param n number of simulated steps
#'
#' @author Nina Louise Pedersen \cr
#' Institute of Mathematics and Computer Science (IMADA) \cr
#' University of Sourthern Denmark, (SDU) \cr
#' \email{npede14@student.sdu.dk} \cr
#'
#' @examples
#' MarkovChain(matrix(c(.5,.5,.5,.5),ncol=2,byrow=TRUE),1,1000)

MarkovChain <- function(p,k,n){

  # =====================================================

  if(missing(p)){
    stop("Parameter p is missing.")
  }
  if(missing(k)){
    stop("Parameter k is missing.")
  }
  if(missing(n)){
    stop("Parameter n is missing.")
  }
  if(!(dim(p)[1] == dim(p)[2])){
    stop("Matrix p must be a n by n matrix.")
  }
  if(!(k == as.integer(k))){
    stop("k must be an integer.")
  }
  if(!(n == as.integer(n))){
    stop("n must be an integer.")
  }
  if(n == TRUE){
    warning("n set as 'TRUE' is the same as setting n = 1")
  }
  if(n == FALSE){
    warning("n set as 'FALSE' is the same as setting n = 0")
  }
  if(k > dim(p)[1]){
    stop("k larger than dimension of matrix")
  }

  # =====================================================

  state <- rep(0,dim(p)[1])
  state[k] <- 1
  for(i in 1:n){
    state <- state %*% p
  }
  return(state)
}
