#' @export
#' @return Return, depending on input, either the density in a point or the bandwidth and
#' a table of densities estimated at minimum, 1st quartile, median, mean, 3rd quartile, maximum.
#' @title Density estimator
#' @usage Density(x,d,h,method = "naive")
#'
#' @description This function estimates the density of the given data.
#'
#' @param x numeric variable
#' @param d point you are evaluating to obtain its density
#' @param h bandwidth
#' @param method method for estimating the density. Is either "naive" or "kernel". Set default as "naive".
#'
#' @author Nina Louise Pedersen \cr
#' Institute of Mathematics and Computer Science (IMADA) \cr
#' University of Sourthern Denmark, (SDU) \cr
#' \email{npede14@student.sdu.dk} \cr
#'
#' @examples
#' Density(cars$speed,3,0.33,method = "kernel")
#'

Density <- function(x, d, h, method = "naive"){

  # =====================================================
  if(method != "naive" && method != "kernel"){
    stop("Method must be either 'naive' or 'kernel'")
  }
  if(!is.numeric(x)){
    stop("x must be numeric")
  }
  if(!missing(d)){
    if(!is.numeric(d)){
      stop("d must be numeric ")
    }
  }
  if(!missing(h)){
    if(!is.numeric(h)){
      stop("h must be numeric")
    }
  }

  # =====================================================

  if(method == "naive"){
    if(missing(h)){
      R <- max(x)-min(x) # Sample range
      h <- R/(1+log2(length(x))) # Optimal width by Sturges
    }
    if(missing(d)){
      point <- c(min(x),quantile(x,.25),quantile(x,.50),mean(x),quantile(x,.75),max(x))
      point.density <- c(Density(x, point[1], h),Density(x, point[2], h),Density(x, point[3], h),Density(x, point[4], h),Density(x, point[5], h),Density(x, point[6], h))
      f.d <- data.frame(point,point.density)
      rownames(f.d) <- c("Min:", "1st Quartile:", "Median:", "Mean:", "3rd Quartile:", "Max:")
      colnames(f.d) <- c("x","y")
      cat("Bandwidth:", h, "\n")
    }

    else{
      w <- rep(0,length(x))
      for(i in 1:length(x)){
        t <- (d-x[i])/h
        if(abs(t)<1){
          w[i] <- 1/2
        }
      }
      f.d <- (1/length(x))*sum((1/h)*w)
    }
  }
  #Else method is kernel
  else{
    if(missing(h)){
      h = 0.9*sd(x)*length(x)^(-1/5) # Optimal width by Silverman
    }
    if(missing(d)){
      point <- c(min(x),quantile(x,.25),quantile(x,.50),mean(x),quantile(x,.75),max(x))
      point.density <- c(Density(x, point[1], h, method = "kernel"),Density(x, point[2], h, method = "kernel"),Density(x, point[3], h, method = "kernel"),Density(x, point[4], h, method = "kernel"),Density(x, point[5], h, method = "kernel"),Density(x, point[6], h, method = "kernel"))
      f.d <- data.frame(point,point.density)
      row.names(f.d) <- c("Min:", "1st Quartile:", "Median:", "Mean:", "3rd Quartile:", "Max:")
      colnames(f.d) <- c("x","y")
      cat("Bandwidth:", h, "\n")
    }
    else{
      w <- rep(0,length(x))
      for(i in 1:length(x)){
        w[i] <- 1/(sqrt(2*pi))*exp((-1/2)*((d-x[i])/h)^2)
      }
      f.d <- (1/length(x))*sum(1/h*w)
    }
  }
  return(f.d)
}


