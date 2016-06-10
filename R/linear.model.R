#' @export
#' @return A list with residuals, the estimate, standard error, t-value, p-value, R squared, Adjusted R squared and an F-statistic
#' @title Linear Model
#' @usage linear.model(formula)
#'
#' @description This function creates a linear model.
#'
#' @param formula formula of the form dependent~predictor1+predictor2+...+predictorN
#'
#' @author Nina Louise Pedersen \cr
#' Institute of Mathematics and Computer Science (IMADA) \cr
#' University of Sourthern Denmark, (SDU) \cr
#' \email{npede14@student.sdu.dk} \cr
#'
#' @examples
#' linear.model(cars$speed,cars$dist)


linear.model <- function(formula){

  # =====================================================
  if(!is.language(formula)){
    stop("a formula must be provided")
  }

  # =====================================================

  # The first lines are from the lm function
  mcall <- match.call()
  mf <- match.call() # Set up matrix
  mf[[1L]] <- quote(stats::model.frame) # 1L holds 1 as integer
  mf <- eval(mf, parent.frame())

  nrows <- dim(mf)[1]
  ncols <- dim(mf)[2]

  # Find X and Y from formula
  Y <- mf[,1]
  X <- rep(1,nrows)
  for(i in 2:ncols){
    X <- cbind(X,mf[,i])
  }

  betaHat <- solve(t(X)%*%X)%*%(t(X)%*%Y)
  YHat <- X%*%betaHat
  eHat <- Y - YHat # The residuals

  # The standard error of the residuals are the same as the standard deviation of the residuals
  SER <- sd(eHat)

  # Then we find the sum of squared residuals
  RSS <- sum((eHat)^2)

  # Then we find the sum of squares of Y's
  SST <- sum((Y-mean(Y))^2)

  # Then we find the R^2
  R2 <- 1 - (RSS/SST)

  # Then we find the adjusted coefficient of determination
  R2.adj <- 1 - (RSS/(nrows-(ncols-1)-1))/(SST/(nrows-1))

  # Then we find the F-statistic and we start by finding the regression sum of squares, SSreg
  SSreg <- sum((YHat-mean(Y))^2)

  F.stat <- (SSreg/(ncols-1))/(RSS/(nrows-ncols))

  # Then we calculate the degress of freedom
  df <- nrows - (ncols-1) - 1

  # Then we find the p-value by using the F distribution since we know the F-statistic
  p.value <- 1 - pf(F.stat,ncols-1,df)

  # Then we find the standard error and to find it then we will need to find the error variance
  EV <- RSS/(nrows-(ncols-1)-1)
  SE <- sqrt(diag(EV*solve(t(X)%*%X)))

  # Then we find the t-value
  t.value <- betaHat/SE

  Pr <- 2*(1-pt(abs(t.value),df))

  # Creating the table.

  print(list("Call" = mcall,
             "Residuals" = data.frame("Min" = min(eHat), "Q1" = quantile(eHat,.25), "Median" = median(eHat), "Q3" = quantile(eHat,.75), "Max" = max(eHat), row.names = " "),
             "Coefficients" = data.frame("Estimate" = betaHat, "Std. Error" = SE, "t value" = t.value, "Pr(>|t|)" = Pr)))
  cat("Residual standard error:",SER, "on", df, "degrees of freedom", "\n")
  cat("Multiple R-squared:", R2, "Adjusted R-squared:", R2.adj, "\n")
  cat("F-statistic:", F.stat, "on", ncols-1, "and", df, "DF,", "p-value:", p.value)

  return(invisible(list("Residual standard error" = SER, "R squared" = R2, "R squared adjacent" = R2.adj, "F-statistic" = F.stat, "Degrees of freedom" = df, "p-value" = p.value, "betaHat" = betaHat)))

}

