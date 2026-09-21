#' @title Self-normalized hypothesis test of Shao (2010)
#' 
#' @description Conducts a self-normalized hypothesis test of Shao (2010) for signficance level 0.1. Other levels are not coded into this function.
#'
#' @param y a vector of observations or residuals from a static regression, for which the ACF should be computed.
#' @param H maximum number of lags for ACF.
#' @param alpha significance level. Only 0.1 is allowed.
#' 
#' @return Binary test decision: 1 for a rejection, 0 for non-rejection.
#' 



shao <- function(y, H, alpha){
  
  if(alpha != 0.1) stop( "The significance level must be 0.1.")
  
  # Number of observations
  N <- length(y)
  N_rec <- N-H
  
  # Full-sample estimator
  gamma_N <- sapply(1:H, function(h){ 1/N*sum(sapply(1:(N-abs(h)), function(k) (y[k] - mean(y))*(y[k+abs(h)] - mean(y))))})
    
  
  # Recursive estimation
  W_N <- lapply(1:N_rec, function(tt){
    
    y_rec <- y[1:(tt+H)]
    ybar_rec <- mean(y_rec)
    
    # Estimate autocovariance recursively
    gamma_rec <- sapply(1:H, function(h){ 1/(tt+H)*sum(sapply(1:(tt+H-abs(h)), function(k) (y[k] - ybar_rec)*(y[k+abs(h)] - ybar_rec)))})
    
    s_t <- tt*(gamma_rec - gamma_N)
    
    s_t %*% t(s_t)

  })
  W_N <- 1/(N_rec^2)*Reduce("+", W_N)
  
  # Test statistic (Divide by H, so that we can use critical values of KVB)
  stat <- (N_rec*t(gamma_N) %*% solve(W_N) %*% gamma_N)/H
  
  # Choose critical value from KVB
  if(alpha == 0.1){
    
    if(H == 1){ cval <- 28.88}
    
    if(H == 10){cval <- 83.84}
    
    if(H == 25){cval <- 161.8}
  }
  
  rej <- stat > cval
  

  
  
  
}