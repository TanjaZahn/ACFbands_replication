#' @title DGP: AR(1)-GARCH
#'
#' @description Generate data according to an AR(1) process with GARCH errors.
#'
#'
#' @param N sample size
#' @param I length of burn-in period. Default is 50.
#' @param phi slope coefficient of AR(1) process.
#' @param a0 GARCH parameter. Set to 0.001.
#' @param a1 GARCH parameter. Set to 0.05.
#' @param b1 GARCH parameter. Set to 0.9.
#' 
#'
#' @return vector of length `N` containing observations generated from an AR(1) process
#' 
#' 


sim_ar1_y_garch <- function(N , I = 50, phi, a0 = 0.001, a1 = 0.05, b1 = 0.9){
  
  # Draw normal error
  eta <- rnorm(n = N + I, mean = 0, sd = 1)

  # Initialization
  y  <- eta[1]
  sigma2 <- eta[1]^2
  
  # GARCH error
  for(tt in 2:(N + I)) sigma2[tt] <- a0 +a1*(eta[tt-1])^2*sigma2[tt-1] + b1*sigma2[tt-1]
  epsilon <-  eta*sqrt(sigma2)
     
  # AR model
  for(tt in 2:(N + I)) y[tt] <- phi*y[tt-1] + epsilon[tt]
  
  # Disregard burn-in observations
  y <- y[-(1:I)]
  
}