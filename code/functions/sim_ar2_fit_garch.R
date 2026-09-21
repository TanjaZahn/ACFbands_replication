#' @title Estimate AR(1) model on data from AR(2) DGP with GARCH errors.
#'
#' @description Generate data according to an AR(2) process and fit an AR(1) model to it.
#'
#'
#' @param N sample size
#' @param I length of burn-in period. Default is 50.
#' @param a intercept
#' @param phi1 coefficient of lag 1
#' @param phi2 coefficient of lag 2
#' @param a0 GARCH parameter. Set to 0.001.
#' @param a1 GARCH parameter. Set to 0.05.
#' @param b1 GARCH parameter. Set to 0.9.
#'
#' @return a fitted model object
#' 
#' 

sim_ar2_fit_garch <- function(N , I = 50, a, phi1, phi2, a0 = 0.001, a1 = 0.05, b1 = 0.9){
  
  # Draw normal error
  eta <- rnorm(n = N + I, mean = 0, sd = 1)
  
  # Initialization
  y  <- eta[1:2]
  sigma2 <- eta[1]^2
  
  # GARCH error
  for(tt in 2:(N + I)) sigma2[tt] <- a0 +a1*(eta[tt-1])^2*sigma2[tt-1] + b1*sigma2[tt-1]
  epsilon <-  eta*sqrt(sigma2)
  
  # AR(2) model
  for(tt in 3:(N + I)) y[tt] <- a + phi1*y[tt-1] + phi2*y[tt-2] + epsilon[tt]
  
  # Collect results in a data frame
  df <- data.frame(y = y, L1_y = dplyr::lag(y, 1))
  
  # Disregard burn-in observations
  df <- df[-(1:I), ]
  
  # Fit an AR(1) model to the data
  fit <- lm(y ~ L1_y, df)
  
}