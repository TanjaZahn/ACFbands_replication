# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Simulations for dynamic regressions with GARCH errors - Other hypothesis tests

#Set Up
source("code/sim_dyn_garch/sim_dyn_garch_01_setup.R")

# Ljung-Box Test ---------------------------------------------------------------

test <- "ljungbox"

lapply(1:nrow(params), function(p){ # iterate over parameters
  
  # Print the iteration step
  print(paste0(format(Sys.time(), "%H:%M")," :     p = ", p))
  
  # Parameters
  N <- params[p, "N"]
  H <- params[p, "H"]
  
  # Load fitted regression
  load(paste0(path_results, dgp_name, "_fit_", params[p, "id"], ".RData"))
  
  # Set seed
  set.seed(params[p, "seed2"])
  
  # Filename
  filename <- paste0(path_results, dgp_name, "_", test, "_", params[p, "id"], ".RData")
  
  # Estimate Inference Bands (if it doesn't exist yet)
  if(!file.exists(filename)){
    
    estimates_r <- lapply(1:R_mc, function(r){
      
      # Select the fit
      fit <- fit_r[ ,r]
      
      # Extract residuals
      res <- fit$residuals
      
      ##### Estimate autocorrelation
      rho_hat <- autocor(y =  res, H = H)
      
      #### Ljung–Box Test
      Q_stat <- N*(N+2)*sum(sapply(1:H, function(h) rho_hat[h]^2/(N-h)))
      
      #### Test Decision
      rej <- (Q_stat > qchisq(p = (1-alpha), df = H))
      
      ### Output
      rej
      
    })
    
    # Save results
    save(estimates_r, file = filename)
    
  }
  
})

# Ljung-Box Test (width adjusted degrees of freedom for residuals) --------------

test <- "ljungbox_res"

lapply(1:nrow(params), function(p){ # iterate over parameters
  
  # Print the iteration step
  print(paste0(format(Sys.time(), "%H:%M")," :     p = ", p))
  
  # Parameters
  N <- params[p, "N"]
  H <- params[p, "H"]
  
  # Load fitted regression
  load(paste0(path_results, dgp_name, "_fit_", params[p, "id"], ".RData"))
  
  # Set seed
  set.seed(params[p, "seed2"])
  
  # Filename
  filename <- paste0(path_results, dgp_name, "_", test, "_", params[p, "id"], ".RData")
  
  
  # Estimate Inference Bands (if it doesn't exist yet)
  if(!file.exists(filename)){
    
    if(H == 1){
      estimates_r <- lapply(1:R_mc, function(r){rej <- NA})}
    
    if(H != 1){
      
      estimates_r <- lapply(1:R_mc, function(r){
        
        # Select the fit
        fit <- fit_r[ ,r]
        
        # Extract residuals
        res <- fit$residuals
        
        ##### Estimate autocorrelation
        rho_hat <- autocor(y =  res, H = H)
        
        #### Ljung–Box Test
        Q_stat <- N*(N+2)*sum(sapply(1:H, function(h) rho_hat[h]^2/(N-h)))
        
        #### Test Decision
        rej <- (Q_stat > qchisq(p = (1-alpha), df = (H-1)))
        
        ### Output
        rej
        
      })
      
    }
    
    
    
    # Save results
    save(estimates_r, file = filename)
    
  }
  
})



# Breusch-Godfrey --------------------------------------------------------------

test <- "BG"

lapply(1:nrow(params), function(p){ # iterate over parameters
  
  # Print the iteration step
  print(paste0(format(Sys.time(), "%H:%M")," :     p = ", p))
  
  # Parameters
  N <- params[p, "N"]
  H <- params[p, "H"]
  
  # Load fitted regression
  load(paste0(path_results, dgp_name, "_fit_", params[p, "id"], ".RData"))
  
  # Filename
  filename <- paste0(path_results, dgp_name, "_", test, "_", params[p, "id"], ".RData")
  
  # Set seed
  set.seed(params[p, "seed2"])
  
  # Estimate Inference Bands (if it doesn't exist yet)
  if(!file.exists(filename)){
    
    estimates_r <- lapply(1:R_mc, function(r){
      
      # Select the fit
      fit <- fit_r[ ,r]
      
      # Extract residuals
      res <- fit$residuals
      
      ##### Estimate autocorrelation
      rho_hat <- autocor(y =  res, H = H)
      
      # Number of regressors
      K <- ncol(fit$model) - 1
      
      # Extract the regressors
      X <- matrix(NA, nrow = N, ncol = K)
      for(kk in 1:K){ X[ , kk] <- fit$model[ , (kk+1)]}
      colnames(X) <- paste0("x", 1:K)
      
      # Lagged residuals
      lagged_res <- matrix(NA, ncol = H, nrow = N)
      for(h in 1:H){
        for(tt in (H+1):N){
          
          lagged_res[tt, h] <- res[tt-h]
        }
        
      }
        
      # Auxiliary data frame
      df_aux <- data.frame(res, X, lagged_res)
      df_aux <- df_aux[-(1:H), ] # sample starts in H+1
      
      # Names of all variables
      xnames <- colnames(df_aux[ , -1])
      
      # Create regression formula
      regformula <- formula(paste0("res ~ ", paste(xnames, collapse = "+")))
      
      # Get R^2 from Auxiliary regression
      rsquared <- summary(lm(regformula, df_aux))$r.squared
      
      # Test statistic
      stat <- (N - H)*rsquared
      
      # Test decision
      rej <- (stat > qchisq(p = (1-alpha), df = H))
      
    })
    
    # Save results
    save(estimates_r, file = filename)
    
  }
  
})



# Robust Box-Pierce Test --------------------------------------------------------


test <- "bp_robust"
covar <- "robust"

lapply(1:nrow(params), function(p){ # iterate over parameters
  
  # Print the iteration step
  print(paste0(format(Sys.time(), "%H:%M")," :     p = ", p))
  
  # Parameters
  N <- params[p, "N"]
  H <- params[p, "H"]
  
  # Load fitted regression
  load(paste0(path_results, dgp_name, "_fit_", params[p, "id"], ".RData"))
  
  # Load covariance matrix
  load(paste0(path_results, dgp_name , "_covmat_", covar, "_", params[p, "id"], ".RData"))
  
  estimates_r <- lapply(1:R_mc, function(r){
    
    # Fit
    fit <- fit_r[ ,r]
    
    # Extract residuals
    res <- fit$residuals
    
    # Estimate the autocorrelation of residuals
    rho_hat <- autocor(y = res, H = H)
    
    #### Estimate covariance matrix
    Sigma_rho <- covmat_r[[r]]$estimate
    
    #### Box-Pierce Test
    Q_stat <- (N-H)*t(rho_hat) %*% solve(Sigma_rho) %*% rho_hat
    
    #### Test Decision
    rej <- (Q_stat > qchisq(p = (1-alpha), df = H))
    
    ### Output
    rej
    
  })
  
  # Save results
  save(estimates_r, file = paste0(path_results, dgp_name, "_", test, "_", params[p, "id"], ".RData"))
  
  
  
  
})


