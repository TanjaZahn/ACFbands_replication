# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Simulations for time series - Construct classical significance bands and run alternative hypotheses tests.

#Set Up 
source("code/sim_ts/sim_ts_01_setup.R")

# Simultaneous Significance Bands -------------------------------------------------

lapply(sig_types, function(band_type){ # iterate over band_types
  
  lapply(1:nrow(params), function(p){ # iterate over parameters
    
    # Print the iteration step
    print(paste0(format(Sys.time(), "%H:%M")," :     p = ", p))
    
    # Load y
    load(paste0(path_results, dgp_name, params[p, "id"], "_y.RData"))
    
    # Set seed
    set.seed(params[p, "seed2"])
    
    # Filename
    filename <- paste0(path_results, dgp_name, params[p, "id"], "_sig_", band_type, ".RData")
    
    # Estimate Inference Bands (if it doesn't exist yet)
    if(file.exists(filename) == FALSE){
      
      estimates_r <- lapply(1:R_mc, function(r){
        
        acf_sigbands(y = y_r[ , r], H = params[p, "H"], type = band_type, covar = "iid", alpha = alpha, plot = FALSE)
        
      })
      
      # Save results
      save(estimates_r, file = filename)
      
    }
    
  })
  
})


# Box Pierce Test ---------------------------------------------------------------

test <- "boxpierce"

lapply(1:nrow(params), function(p){ # iterate over parameters
  
  # Print the iteration step
  print(paste0(format(Sys.time(), "%H:%M")," :     p = ", p))
  
  # Parameters
  N <- params[p, "N"]
  H <- params[p, "H"]
  
  # Load y
  load(paste0(path_results, dgp_name, params[p, "id"], "_y.RData"))
  
  # Set seed
  set.seed(params[p, "seed2"])
  
  # Filename
  filename <- paste0(path_results, dgp_name, params[p, "id"], 
                     "_sig_", test, ".RData")
  
  # Estimate Inference Bands (if it doesn't exist yet)
  if(file.exists(filename) == FALSE){
    
    estimates_r <- lapply(1:R_mc, function(r){
      
      ##### Estimate autocorrelation
      rho_hat <- autocor(y =  y_r[ , r], H = H)
      
      #### Box-Pierce Test
      Q_stat <- N*sum(rho_hat^2)
      
      #### Test Decision
      rej <- (Q_stat > qchisq(p = (1-alpha), df = H))
    
      ### Output
      rej
      
    })
    
    # Save results
    save(estimates_r, file = filename)
    
  }
  
})


# Ljung-Box Test ---------------------------------------------------------------

test <- "ljungbox"

lapply(1:nrow(params), function(p){ # iterate over parameters
  
  # Print the iteration step
  print(paste0(format(Sys.time(), "%H:%M")," :     p = ", p))
  
  # Parameters
  N <- params[p, "N"]
  coef <- params[p, "coef"]
  H <- params[p, "H"]
  
  # Load y
  load(paste0(path_results, dgp_name, params[p, "id"], "_y.RData"))
  
  # Set seed
  set.seed(params[p, "seed2"])
  
  # Filename
  filename <- paste0(path_results, dgp_name, params[p, "id"], 
                     "_sig_", test, ".RData")
  
  # Estimate Inference Bands (if it doesn't exist yet)
  if(file.exists(filename) == FALSE){
    
    estimates_r <- lapply(1:R_mc, function(r){
      
      ##### Estimate autocorrelation
      rho_hat <- autocor(y =  y_r[ , r], H = H)
      
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


# Shao (2010) -------------------------------------------------------------------

test <-  "shao"

lapply(1:nrow(params), function(p){ # iterate over parameters

  # Print the iteration step
  print(paste0(format(Sys.time(), "%H:%M")," :     p = ", p))

  # Parameters
  N <- params[p, "N"]
  coef <- params[p, "coef"]
  H <- params[p, "H"]

  # Load y
  load(paste0(path_results, dgp_name, params[p, "id"], "_y.RData"))

  # Set seed
  set.seed(params[p, "seed2"])
  
  # Filename
  filename <- paste0(path_results, dgp_name, params[p, "id"],
                     "_sig_", test, ".RData")

  # Estimate Inference Bands (if it doesn't exist yet)
  if(file.exists(filename) == FALSE){

    estimates_r <- try(lapply(1:R_mc, function(r){ 
      
      rej <- try(shao(y = y_r[, r], H = H, alpha = alpha))
      # Check for error
      if (inherits(rej, "try-error")){rej <- NA}
      else{rej <- as.numeric(rej)}
      rej
      }))
    

    # Save results
    save(estimates_r, file = filename)

  }

})

# Robust Box-Pierce Test --------------------------------------------------------


test <- "bp-robust"

lapply(1:nrow(params), function(p){ # iterate over parameters
  
  # Print the iteration step
  print(paste0(format(Sys.time(), "%H:%M")," :     p = ", p))
  
  # Parameters
  N <- params[p, "N"]
  H <- params[p, "H"]
  
  # Load y
  load(paste0(path_results, dgp_name, params[p, "id"], "_y.RData"))
  
  # Set seed
  set.seed(params[p, "seed2"])
  
  # Filename
  filename <- paste0(path_results, dgp_name, params[p, "id"], 
                     "_sig_", test, ".RData")
  
  # Estimate Inference Bands (if it doesn't exist yet)
  if(file.exists(filename) == FALSE){
    
    estimates_r <- lapply(1:R_mc, function(r){
      
      ##### Estimate autocorrelation
      rho_hat <- autocor(y =  y_r[ , r], H = H)
      
      #### Estimate covariance matrix
      B_hat <- covar_robust(y = y_r[, r], H = params[p, "H"], covar = "MDS")
      
      #### Box-Pierce Test
      Q_stat <- (N-H)*t(rho_hat) %*% solve(B_hat) %*% rho_hat
      
      #### Test Decision
      rej <- (Q_stat > qchisq(p = (1-alpha), df = H))
      
      ### Output
      rej
      
    })
    
    # Save results
    save(estimates_r, file = filename)
    
  }
  
})
















