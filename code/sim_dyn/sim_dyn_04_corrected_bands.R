# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Simulations for dynamic regressions - Corrected bands


#Set Up
source("code/sim_dyn/sim_dyn_01_setup.R")

# Estimate the covariance matrix -----------------------------------------------

lapply(covar_vec, function(covar){ # iterate over covariance types
  
    lapply(1:nrow(params), function(p){ # iterate over parameters
      
      # Print the iteration step
      print(paste0(format(Sys.time(), "%H:%M")," :     p = ", p, " ; covar = ", covar))
      
      # Load fitted regression
      load(paste0(path_results, dgp_name , "_fit_", params[p, "id"], ".RData"))
      
      # Filename
      filename <- paste0(path_results, dgp_name , "_covmat_", covar, "_", params[p, "id"], ".RData")
      
      # Estimate covariance matrix
      if(!file.exists(filename)){
        
        covmat_r <- lapply(1:R_mc, function(r){covar_dynamic(fit = fit_r[ ,r], H = params[p, "H"], covar = covar)})
        
        # Save results 
        save(covmat_r, file = filename)
        
      }
      
    })
})



# Estimate classical bands -----------------------------------------------------

covar <- "classical"

lapply(type_vec, function(type){ # iterate over band types
    
    lapply(1:nrow(params), function(p){ # iterate over parameters
      
      # Print the iteration step
      print(paste0(format(Sys.time(), "%H:%M")," :     p = ", p, " ; covar = ", covar,  " ; type = ", type))
      
      # Load fitted regression
      load(paste0(path_results, dgp_name, "_fit_", params[p, "id"], ".RData"))
      
      # Load covariance matrix
      load(paste0(path_results, dgp_name , "_covmat_", covar, "_", params[p, "id"], ".RData"))
      
      # Filename
      filename <- paste0(path_results, dgp_name , "_bands_", type, "_", covar, "_", params[p, "id"], ".RData")
      
      # Set seed
      set.seed(params[p, "seed2"])
      
      # Estimate Inference Bands (if it doesn't exist yet)
      if(!file.exists(filename)){
        
        estimates_r <- lapply(1:R_mc, function(r){ 
          
          acf_sigbands_dyn(fit = fit_r[ ,r], H = params[p, "H"], type = type ,  
                           covar = covar, Sigma_rho_hat = covmat_r[[r]],
                           alpha = alpha, plot = FALSE)
          })
        
        # Save results 
        save(estimates_r, file = filename)
        
      }
      
    })
    
  })
  



# Estimate robust bands (only Bonferroni) -----------------------


covar <- "robust"

lapply(type_vec_robust, function(type){ # iterate over band types
  
  lapply(1:nrow(params), function(p){ # iterate over parameters
    
    # Print the iteration step
    print(paste0(format(Sys.time(), "%H:%M")," :     p = ", p, " ; covar = ", covar,  " ; type = ", type))
    
    # Load fitted regression
    load(paste0(path_results, dgp_name, "_fit_", params[p, "id"], ".RData"))
    
    # Load covariance matrix
    load(paste0(path_results, dgp_name , "_covmat_", covar, "_", params[p, "id"], ".RData"))
    
    # Filename
    filename <- paste0(path_results, dgp_name , "_bands_", type, "_", covar, "_", params[p, "id"], ".RData")
    
    # Set seed
    set.seed(params[p, "seed2"])
    
    # Estimate Inference Bands (if it doesn't exist yet)
    if(!file.exists(filename)){
      
      estimates_r <- lapply(1:R_mc, function(r){ 
        
        acf_sigbands_dyn(fit = fit_r[ ,r], H = params[p, "H"], type = type ,  
                         covar = covar, Sigma_rho_hat = covmat_r[[r]],
                         alpha = alpha, plot = FALSE)
      })
      
      # Save results 
      save(estimates_r, file = filename)
      
    }
    
  })
  
})







