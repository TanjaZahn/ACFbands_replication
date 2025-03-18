# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Simulations for dynamic regressions - Dynamic bands


###### This file generates pointwise and simultaneous significance bands allowing for lagged endogeneous regressors, 
####### assuming conditional homoskedastiticy.

#Set Up
source("code/sim_dyn/sim_dyn_01_setup.R")

# Estimate the covariance matrix -----------------------------------------------

lapply(error_vec, function(error){ # iterate over error types
  
    lapply(1:nrow(params), function(p){ # iterate over parameters
      
      # Print the iteration step
      print(paste0(format(Sys.time(), "%H:%M")," :     p = ", p, " ; error = ", error))
      
      # Load fitted regression
      load(paste0(path_results, "dyn_fit_", params[p, "id"], ".RData"))
      
      # Filename
      filename <- paste0(path_results, "dyn_covmat_", error, "_", params[p, "id"], ".RData")
      
      # Estimate covariance matrix
      if(!file.exists(filename)){
        
        covmat_r <- lapply(1:R_mc, function(r){covar_dynamic(fit = fit_r[ ,r], H = params[p, "H"], error = error)})
        
        # Save results 
        save(covmat_r, file = filename)
        
      }
      
    })
})



# Estimate Bands -------------------------------------

lapply(error_vec, function(error){ # iterate over error types
  
  lapply(type_vec, function(type){ # iterate over band types
    
    lapply(1:nrow(params), function(p){ # iterate over parameters
      
      # Print the iteration step
      print(paste0(format(Sys.time(), "%H:%M")," :     p = ", p, " ; error = ", error,  " ; type = ", type))
      
      # Load fitted regression
      load(paste0(path_results, "dyn_fit_", params[p, "id"], ".RData"))
      
      # Load covariance matrix
      load(paste0(path_results, "dyn_covmat_", error, "_", params[p, "id"], ".RData"))
      
      # Filename
      filename <- paste0(path_results, "dyn_bands_", type, "_", error, "_", params[p, "id"], ".RData")
      
      # Set seed
      set.seed(params[p, "seed2"])
      
      # Estimate Inference Bands (if it doesn't exist yet)
      if(!file.exists(filename)){
        
        estimates_r <- lapply(1:R_mc, function(r){ 
          
          acf_sigbands_dyn(fit = fit_r[ ,r], H = params[p, "H"], type = type ,  
                           error = error, Sigma_rho_hat = covmat_r[[r]],
                           alpha = alpha, plot = FALSE)
          })
        
        # Save results 
        save(estimates_r, file = filename)
        
      }
      
    })
    
  })
  
})



