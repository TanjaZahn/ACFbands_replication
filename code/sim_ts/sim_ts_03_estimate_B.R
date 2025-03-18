# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Simulations for time series - Estimate the covariance matrix using the Bartlett formula.

#Set Up 
source("code/sim_ts/sim_ts_01_setup.R")

# Estimate B --------------------------------------------------------------------

lapply(bw_vec, function(bw){ # iterate over bandwidths
  
  lapply(1:nrow(params), function(p){ # iterate over parameters
    
    # Print the iteration step
    print(paste0(format(Sys.time(), "%H:%M")," :     bw = ", bw, "; p = ", p))
    
    # Load y
    load(paste0(path_results, dgp_name, params[p, "id"], "_y.RData"))
    
    # Filename
    filename <- paste0(path_results, dgp_name, params[p, "id"], "_B_hat_", bw, ".RData")

    # Estimate covariance formula (if it doesn't exist yet)
    if(!file.exists(filename)){
      
      B_hat_r <- lapply(1:R_mc, function(r)
        covar_bartlett(y = y_r[, r], H = params[p, "H"],  L = params[p, bw]))
      
      # Save results
      save(B_hat_r, file = filename)
      
    }
    
    
  })
  
  
  
})





