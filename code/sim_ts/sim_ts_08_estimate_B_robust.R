# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Simulations for time series with Chi Square GARCH errors - Estimate robust covariance matrices.

#Set Up 
source("code/sim_ts/sim_ts_07_setup_robust.R")

library(future.apply)
plan(multisession, workers = 12) # number of cores

# Estimate B (robust) ----------------------------------------------------------

covar <- "robust"

lapply(bw_vec, function(bw){ # iterate over bandwidths

  lapply(1:nrow(params), function(p){ # iterate over parameters

    # Print the iteration step
    print(paste0(format(Sys.time(), "%H:%M")," :     bw = ", bw, "; p = ", p))

    # Load y
    load(paste0(path_results, dgp_name, params[p, "id"], "_y.RData"))

    # Filename
    filename <- paste0(path_results, dgp_name, params[p, "id"], "_B_hat_", covar, "_", bw, ".RData")

    # Estimate covariance formula (if it doesn't exist yet)
    if(!file.exists(filename)){

      B_hat_r <- future_lapply(1:R_mc, function(r)
        covar_robust(y = y_r[, r], H = params[p, "H"],  L = params[p, bw], covar = covar))

      # Save results
      save(B_hat_r, file = filename)

     }


  })
})


# Estimate B (MDS: no bandwidth) ------------------------------------------------

covar <- "MDS"

lapply(1:nrow(params), function(p){ # iterate over parameters
      
      # Print the iteration step
      print(paste0(format(Sys.time(), "%H:%M")," :     p = ", p))
      
      # Load y
      load(paste0(path_results, dgp_name, params[p, "id"], "_y.RData"))
      
      # Filename
      filename <- paste0(path_results, dgp_name, params[p, "id"], "_B_hat_", covar, ".RData")
      
      # Estimate covariance formula (if it doesn't exist yet)
      if(!file.exists(filename)){
        
        B_hat_r <- future_lapply(1:R_mc, function(r)
          covar_robust(y = y_r[, r], H = params[p, "H"], covar = covar))
        
        # Save results
        save(B_hat_r, file = filename)
        
      }
      
      
    })










