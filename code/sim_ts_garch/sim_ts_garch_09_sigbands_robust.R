# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Simulations for time series with GARCH errors - Construct robust significance bands.

#Set Up 
source("code/sim_ts_garch/sim_ts_garch_07_setup_robust.R")

# Robust Simultaneous Significance Bands ----------------------------------------

covar <- "MDS"
band_type <- "bonferroni"

# lapply(sig_types, function(band_type){ # iterate over band_types
  
    lapply(1:nrow(params), function(p){ # iterate over parameters
      
      # Print the iteration step
      print(paste0(format(Sys.time(), "%H:%M")," :   type = ", band_type, "; covar = ", covar, ";  p = ", p))
      
      # Load y
      load(paste0(path_results, dgp_name, params[p, "id"], "_y.RData"))
      
      # Set seed
      set.seed(params[p, "seed2"])
      
      # Estimate Inference Bands (if it doesn't exist yet)
      filename <- paste0(path_results, dgp_name, params[p, "id"], "_sig_", covar, "_", band_type, ".RData")
      if(file.exists(filename) == FALSE){
        
        estimates_r <- lapply(1:R_mc, function(r){
          
          acf_sigbands(y = y_r[ , r], covar = covar, 
                       H = params[p, "H"], type = band_type,  alpha = alpha, plot = FALSE)
          
        })
        
        # Save results
        save(estimates_r, file = filename)
        
      }
      
    })
# })

