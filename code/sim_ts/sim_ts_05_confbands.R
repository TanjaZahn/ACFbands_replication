# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Simulations for time series - Construct confidence bands.


#Set Up 
source("code/sim_ts/sim_ts_01_setup.R")


# Generate Confidence Bands ----------------------------------------------------


lapply(conf_types, function(band_type){ # iterate over band_type
  
  lapply(bw_vec, function(bw){ # iterate over bandwidths
    
    lapply(1:nrow(params), function(p){ # iterate over parameters
      
      # Print the iteration step
      print(paste0(format(Sys.time(), "%H:%M")," :     bw = ", bw, "; band_type = ", band_type, "; p = ", p))
      
      # Load y
      load(paste0(path_results, dgp_name, params[p, "id"], "_y.RData"))
      
      # Load B_hat
      load(paste0(path_results, dgp_name, params[p, "id"], "_B_hat_", bw, ".RData"))
      
      # Set seed
      set.seed(params[p, "seed2"])
      
      # Filename
      filename <- paste0(path_results, dgp_name, params[p, "id"], "_conf_", band_type, "_", bw, ".RData")
      
      # Estimate Inference Bands (if it doesn't exist yet)
      if(!file.exists(filename)){
        
        estimates_r <- lapply(1:R_mc, function(r){
          
          out <- try(acf_confbands(y = y_r[ , r], H = params[p, "H"], type = band_type, B_hat = B_hat_r[[r]], 
                     L =  params[p, bw], alpha = alpha, plot = FALSE))
          
          # If there is an error, save as NA
          if("try-error" %in% class(out)){
            
            if(band_type == "sup_t"){ 
              out <- list(rho_hat = NA, 
                          conf_band = cbind(lb = NA, ub = NA, width = NA),
                          B_hat = NA) }
            
            if(band_type %in% c("bonferroni", "pointwise")){ 
              out <- list(rho_hat = NA, 
                          conf_band = cbind(lb = NA, ub = NA, width = NA),
                          B_hat_diag = NA)}
    
          }
          
          out
          
        })
        
        # Save results
        save(estimates_r, file = filename)
        
      }
      
    })
    
    
  })
  
})







