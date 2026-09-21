# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Simulations for time series with Chi Square GARCH errors - Construct robust confidence bands.


#Set Up 
source("code/sim_ts_garch_chi2/sim_ts_garch_chi2_07_setup_robust.R")

# Select robust covariance matrix
covar <- "robust"

# Generate Bonferroni Confidence Bands ------------------------------------------

band_type <- "bonferroni"

lapply(bw_vec, function(bw){ # iterate over bandwidths
      
      lapply(1:nrow(params), function(p){ # iterate over parameters
        
        # Print the iteration step
        print(paste0(format(Sys.time(), "%H:%M")," :     bw = ", bw, "; band_type = ", band_type, "; p = ", p))
        
        # Load y
        load(paste0(path_results, dgp_name, params[p, "id"], "_y.RData"))
        
        # Load B_hat
        load(paste0(path_results, dgp_name, params[p, "id"], "_B_hat_", covar, "_", bw, ".RData"))
        
        # Set seed
        set.seed(params[p, "seed2"])
        
        # Filename (robust)
        filename <- paste0(path_results, dgp_name, params[p, "id"], "_conf_", band_type, "_", covar, "_", bw, ".RData")
        
        # Estimate Inference Bands (if it doesn't exist yet)
        if(!file.exists(filename)){
          
          estimates_r <- lapply(1:R_mc, function(r){
            
            # robust
            out <- try(acf_confbands(y = y_r[ , r], H = params[p, "H"], type = band_type, B_hat = B_hat_r[[r]],
                                     covar = covar, L =  params[p, bw], alpha = alpha, plot = FALSE))
            
            # If there is an error, save as NA
            if("try-error" %in% class(out)){
              
              if(band_type == "sup-t"){
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




# Generate all confidence Bands (N-H) for one bandwidth ------------------------

bw <- "L2" # T^(1/3)

lapply(conf_types, function(band_type){ # iterate over band_type
  
    lapply(1:nrow(params), function(p){ # iterate over parameters
      
      # Print the iteration step
      print(paste0(format(Sys.time(), "%H:%M")," :     bw = ", bw, "; band_type = ", band_type, "; p = ", p))
      
      # Load y
      load(paste0(path_results, dgp_name, params[p, "id"], "_y.RData"))
      
      # Load B_hat
      load(paste0(path_results, dgp_name, params[p, "id"], "_B_hat_", covar, "_", bw, ".RData"))
      
      # Set seed
      set.seed(params[p, "seed2"])
      
      # Filename (robust)
      filename <- paste0(path_results, dgp_name, params[p, "id"], "_conf_", band_type, "_", covar, "_", bw, ".RData")
      
      # Estimate Inference Bands (if it doesn't exist yet)
      if(!file.exists(filename)){
        
        estimates_r <- lapply(1:R_mc, function(r){
          
          # robust
          out <- try(acf_confbands(y = y_r[ , r], H = params[p, "H"], type = band_type, B_hat = B_hat_r[[r]],
                                               covar = covar, L =  params[p, bw], alpha = alpha, plot = FALSE))
          
          # If there is an error, save as NA
          if("try-error" %in% class(out)){
            
            if(band_type == "sup-t"){
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













