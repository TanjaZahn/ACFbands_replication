# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Simulations for dynamic regressions - Naive bands

#Set Up
source("code/sim_dyn/sim_dyn_01_setup.R")

# Estimate Naive Bands ---------------------------------------------------------

lapply(type_vec, function(type){ # iterate over band types
  
  lapply(1:nrow(params), function(p){ # iterate over parameters
    
    # Print the iteration step
    print(paste0(format(Sys.time(), "%H:%M")," :     p = ", p, " ; type = ", type))
    
    # Parameters
    N <- params[p, "N"]
    H <- params[p, "H"]
    
    # Load fitted regression
    load(paste0(path_results, "dyn_fit_", params[p, "id"], ".RData"))
    
    # Set seed
    set.seed(params[p, "seed2"])
    
    # Filename
    filename <- paste0(path_results, "naive_bands_", type, "_", params[p, "id"], ".RData")
    
    # Estimate Inference Bands (if it doesn't exist yet)
    if(!file.exists(filename)){
      
      estimates_r <- lapply(1:R_mc, function(r){
        
        # Construct significance bands for residual autocorrelations
        acf_sigbands(y = fit_r[ ,r]$residuals, H = H, type = type,  alpha = alpha, plot = FALSE)
        
      })
      
      # Save results
      save(estimates_r, file = filename)
      
    }
    
  })
  
  
  
  
})



