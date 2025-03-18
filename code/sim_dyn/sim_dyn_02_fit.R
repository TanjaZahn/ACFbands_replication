# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Simulations for dynamic regressions - Simulate y and fit AR(1) regression

#Set Up
source("code/sim_dyn/sim_dyn_01_setup.R")

# Simulate y and fit AR(1) regression -------------------------------------------

lapply(1:nrow(params), function(p){ # iterate over parameters
  
  # Print the iteration step
  print(paste0(format(Sys.time(), "%H:%M")," :     p = ", p))
  
  # Parameters
  N <- params[p, "N"]
  H <- params[p, "H"]
  phi1 <- params[p, "phi1"]
  phi2 <- params[p, "phi2"]
  
  # Set seed
  set.seed(params[p, "seed1"])
  
  # Name the file
  filename <- paste0(path_results, "dyn_fit_", params[p, "id"], ".RData")
  
  # Check if file exists
  if(!file.exists(filename)){
    
    # Simulate y and fit AR(1) model  
    fit_r <- replicate(R_mc, sim_ar2_fit(N = N, I = I, a = a, phi1 = phi1, phi2 = phi2), simplify = "array")
    
    # Save
    save(fit_r, file = filename)
    
  }
  
})