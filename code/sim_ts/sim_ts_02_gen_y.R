# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Simulations for time series - Draw the time series using an AR(1) process.

#Set Up 
source("code/sim_ts/sim_ts_01_setup.R")


# Simulate y -------------------------------------------------------------------

lapply(1:nrow(params), function(p){ # iterate over parameters
  
  # Parameters
  N <- params[p, "N"]
  phi <- params[p, "phi"]
  H <- params[p, "H"]
  
  # Set seed
  set.seed(params[p, "seed1"])
  
  # Simulation
  y_r <- replicate(R_mc, sim_ar1_y(N = N, I = I, phi = phi), simplify = "array")
  
  # Save
  save(y_r, file = paste0(path_results, dgp_name, params[p, "id"], "_y.RData"))
  
})








