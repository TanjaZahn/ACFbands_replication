# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Simulations for time series with Chi Square GARCH errors - Calculate frequency of rejections and create data frames.

source("code/sim_ts_garch_chi2/sim_ts_garch_chi2_01_setup.R")
library(tidyverse)


# Calculate frequency of rejection for the significance bands ------------------

df_sigbands  <- bind_rows(lapply(sig_types, function(band_type){ # iterate over band_type
  
  bind_rows(lapply(1:nrow(params), function(p){ # iterate over parameters
    
    calc_rej(filename = paste0(path_results, dgp_name, params[p, "id"], "_sig_", band_type, ".RData"),
             add_params = params[p, c("H", "phi", "N")])
    
  }))
  
}), .id = "type") %>% 
  mutate(type = str_replace_all(type, label_sig)) # use labels
save(df_sigbands, file = paste0(path_results, dgp_name, "_df_sigbands.RData"))


# Load frequency of rejection for the tests -------------------------------

df_tests <- bind_rows(lapply(test_vec, function(test){ # iterate over test_type
  
  bind_rows(lapply(1:nrow(params), function(p){ # iterate over parameters
    
    # print(test, p)
    
    # Load estimated inference bands
    load(paste0(path_results, dgp_name, params[p, "id"], "_sig_", test, ".RData"))
    
    # Take the mean over R_mc simulations and add parameter values
    cbind(rej = mean(do.call(c, estimates_r)), params[p,  c("H", "phi", "N")])
    
  }))
}), .id = "type") %>% 
  mutate(type = str_replace_all(type, label_tests)) # use labels
save(df_tests, file = paste0(path_results, dgp_name, "_df_tests.RData"))


# Calculate rejection of null hypothesis for the confidence bands --------------

df_confbands <- bind_rows(lapply(conf_types, function(type){ # iterate over band_type
  
  bind_rows(lapply(bw_vec, function(bw){ # iterate over bandwidths
    
    bind_rows(lapply(1:nrow(params), function(p){ # iterate over parameters
      
      # Load estimated inference bands
      load(paste0(path_results, dgp_name, params[p, "id"], "_conf_", type, "_", bw, ".RData"))
      
      # Calculate frequencies over replications
      mat_r <- sapply(1:R_mc, function(r){
        
        conf_band <- estimates_r[[r]]$conf_band
        
        # Hypothesis test: Reject if zero is not included
        rej_h <- (0 < conf_band[ , "lb"]|  0 > conf_band[ , "ub"])
        
        # Reject if rho_hat is outside the bands for at least one h
        rej <- max(rej_h)
        
        # Calculate true rho
        rho <- sapply(1:params[p, "H"], function(h) params[p, "phi"]^h)
        
        # Determine if true rho is inside the band at h
        cover_h <- (rho >= conf_band[ , "lb"] & rho <= conf_band[ , "ub"])
        
        # Decision: Determine if the entire path of rho is inside CB
        cover <- min(cover_h)
        
        # Calculate the average width
        avg_width <- mean(conf_band[ , "width"])
        
        # Combine results
        c(rej = rej, cover = cover, avg_width = avg_width)
      })
      
      # Take the mean over R_mc simulations and add parameter values
      out <- cbind(t(rowMeans(mat_r)), params[p, c("H", "phi", "N")])
      
    }))
    
  }), .id = "L")
  
}), .id = "type") %>% 
  mutate(type = str_replace_all(type, label_conf)) # use labels
save(df_confbands, file = paste0(path_results, dgp_name, "_df_confbands.RData"))

