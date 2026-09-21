# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Simulations for time series - Calculate frequency of rejections and create data frames for robust bands.


#Set Up 
source("code/sim_ts/sim_ts_07_setup_robust.R")
library(tidyverse)


# Significance bands ------------------------------------------------------------

df_sigbands_robust <- bind_rows(lapply(sig_types, function(band_type){ # iterate over band_types
  
  bind_rows(lapply(covar_types[2], function(covar){
    
    bind_rows(lapply(1:nrow(params), function(p){ # iterate over parameters
      
      calc_rej(filename =  paste0(path_results, dgp_name, params[p, "id"], "_sig_", covar, "_", band_type, ".RData"),
               add_params = params[p, c("H", "phi", "N")])
      
    }))
    
  }), .id = "covar")
  
}), .id = "band") %>% 
  mutate(type = paste(band, covar)) %>% 
  mutate(type = str_replace_all(type, label_sig)) # use labels
save(df_sigbands_robust, file = paste0(path_results, dgp_name, "_df_sigbands_robust.RData"))


# Bonferroni Confidence bands -------------------------------------------------------------

df_confbands_robust <- bind_rows(lapply(conf_types[2], function(band_type){ # iterate over band_type
  
  bind_rows(lapply(bw_vec, function(bw){ # iterate over bandwidths
    
    bind_rows(lapply(covar_types[1], function(covar){
      
      bind_rows(lapply(1:nrow(params), function(p){ # iterate over parameters
        
        # Load estimated inference bands
        load(paste0(path_results, dgp_name, params[p, "id"], "_conf_", band_type, "_", covar, "_", bw, ".RData"))
        
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
        
        # Frequency of NAs
        freq_nas <-  mean(is.na(mat_r[1, ]))
        
        # Take the mean over R_mc simulations and add parameter values
        out <- cbind(t(rowMeans(mat_r)), freq_nas , params[p, c("H", "phi", "N")])          
        
        
      }))
      
    }), .id = "covar")
    
  }), .id = "L")
  
}), .id = "band") %>% 
  mutate(type = paste(band, covar)) %>% 
  mutate(type = str_replace_all(type, label_conf)) # use labels
save(df_confbands_robust, file = paste0(path_results, dgp_name, "_df_confbands_robust.RData"))



# All Confidence bands for for bandwidth T^(1/3) -------------------------------


bw <- "L2" # T^(1/3)

df_confbands_robustL2 <- bind_rows(lapply(conf_types, function(band_type){ # iterate over band_type
  
  bind_rows(lapply(covar_types[1], function(covar){
    
    bind_rows(lapply(1:nrow(params), function(p){ # iterate over parameters
      
      # Load estimated inference bands
      load(paste0(path_results, dgp_name, params[p, "id"], "_conf_", band_type, "_", covar, "_", bw, ".RData"))
      
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
      
      # Frequency of NAs
      freq_nas <-  mean(is.na(mat_r[1, ]))
      
      # Take the mean over R_mc simulations and add parameter values
      out <- cbind(t(rowMeans(mat_r)), freq_nas , params[p, c("H", "phi", "N")])          
      
      
    }))
    
  }), .id = "covar")
  
}), .id = "band") %>% 
  mutate(type = paste(band, covar)) %>% 
  mutate(type = str_replace_all(type, label_conf)) # use labels
save(df_confbands_robustL2, file = paste0(path_results, dgp_name, "_df_confbands_robustL2.RData"))

