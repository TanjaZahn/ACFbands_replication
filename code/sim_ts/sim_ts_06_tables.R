# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Simulations for time series - Calculate the frequency of rejections and save results as a data frame.


#Set Up 
source("code/sim_ts/sim_ts_01_setup.R")

# Load additional package
library(tidyverse)
library(xtable)
library(kableExtra)
library(tidyverse)

# Calculate frequency of rejection for the significance bands ------------------

df_sigbands  <- bind_rows(lapply(sig_types, function(band_type){ # iterate over band_type
  
  bind_rows(lapply(1:nrow(params), function(p){ # iterate over parameters
    
    calc_rej(filename = paste0(path_results, dgp_name, params[p, "id"], "_sig_", band_type, ".RData"),
             add_params = params[p, c("H", "phi", "N")])
    
  }))
  
}), .id = "type")


# Load frequency of rejection for the tests -------------------------------

df_tests <- bind_rows(lapply(test_vec, function(test){ # iterate over test_type
  
  bind_rows(lapply(1:nrow(params), function(p){ # iterate over parameters
    
    # Load estimated inference bands
    load(paste0(path_results, dgp_name, params[p, "id"], "_sig_", test, ".RData"))
    
    # Take the mean over R_mc simulations and add parameter values
    cbind(rej = mean(do.call(c, estimates_r)), params[p,  c("H", "phi", "N")])
    
  }))
}), .id = "type")


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

}), .id = "type")


# Tables: Rejections and width of signficance bands -----------------------------------------------

# Combine results into data frame
df_rej <- bind_rows(df_sigbands, 
                    df_confbands %>% filter(type == "sup_t", L == "L1") %>% dplyr::select(-c(L, cover)), 
                    df_tests) %>% 
  dplyr::select(-avg_width) %>% 
  arrange(phi, N, match(type, c("simultaneous", "sup_t", "pointwise", "boxpierce", "ljungbox")))

# Table for frequency of rejections
make_table(df = df_rej,
           x = "rej",
           rnames = c("Simult. SB", "Sup-t CB", "Pointw. SB", "Box-Pierce", "Ljung-Box"),
           panel_names =  c("size: $\\phi = 0$", "power: $\\phi = 0.25$", 
                            "power: $\\phi = 0.5$", "power: $\\phi = 0.75$"),
            filename =  paste0(path_graphics, "/tab_", dgp_name, "_sig_rej.tex"))


df_width <- df_sigbands %>% 
  arrange(phi, H, N, match(type, c("simultaneous", "pointwise"))) %>% 
  filter(phi == 0) %>% # the width is the same for each coefficient
  dplyr::select(-rej)

# Table for average width
make_table(df = df_width,
           x = "avg_width",
           rnames = c("Simultaneous", "Pointwise"),
           filename = paste0(path_graphics, "/tab_", dgp_name, "_sig_avgwidth.tex"))


# Table: Coverage and width of confidence bands for bandwidth T^(1/2) -----------------

df_cover_sel <- df_confbands %>% 
  filter(L == "L1") %>% 
  arrange(phi, N, match(type, conf_types))


# Table for coverage
make_table(df = df_cover_sel %>% dplyr::select(-c(avg_width, rej)),
           x = "cover",
           rnames = c("Sup-t", "Bonf.", "Pointw."),
           panel_names =  c("$\\phi = 0$", "$\\phi = 0.25$", 
                            "$\\phi = 0.5$", "$\\phi = 0.75$"),
           filename =  paste0(path_graphics, "/tab_", dgp_name, "_conf_cover_L1.tex"))

# Table for average width
make_table(df = df_cover_sel %>% dplyr::select(-c(rej, cover)),
           x = "avg_width",
           rnames = c("Sup-t", "Bonf.", "Pointw."),
           panel_names =  c("$\\phi = 0$", "$\\phi = 0.25$", 
                            "$\\phi = 0.5$", "$\\phi = 0.75$"),
           filename =  paste0(path_graphics, "/tab_", dgp_name, "_conf_avg_width_L1.tex"))


# Tables: Coverage of sup-t confidence bands for all bandwidths ---------------------------------------

df_cover_sel <- df_confbands %>% 
  filter(type == "sup_t") %>% 
  dplyr::select(-c(avg_width, rej)) %>% 
  arrange(phi, N, match(L, bw_vec))

# Table for frequency of rejections
make_table(df = df_cover_sel,
           x = "cover",
           rnames = c("$L = 5T^{1/2}$",
                      "$L = 3T^{1/2}$",
                      "$L = T^{1/2}$",
                      "$L = T^{1/3}$",
                      "$L = 0.75T^{1/3}$"),
           panel_names =  c("$\\phi = 0$", "$\\phi = 0.25$", 
                            "$\\phi = 0.5$", "$\\phi = 0.75$"),
           filename =  paste0(path_graphics, "/tab_", dgp_name, "_conf_cover_supt.tex"))


