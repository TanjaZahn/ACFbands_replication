# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Simulations for dynamic regressions - Make tables

#Set Up
source("code/sim_dyn/sim_dyn_01_setup.R")

# Additional Packages
library(xtable)
library(kableExtra)

# Labels
label <- c("naive pointwise" = "Naive pointw. SB",
           "naive simultaneous" = "Naive simult. SB",
           "hom simultaneous" = "Exact simult. SB",
           "NA ljungbox" = "Ljung-Box",
           "NA BG" = "Breusch-Godfrey")

# Order
band_order <- c("hom simultaneous",
                "naive simultaneous",
                "naive pointwise",
                "ljungbox",
                "BG")


# Load results and calculcate frequency of rejections ---------------------------

# Naive bands
df_naive <- bind_rows(lapply(type_vec , function(type){ # iterate over band_type
   bind_rows(lapply(1:nrow(params), function(p){ # iterate over parameters
      
      calc_rej(filename = paste0(path_results, "naive_bands_", type, "_", params[p, "id"], ".RData"),
               add_params = params[p, c("H", "phi1", "phi2", "N")])
      
    }))
  }) , .id = "type") %>% 
  mutate(error = "naive")


# Homoskedastic Simultaneous Dynamic bands
error <- "hom"
type <- "simultaneous"
df_dyn <- bind_rows(lapply(1:nrow(params), function(p){
      
      calc_rej(filename =  paste0(path_results, "dyn_bands_", type, "_", error, "_", params[p, "id"], ".RData"),
               add_params = params[p, c("H", "phi1", "phi2", "N")])
      
    })) %>% 
  mutate(error = error, type = type)



# Tests
df_tests <- bind_rows(lapply(test_vec, function(test){ # iterate over test_type
    
    rej_t <- bind_rows(lapply(1:nrow(params), function(p){ # iterate over parameters
      
      # Load estimated inference bands
      load(paste0(path_results, "dyn_", test, "_", params[p, "id"], ".RData"))
      
      # Take the mean over R_mc simulations and add parameter values
      cbind(rej = mean(do.call(c, estimates_r)), params[p, c("H", "phi1", "phi2", "N")])
      
    }))
  }), .id = "type")
  
# Combine into data frame
df_rej <- bind_rows(df_naive, df_dyn, df_tests) %>% 
  mutate(band = paste(error, type)) %>% 
  mutate(label = str_replace_all(band, label)) %>% 
  arrange(phi2, H, N, match(band, band_order)) 

# Make tables ------------------------------------------------------------------

# Frequency of rejections
make_table(df = (df_rej %>% dplyr::select(any_of(c("rej", "label", "H", "phi2", "N")))),
             x = "rej",
             rnames = unique(df_rej$label),
             panel_names =   c("size: $\\phi_2 = 0$", 
                               "power: $\\phi_2 = 0.125$",
                               "power: $\\phi_2 = 0.25$"),
             filename = paste0(path_graphics, "/tab_dyn_sig_rej.tex"))

df_width <- (df_rej %>% filter(type %in% c("simultaneous", "pointwise")) %>% 
               dplyr::select(any_of(c("avg_width", "label", "H", "phi2", "N")))) 
# Average width (over h and R)
make_table(df = df_width,
           x = "avg_width",
           rnames = unique(df_width$label),
           panel_names =   c("size: $\\phi_2 = 0$", 
                             "power: $\\phi_2 = 0.125$",
                             "power: $\\phi_2 = 0.25$"),
            filename = paste0(path_graphics, "/tab_dyn_sig_width.tex"))
  


