# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Simulations for dynamic regressions with GARCH errors - Make tables

#Set Up
source("code/sim_dyn_garch/sim_dyn_garch_01_setup.R")

# Additional Packages
library(xtable)
library(kableExtra)


# Labels
label <- c("naive pointwise classical" = "Naive classical pointw. SB",
           "naive sup-t classical" = "Naive classical sup-t SB",
           "corrected sup-t classical" = "Corrected classical SB",
           "NA ljungbox_res classical" = "Ljung-Box",
           "NA BG classical" = "Breusch-Godfrey",
           "NA bp_robust NA" = "Robust Box-Pierce",
           "corrected bonferroni robust" = "Corrected robust SB")

# Order
band_order <- c("corrected sup-t classical",
                "corrected bonferroni robust",
                "naive sup-t classical",
                "naive pointwise classical",
                "NA ljungbox_res classical" ,
                "NA BG classical",
                "NA bp_robust NA")


# Load results and calculcate frequency of rejections ---------------------------

# Naive bands
df_naive <- bind_rows(lapply(type_vec , function(type){ # iterate over band_type
  bind_rows(lapply(1:nrow(params), function(p){ # iterate over parameters
    
    calc_rej(filename = paste0(path_results, dgp_name, "_naive_bands_", type, "_", params[p, "id"], ".RData"),
             add_params = params[p, c("H", "phi1", "phi2", "N")])
    
  }))
}) , .id = "type") %>% 
  mutate(covar = "classical", corr = "naive")


# Corrected classical bands
covar<- "classical"
type <- "sup-t"
df_dyn <- bind_rows(lapply(1:nrow(params), function(p){
  
  calc_rej(filename =  paste0(path_results, dgp_name , "_bands_", type, "_", covar, "_", params[p, "id"], ".RData"),
           add_params = params[p, c("H", "phi1", "phi2", "N")])
  
})) %>% 
  mutate(type = type, covar = covar, corr = "corrected")



# Tests
df_tests <- bind_rows(lapply(test_vec, function(test){ # iterate over test_type
  
  rej_t <- bind_rows(lapply(1:nrow(params), function(p){ # iterate over parameters
    
    # Load estimated inference bands
    load(paste0(path_results, dgp_name, "_", test, "_", params[p, "id"], ".RData"))
    
    # Take the mean over R_mc simulations and add parameter values
    cbind(rej = mean(do.call(c, estimates_r)), params[p, c("H", "phi1", "phi2", "N")])
    
  }))
}), .id = "type") %>% 
  mutate(covar = "classical")

test <- "bp_robust"
# Robust Box Pierce test
df_bp <- bind_rows(lapply(1:nrow(params), function(p){ # iterate over parameters
  
  # Load estimated inference bands
  load(paste0(path_results, dgp_name, "_", test, "_", params[p, "id"], ".RData"))
  
  # Take the mean over R_mc simulations and add parameter values
  cbind(rej = mean(do.call(c, estimates_r)), params[p, c("H", "phi1", "phi2", "N")])
  
}))  %>% 
  mutate(type = "bp_robust")



# Robust bands
covar <- "robust"
df_dyn_robust <-bind_rows(lapply(type_vec_robust, function(type){ # iterate over band types
  
  bind_rows(lapply(1:nrow(params), function(p){ # iterate over parameters
    
    calc_rej(filename =  paste0(path_results, dgp_name , "_bands_", type, "_", covar, "_", params[p, "id"], ".RData"),
             add_params = params[p, c("H", "phi1", "phi2", "N")])
    
  }))
  
}), .id = "type") %>%  
  mutate(covar = "robust", corr = "corrected")





# Combine into data frame
df_rej <- bind_rows(df_naive, df_dyn, df_tests, df_bp, df_dyn_robust ) %>% 
  mutate(band = paste(corr, type, covar)) %>% 
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
           filename = paste0(path_graphics, "/tab_", dgp_name, "_sig_rej.tex"))

df_width <- (df_rej %>% filter(type %in% c("sup-t", "pointwise", "bonferroni")) %>% 
               dplyr::select(any_of(c("avg_width", "label", "H", "phi2", "N")))) 
# Average width (over h and R)
make_table(df = df_width,
           x = "avg_width",
           rnames = unique(df_width$label),
           panel_names =   c("$\\phi_2 = 0$", 
                             "$\\phi_2 = 0.125$",
                             "$\\phi_2 = 0.25$"),
           filename = paste0(path_graphics, "/tab_", dgp_name, "_sig_width.tex"))


# Analyze algorithm ------------------------------------------------------------

label<- c("robust" = "het", "classical" = "hom")

df_shrink <- bind_rows(lapply(covar_vec, function(covar){
  
  bind_rows(lapply(1:nrow(params), function(p){
    
    # Load covariance matrix
    filename <- paste0(path_results, dgp_name , "_covmat_", covar, "_", params[p, "id"], ".RData")
    load(filename)
    
    shrinkage <- mean(sapply(1:R_mc, function(r) covmat_r[[r]]$shrinkage_used))
    med_shrink = median(sapply(1:R_mc, function(r) covmat_r[[r]]$shrinkage_lag), na.rm = TRUE)
    cbind(shrinkage = shrinkage, med_shrink = med_shrink, params[p, c("H", "phi1", "phi2", "N")])
    
  })) 
  
}), .id = "type")  %>% 
  mutate(type = str_replace_all(type, label))



# Make table 
make_table(df = df_shrink %>% arrange(phi2) %>% select(-med_shrink),
           x = "shrinkage",
           rnames = unique(df_shrink$type),
           panel_names =   c("$\\phi_2 = 0$", 
                             "$\\phi_2 = 0.125$",
                             "$\\phi_2 = 0.25$"),
           filename = paste0(path_graphics, "/tab_", dgp_name, "_robust_shrinkage.tex"))


# Median lag 
make_table(df =df_shrink %>% arrange(phi2) %>%  select(-shrinkage),
           x = "med_shrink",
           rnames = unique(df_shrink$type),
           panel_names =   c("$\\phi_2 = 0$", 
                             "$\\phi_2 = 0.125$",
                             "$\\phi_2 = 0.25$"),
           filename = paste0(path_graphics, "/tab_", dgp_name, "_robust_shrinkage_lag.tex"))



