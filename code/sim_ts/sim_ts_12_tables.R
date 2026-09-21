# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Simulations for time series - Make tables.


#Set Up 
source("code/sim_ts/sim_ts_07_setup_robust.R")

# Load additional package
library(tidyverse)
library(xtable)
library(kableExtra)
library(tidyverse)

# Panel names
panel_names1 <-  c("size: $\\phi = 0$", "power: $\\phi = 0.25$", 
                   "power: $\\phi = 0.5$", "power: $\\phi = 0.75$",
                   "power: $\\phi = 0.95$")

# Panel names
panel_names2 <-  c("$\\phi = 0$", "$\\phi = 0.25$", 
                   "$\\phi = 0.5$", "$\\phi = 0.75$",
                   "$\\phi = 0.95$")
# Bandwidths classic
label_bw <- setNames(c("$L = 5T^{1/2}$",
                       "$L = 3T^{1/2}$",
                       "$L = T^{1/2}$",
                       "$L = T^{1/3}$",
                       "$L = 0.75T^{1/3}$"), c("L5", "L4", "L1", "L2", "L3"))

# Bandwidths robust
label_bw_robust <- setNames(c("$L = 5N^{1/2}$",
                       "$L = 3N^{1/2}$",
                       "$L = N^{1/2}$",
                       "$L = N^{1/3}$",
                       "$L = 0.75N^{1/3}$"), c("L5", "L4", "L1", "L2", "L3"))

# Load results for classical bands
load(paste0(path_results, dgp_name, "_df_sigbands.RData"))
load(paste0(path_results, dgp_name, "_df_tests.RData"))
load(paste0(path_results, dgp_name, "_df_confbands.RData"))

# Load results for robust bands
load(paste0(path_results, dgp_name, "_df_confbands_robust.RData"))
load(paste0(path_results, dgp_name, "_df_sigbands_robust.RData"))
load(paste0(path_results, dgp_name, "_df_confbands_robustL2.RData"))

# Significance Bands------------------------------------------------------------

# Combine results into data frame
df_sig <- bind_rows(df_sigbands, 
                    df_confbands %>% filter(type == "Classical sup-t CB", L == "L1") %>% dplyr::select(-c(L, cover)), 
                    df_confbands_robust %>% filter(band == "bonferroni", L == "L2"),
                    df_sigbands_robust %>% filter(band == "bonferroni"))  %>% 
  arrange(phi, N, 
          match(type, c("Classical sup-t SB", "Classical sup-t CB", "Classical pointw. SB", "Robust Bonf. SB",  
                        "Robust Bonf. CB")))

# Select variables
df_rej <- df_sig %>% 
  dplyr::select(type, rej, H, phi, N) 
df_width <- df_sig %>% 
  dplyr::select(type, avg_width, H, phi, N) 


# Table for frequency of rejections
make_table(df = df_rej,
           x = "rej",
           rnames = unique(df_rej$type),
           panel_names = panel_names1,
           filename =  paste0(path_graphics, "/tab_", dgp_name, "_inf_rej.tex"))

# Table for average width
make_table(df = df_width,
           x = "avg_width",
           rnames = unique(df_width$type),
           panel_names = panel_names2,
           filename =  paste0(path_graphics, "/tab_", dgp_name, "_inf_width.tex"))


df_rej <- df_tests %>% 
  dplyr::select(type, rej, H, phi, N) %>% 
  arrange(phi, N, 
          match(type, c("Box-Pierce",  "Ljung-Box", "Robust Box-Pierce", "Shao")))
# Table for frequency of rejections
make_table(df = df_rej,
           x = "rej",
           rnames = unique(df_rej$type),
           panel_names = panel_names1,
           filename =  paste0(path_graphics, "/tab_", dgp_name, "_test_rej.tex"))



# Joint table for classical and robust confidence bands --------------------------

df_cover_sel <- bind_rows(df_confbands %>% 
  filter(L == "L1"),  df_confbands_robustL2) %>% 
  arrange(phi, N, match(type, c("Classical sup-t CB", "Classical Bonf. CB", "Classical pointw. CB", 
                                "Robust sup-t CB", "Robust Bonf. CB", "Robust pointw. CB")))
  
# Table for coverage
make_table(df = df_cover_sel %>% dplyr::select(type, cover, H, phi, N),
           x = "cover",
           rnames = unique(df_cover_sel$type),
           panel_names =  paste0("$\\phi = ", phi_vec, "$"),
           filename =  paste0(path_graphics, "/tab_", dgp_name, "_conf_cover_sel.tex"))

# Table for average width
make_table(df = df_cover_sel %>%  dplyr::select(type, avg_width, H, phi, N),
           x = "avg_width",
           rnames = unique(df_cover_sel$type),
           panel_names = paste0("$\\phi = ", phi_vec, "$"),
           filename =  paste0(path_graphics, "/tab_", dgp_name, "_conf_avg_width_sel.tex"))


# Classical sup-t confidence bands for all bandwidths --------------------------------------

df_cover_sel <- df_confbands %>% 
  filter(type == "Classical sup-t CB") %>% 
  dplyr::select(L, cover, H, phi, N) %>% 
  arrange(phi, N, match(L, bw_vec)) %>% 
  mutate(L = str_replace_all(L, label_bw)) # use labels

make_table(df = df_cover_sel,
           x = "cover",
           rnames = unique(df_cover_sel$L),
           panel_names =  paste0("$\\phi = ", phi_vec, "$"),
           filename =  paste0(path_graphics, "/tab_", dgp_name, "_conf_cover_supt.tex"))

# Coverage of robust Bonferroni confidence bands for all bandwidths ------------

covar_sel <- "bonferroni"

df_cover_sel <- df_confbands_robust %>% 
  filter(band == covar_sel) %>% 
  dplyr::select(L, cover, H, phi, N) %>% 
  arrange(phi, N, match(L, bw_vec)) %>% 
  mutate(L = str_replace_all(L, label_bw_robust)) # use labels


make_table(df = df_cover_sel,
           x = "cover",
           rnames = unique(df_cover_sel$L),
           panel_names =  paste0("$\\phi = ", phi_vec, "$"),
           filename =  paste0(path_graphics, "/tab_", dgp_name, "_conf_robust_cover_", covar_sel, ".tex"))




