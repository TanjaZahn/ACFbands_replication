# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Conducts the macroeconomic application of the paper using the sample from 1985 onward

#Set Up 
source("code/application/macro_01_setup.R")

# Start the sample in 1985
df_fred <- df_fred %>% filter(date >= as.Date("1985-01-01"))

N <- nrow(df_fred)
H <- floor(10*log10(N))

# Run Dynamic Phillips Curve ---------------------------------------------------

# Regression formulas
formulas <-c(
  
  paste0("delta_inf ~ ",  " unrate + ", paste("L", 1:1, "_delta_inf", sep = "", collapse = " + ")),
  
  paste0("delta_inf ~ ", " unrate + ", paste("L", 1:i, "_unrate", sep = "", collapse = " + ") ,
         " + ", paste("L", 1:i, "_delta_inf", sep = "", collapse = " + ")),
  
  paste0("inf ~ ",  " unrate + ", paste("L", 1:1, "_inf", sep = "", collapse = " + ")),
  
  paste0("inf ~ ", " unrate + ", paste("L", 1:i, "_unrate", sep = "", collapse = " + ") ,
         " + ", paste("L", 1:i, "_inf", sep = "", collapse = " + "))
  
)

# Fit regression models
fits <- lapply(1:length(formulas), function(i){ lm(formulas[[i]], df_fred)})

# Generate classic dynamic significance bands with correct variance
classic_simul <- lapply(fits, function(fit){
  acf_sigbands_dyn(fit = fit, H = H, type = "sup-t" , covar = "classical" , alpha = alpha, plot = FALSE)
})

# Generate naive simultaneous bands, i.e. with incorrect variance matrix
naive_simul <- lapply(fits, function(fit){
  acf_sigbands(y = fit$residuals, H = H, type = "sup-t",  covar = "iid" , alpha = alpha, plot = FALSE)})

# Generate naive simultaneous bands, i.e. with incorrect variance matrix
naive_pointw <- lapply(fits, function(fit){
  acf_sigbands(y = fit$residuals, H = H, type = "pointwise",  covar = "iid" , alpha = alpha, plot = FALSE)})

# Robust (MDS) significance bands
robust_simul <- lapply(fits, function(fit){
  acf_sigbands_dyn(fit = fit, H = H, type = "bonferroni" ,  covar = "robust", alpha = alpha, plot = FALSE)})


# Plot significance bands ------------------------------------------------------

p_bands <- lapply(1:length(formulas), function(i){ 
  
  # Make a data frame
  df_dyn <- bind_rows(list("Corrected classical sup-t SB" = data.frame(rho_hat = classic_simul[[i]]$rho_hat, classic_simul[[i]]$sig_band),
                           "Naive classical sup-t SB" = data.frame(rho_hat = naive_simul[[i]]$rho_hat, naive_simul[[i]]$sig_band),
                           "Naive classical pointw. SB" = data.frame(rho_hat = naive_pointw[[i]]$rho_hat, naive_pointw[[i]]$sig_band),
                           "Corrected robust Bonf. SB" = data.frame(rho_hat = robust_simul[[i]]$rho_hat, robust_simul[[i]]$sig_band)), 
                      .id = "type") %>% 
    group_by(type) %>% 
    mutate(h = 1:H) %>% 
    ungroup()
  
  # Order
  df_dyn <- df_dyn %>% 
    arrange(match(type, c("Naive classical pointw. SB", "Naive classical sup-t SB",  "Corrected classical sup-t SB", "Corrected robust Bonf. SB")))
  
  # Plot
  p_bands <- make_plot_color(df = df_dyn, segment = TRUE, color_vec = mycolors, line_vec = mylines) +
    scale_y_continuous(limits = c(-0.4, 0.4), breaks = round(seq(-0.4, 0.4, 0.1), 1)) +
    theme(legend.position = c(0.1, 0.14), legend.margin=margin(c(0,1,1,1)))
})

# Combine plots: Inflation in First Difference
p_bands[[1]] + plot_spacer() + p_bands[[2]] + plot_spacer() +
  plot_layout(guides = 'collect', # same legend
              widths = c(7, 0.5 ,7, 0.5))#  &
theme(legend.text=element_text(size=10))
ggsave(paste0(path_graphics, "phillips_sig_dyn_diff_1985.pdf"), width = 30, height = 10, units = "cm") 

# Combine plots: Inflation in Levels
p_bands[[3]] + plot_spacer() + p_bands[[4]] + plot_spacer() +
  plot_layout(guides = 'collect', # same legend
              widths = c(7, 0.5 ,7, 0.5))#  &
theme(legend.text=element_text(size=10))
ggsave(paste0(path_graphics, "phillips_sig_dyn_level_1985.pdf"), width = 30, height = 10, units = "cm") 

