# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Robustness checks for the sample from 1985 onward
#           # construction of significance bands for the regression residuals.


#Set Up 
source("code/application/app_01_setup.R")

# Start the sample in 1985
df_fred <- df_fred %>% filter(date >= as.Date("1985-01-01"))

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

# Generate dynamic significance bands with correct variance
exact_simul <- lapply(fits, function(fit){
  acf_sigbands_dyn(fit = fit, H = H, type = "simultaneous" ,  error = "hom", alpha = alpha, plot = FALSE)})

# Generate naive simultaneous bands, i.e. with incorrect variance matrix
naive_simul <- lapply(fits, function(fit){
  acf_sigbands(y = fit$residuals, H = H, type = "simultaneous",  alpha = alpha, plot = FALSE)})

# Generate naive simultaneous bands, i.e. with incorrect variance matrix
naive_pointw <- lapply(fits, function(fit){
  acf_sigbands(y = fit$residuals, H = H, type = "pointwise",  alpha = alpha, plot = FALSE)})


# Plot significance bands ------------------------------------------------------

p_bands <- lapply(1:length(formulas), function(i){ 
  
  # Make a data frame
  df_dyn <- bind_rows(list("Exact simultaneous" = data.frame(rho_hat = exact_simul[[i]]$rho_hat, exact_simul[[i]]$sig_band),
                           "Naive simultaneous" = data.frame(rho_hat = naive_simul[[i]]$rho_hat, naive_simul[[i]]$sig_band),
                           "Naive pointwise" = data.frame(rho_hat = naive_pointw[[i]]$rho_hat, naive_pointw [[i]]$sig_band)), 
                      .id = "type") %>% 
    group_by(type) %>% 
    mutate(h = 1:H) %>% 
    ungroup()
  
  # Order
  df_dyn <- df_dyn %>% arrange(match(type, c("Naive pointwise", "Naive simultaneous",  "Exact simultaneous")))
  
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

