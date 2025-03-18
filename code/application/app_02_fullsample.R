# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Conducts the main empirical application of the paper, i.e. using the full sample, including:
#           # construction of confidence bands for inflation itself;
#           # estimation of statsic and dynamic Phillips curve (in differences and in levels);
#           # graphical analysis of regression residuals;
#           # construction of significance bands for the regression residuals.


#Set Up 
source("code/application/app_01_setup.R")

# Plot the monthly inflation and employment series -----------------------------

# Monthly inflation rate in percent
p1 <- ggplot(df_fred, aes(x = date)) + geom_line(mapping = aes(y = inf), color = "black") +
 labs(x = "Date", y = "Inflation") +
  mytheme

# Monthly inflation rate in percent
p2 <- ggplot(df_fred, aes(x = date)) + geom_line(mapping = aes(y = unrate), color = "black") +
  labs(x = "Date", y = "Unemployment") +
  mytheme 

# Combine plots
p1 + plot_spacer() + p2 + plot_spacer() +
  plot_layout(guides = 'collect', # same legend
              widths = c(7, 0.5 ,7, 0.5))
ggsave(paste0(path_graphics, "monthly_inflation_unemployment.pdf"), width = 30, height = 10, units = "cm") 


# Confidence Bands for Inflation -----------------------------------------------

# Generate bands
df_conf <- bind_rows(lapply(conf_types, function(type){
  
  estimate <- acf_confbands(y = df_fred$inf, H = H, L = sqrt(length(df_fred$inf)), alpha = alpha, type = type, plot = FALSE)
  df <- data.frame(rho_hat = estimate$rho_hat , estimate$conf_band, h = 1:H)
  
}), .id = "type") %>% 
  mutate(type = str_replace_all(type, label_conf)) # use labels

# Plot
make_plot_color(df = df_conf, segment = FALSE, line_vec = mylines, color_vec = mycolors)
ggsave(paste0(path_graphics, "inflation_conf_bands_m1.pdf"), width = 20, height = 12, units = "cm") 


# Static Phillips Curve --------------------------------------------------------

# Get residuals from regression
res <- lm(delta_inf ~ unrate, df_fred)$residuals

# Plot the residuals
ggplot(df_fred, aes(x = date)) + geom_line(mapping = aes(y = res), color = "black") +
  labs(x = "Date", y ="Residuals") +
  mytheme
ggsave(paste0(path_graphics, "phillips_res.pdf"), width = 20, height = 12, units = "cm") 

# Generate bands
df_sig <- bind_rows(lapply(sig_types, function(type){
  
  estimate <- acf_sigbands(y = res, H = H, alpha = alpha, type = type, plot = FALSE)
  df <- data.frame(rho_hat = estimate$rho_hat , estimate$sig_band, h = 1:H)
  
}), .id = "type") %>% 
  mutate(type = str_replace_all(type, label_sig)) # use labels

# Plot
make_plot_color(df = df_sig, segment = TRUE,  line_vec = mylines, color_vec = mycolors)
ggsave(paste0(path_graphics, "phillips_sig_bands.pdf"), width = 20, height = 12, units = "cm")


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


# Make tables with coefficients from regression in differences -----------------

# Get the names of regressors
regressors <- names(coef(fits[[2]]))

# Desired order
myorder <- c("(Intercept)", "unrate", paste0("L", 1:i, "_unrate"),  paste0("L", 1:i, "_delta_inf"))

# Match
order <- match(myorder, regressors )

# Rename regressors
delta_inf_names <- paste0("$\\Delta \\pi_{t-", 1:i, "}$")
unrate_names <- c("$ u_t $", paste0("$u_{t-", 1:i, "}$"))
mynames <- c("Intercept", unrate_names, delta_inf_names)             
        

stargazer(lm(delta_inf ~ unrate, df_fred), fits[[1]], fits[[2]], 
          align=TRUE, single.row = TRUE, font.size = "small",
          dep.var.labels.include = FALSE,
          dep.var.caption= "Dependent variable: $\\Delta \\pi_t$",
          omit.stat=c("ser","f"), no.space=TRUE,
          order= order, intercept.top=TRUE, intercept.bottom=FALSE,
          covariate.labels = mynames,
          label = "tab:phillips_estimates_diff")

# Make tables with coefficients from regression in levels ---------------------

# Get the names of regressors
regressors <- names(coef(fits[[4]]))

# Desired order
myorder <- c("(Intercept)", "unrate", paste0("L", 1:i, "_unrate"),  paste0("L", 1:i, "_inf"))

# Match
order <- match(myorder, regressors )

# Rename regressors
inf_names <- paste0("$ \\pi_{t-", 1:i, "}$")
unrate_names <- c("$ u_t $", paste0("$u_{t-", 1:i, "}$"))
mynames <- c("Intercept", unrate_names, inf_names)             


stargazer(fits[[3]], fits[[4]], 
          align=TRUE, single.row = TRUE, font.size = "small",
          dep.var.labels.include = FALSE,
          dep.var.caption= "Dependent variable: $\\pi_t$",
          omit.stat=c("ser","f"), no.space=TRUE,
          order= order, intercept.top=TRUE, intercept.bottom=FALSE,
          covariate.labels = mynames,
          label = "tab:phillips_estimates_level")



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
ggsave(paste0(path_graphics, "phillips_sig_dyn_diff.pdf"), width = 30, height = 10, units = "cm") 

# Combine plots: Inflation in Levels
p_bands[[3]] + plot_spacer() + p_bands[[4]] + plot_spacer() +
  plot_layout(guides = 'collect', # same legend
              widths = c(7, 0.5 ,7, 0.5))#  &
theme(legend.text=element_text(size=10))
ggsave(paste0(path_graphics, "phillips_sig_dyn_level.pdf"), width = 30, height = 10, units = "cm") 


# Plot Residuals ----------------------------------------------------------------

# Individuals Plots
p_res <-  lapply(fits, function(fit){
  ggplot(df_fred, aes(x = date)) + geom_line(mapping = aes(y = fit$residuals), color = "black") +
    labs(x = "Date", y = "Residuals") +
    mytheme })

# Combine plots: Inflation in First Difference
p_res[[1]] + plot_spacer() + p_res[[2]] + plot_spacer() +
  plot_layout(guides = 'collect', # same legend
              widths = c(7, 0.5 ,7, 0.5))#  &
theme(legend.text=element_text(size=10))
ggsave(paste0(path_graphics, "phillips_res_dyn_diff.pdf"), width = 30, height = 10, units = "cm") 

# Combine plots: Inflation in Levels
p_res[[3]] + plot_spacer() + p_res[[4]] + plot_spacer() +
  plot_layout(guides = 'collect', # same legend
              widths = c(7, 0.5 ,7, 0.5))#  &
theme(legend.text=element_text(size=10))
ggsave(paste0(path_graphics, "phillips_res_dyn_level.pdf"), width = 30, height = 10, units = "cm") 


# Plot residuals against regressors --------------------------------------------

# Plot the residuals against regressors
p_res_u <- ggplot(df_fred, aes(x = unrate)) + geom_line(mapping = aes(y = fits[[2]]$residuals), color = "black") +
  labs(x =  expression(paste(u[t])), y = "Residuals") +
  mytheme 

p_res_inf <- ggplot(df_fred, aes(x = L1_delta_inf)) + geom_line(mapping = aes(y = fits[[2]]$residuals), color = "black") +
  labs(x =  expression(paste(Delta, pi[t-1])), y = "Residuals") +
  mytheme 

# Combine plots: Inflation in Levels
p_res_u + plot_spacer() + p_res_inf + plot_spacer() +
  plot_layout(guides = 'collect', # same legend
              widths = c(7, 0.5 ,7, 0.5))#  &
theme(legend.text=element_text(size=10))
ggsave(paste0(path_graphics, "phillips_res_against_x_diff.pdf"), width = 30, height = 10, units = "cm") 


# Plot the residuals against regressors
p_res_u <- ggplot(df_fred, aes(x = unrate)) + geom_line(mapping = aes(y = fits[[4]]$residuals), color = "black") +
  labs(x =  expression(paste(u[t])), y = "Residuals") +
  mytheme 

p_res_inf <- ggplot(df_fred, aes(x = L1_inf)) + geom_line(mapping = aes(y = fits[[4]]$residuals), color = "black") +
  labs(x =  expression(paste(pi[t-1])), y = "Residuals") +
  mytheme 

# Combine plots: Inflation in Levels
p_res_u + plot_spacer() + p_res_inf + plot_spacer() +
  plot_layout(guides = 'collect', # same legend
              widths = c(7, 0.5 ,7, 0.5))#  &
theme(legend.text=element_text(size=10))
ggsave(paste0(path_graphics, "phillips_res_against_x_level.pdf"), width = 30, height = 10, units = "cm") 





