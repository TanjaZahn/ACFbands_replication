# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Create figures 3 and 4, i.e. for an AR(1) process showing
#           # the relative width of sup-t and pointwise confidence bands and asymptotic coverage of pointwise bands
#           # the relative width of sup-t and Bonferroni confidence bands and asympotic coverage of Bonferroni bands


#Set Up 
source("code/analytical_examples/ex_01_setup.R")

# Compute quantiles ----------------------------------------------------------

df_res  <- bind_rows(lapply(phi_vec, function(phi){ # iterate over coefficients
  
  res_rho <- bind_rows(lapply(H_vec, function(H){ # iterate over H
    
    # Calculate B matrix for an AR(1) process
    B <- calc_Bartlett_true(phi = phi, H = H)
    
    # Vector of expected values
    mu <- rep(0, H)
    
    # Rescale covariance matrix
    A <- matrix(0, nrow = H, ncol = H)
    diag(A) <- 1/sqrt(diag(B)) 
    Sigma <- A %*% B %*% t(A)
    
    # Quantile: Sup-t Bands
    c1 <- qmvnorm(p = 1 - alpha, tail = "both.tails", mean = mu, sigma = Sigma)$quantile
    
    # Quantile: Bonferroni
    c2 <- qnorm(p = 1-alpha/(2*H))
    
    # Quantile: Pointwise bands
    c3 <- qnorm(1-alpha/2)
    
    # True coverage of sup-t intervals
    cover_supt <- pmvnorm(lower = rep(-c1, H), upper = rep(c1, H), mean = mu, sigma = Sigma)[1]
    names(cover_supt) <- "cover_supt"
    
    # True coverage of Bonferroni intervals
    cover_bonf <- pmvnorm(lower = rep(-c2, H), upper = rep(c2, H), mean = mu, sigma = Sigma)[1]
    names(cover_bonf) <- "cover_bonf"
    
    # True coverage of pointwise intervals
    cover_point <- pmvnorm(lower = rep(-c3, H), upper = rep(c3, H), mean = mu, sigma = Sigma)[1]
    names(cover_point) <- "cover_point"
    
    # Collect results
    c(c1 = c1, c2 = c2, c3 = c3, ratio_bonf = c2/c1, ratio_point = c1/c3,
      cover_supt, cover_bonf, cover_point)
    
  }), .id = "H")
}), .id = "phi") %>% 
  mutate(H = as.numeric(H))

# Relative width: Sup-t versus Pointwise ---------------------------------------

# Relative width: Bonferroni to Sup-t
p1 <- plot_example(df = df_res, yvar = "ratio_point", colorvar = "phi", ylabel = "Relative width", hline = 1) +
  scale_y_continuous(limits=c(0.9, 2), breaks = seq(0.9, 2, 0.1)) 

# coverage of Bonferroni 
p2 <- plot_example(df = df_res, yvar = "cover_point", colorvar = "phi", ylabel =  "Coverage", hline = 1-alpha) +
  scale_y_continuous(limits=c(0, 1), breaks = seq(0, 1, 0.1))


# Combine plots
p1 + plot_spacer() + p2 + plot_spacer() +
  plot_layout(guides = 'collect', # same legend
              widths = c(7, 0.5 ,7, 0.5))
ggsave(paste0(path_graphics, "conf_bands_width_and_coverage_pointwise.pdf"), width = 30, height = 10, units = "cm") 



# Relative width: Sup-t versus Bonferroni ---------------------------------------

# Relative width: Bonferroni to Sup-t
p1 <- plot_example(df = df_res, yvar = "ratio_bonf", colorvar = "phi", ylabel = "Relative width", hline = 1) +
  scale_y_continuous(limits=c(0.9, 1.2), breaks = seq(0.9, 1.2, 0.1)) 


# coverage of Bonferroni 
p2 <- plot_example(df = df_res, yvar = "cover_bonf", colorvar = "phi", ylabel =  "Coverage", hline = 1-alpha) +
  scale_y_continuous(limits=c(0.8, 1.1), breaks = seq(0.8, 1.1, 0.1)) 
  

# Combine plots
p1 + plot_spacer() + p2 + plot_spacer() +
  plot_layout(guides = 'collect', # same legend
              widths = c(7, 0.5 ,7, 0.5))
ggsave(paste0(path_graphics, "conf_bands_relative_width_ar1.pdf"), width = 30, height = 10, units = "cm") 

