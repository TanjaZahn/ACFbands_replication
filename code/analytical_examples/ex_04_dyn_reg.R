# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Create figure 6 and 7, i.e. for an AR(1) example
#             # plot the exact versus the naive significance bands
#             # plot the coverage of the naive significance bands for a dynamic regression

#Set Up 
source("code/analytical_examples/ex_01_setup.R")

# Only for coefficients not equal to zero
phi1_vec <- phi_vec[-1]
mylabels <- mylabels[-1]
mycolors <- mycolors[-1]


# Compute coverage probability -------------------------------------------------

df_res  <- bind_rows(lapply(phi1_vec, function(phi1){ # iterate over coefficients
  
  res_list2 <- bind_rows(lapply(H_vec, function(H){ # iterate over H
    
    # Calculate true variance matrix
    G <- matrix(NA, H, H)
    for(i in 1:H){
      for(j in 1:H){
        G[i, j] <- phi1^(i+j-2)
      }
    }
    Sigma_rho <- diag(H) - (1-phi1^2)*G
    
    # Vector of expected values
    mu <- rep(0, H)
    
    # Rescale covariance matrix
    A <- matrix(0, nrow = H, ncol = H)
    diag(A) <- 1/sqrt(diag(Sigma_rho)) 
    
    # New Covariance matrix
    Sigma <- A %*% Sigma_rho %*% t(A)
    
    # Quantile: Correct Simultaneous Bands
    s <- qmvnorm(p = 1 - alpha, tail = "both.tails", mean = mu, sigma = Sigma)$quantile
    
    # Quantile: Naive simultaneous bands
    s_naive <- sqrt(qchisq(p = (1-alpha)^(1/H), df = 1))
    
    # Coverage probability
    cover <- pmvnorm(lower = rep(-s, H), upper = rep(s, H), mean = mu, sigma = Sigma)[1]
    names(cover) <- "cover"
    cover_naive <- pmvnorm(lower = rep(-s_naive, H), upper = rep(s_naive, H), mean = mu, sigma = Sigma_rho)[1]
    names(cover_naive) <- "cover_naive"
    
    c(s = s, s_naive = s_naive, cover, cover_naive)
    
  }), .id = "H")
}), .id = "phi1") %>% 
  mutate(H = as.numeric(H))


# Plot the coverage of the naive bands
plot_example(df = df_res, yvar = "cover_naive", colorvar = "phi1", ylabel = "Coverage", hline = 1-alpha) +
  scale_y_continuous(limits=c(0.8, 1), breaks = seq(0.8, 1, 0.1)) 
ggsave(paste0(path_graphics, "ar1_residual_cover_naive.pdf"), width = 20, height = 12, units = "cm") 


# Plot the inference bands themselves ------------------------------------------

phi1 <- 0.5
H <- 25
N <- 200
set.seed(42)

# Calculate true variance matrix
G <- matrix(NA, H, H)
for(i in 1:H){
  for(j in 1:H){
    G[i, j] <- phi1^(i+j-2)
  }
}
Sigma_rho <- diag(H) - (1-phi1^2)*G

# Correct Bands ----------------------------------------------
s1 <- equi_quantile(Sigma = Sigma_rho, tau = 1-alpha)

# Construct significance bands
lb <- - sqrt(diag(Sigma_rho)/N)*s1 # lower bound
ub <-+ sqrt(diag(Sigma_rho)/N)*s1 # upper bound
width <- ub - lb # width

# Into data frame
sigband_simul <- data.frame(lb, ub, width, h = 1:H, type = rep("Exact simultaneous", H))

# Naive Simultaneous Bands: Wrong covariance matrix ----------------------------

s2 <- sqrt(qchisq(p = (1-alpha)^(1/H), df = 1))

# Construct significance bands
lb <- - sqrt(rep(1, H)/N)*s2 # lower bound
ub <-+ sqrt(rep(1, H)/N)*s2 # upper bound
width <- ub - lb # width

sigband_naive_simul <- data.frame(lb, ub, width,  h = 1:H, type = rep("Naive simultaneous", H))


# Plot -------------------------------------------------------------------------

# Combine into one data frame
df <- as_tibble(bind_rows(sigband_naive_simul,
                          sigband_simul))

# lock in factor level order
df$type <- factor(df$type, levels = unique(df$type))


ggplot(df, aes(x = h)) +
  geom_hline(aes(yintercept = 0), color = "cornsilk4") +
  geom_line(aes(y = lb, color = type), linetype = "longdash") +
  geom_line(aes(y = ub, color = type), linetype = "longdash") +
  # labs(x = "Lag", y = "Significance bands") +
  labs(x = "Lag", y = "ACF") +
  scale_x_continuous(limits = c(0, H), breaks = seq(0, H, 5)) +
  scale_color_manual(values=mycolors) +
  mytheme +
  scale_y_continuous(limits = c(-1, 1), breaks = round(seq(-1, 1, 0.2), 1)) +
  theme(legend.position = c(0.15, 0.14), legend.margin=margin(c(0,1,1,1)))
ggsave(paste0(path_graphics, "ar1_residual_sig_bands.pdf"), width = 20, height = 12, units = "cm")








