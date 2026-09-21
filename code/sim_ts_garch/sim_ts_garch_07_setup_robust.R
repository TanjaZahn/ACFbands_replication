# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Simulations for time series with GARCH errors - Setup for robust bands

#Set Up -------------------------------------------------------------------------

rm(list = ls()) # Clear Work space

# Load the package
library(ACFbands)

# Name of DGP
dgp_name <- "ar1_garch"

# Load custom functions
file_sources = list.files(path = "code/functions", pattern="*.R", include.dirs = TRUE)
sapply(file_sources, function(i) source(paste("code/functions/", i, sep="")))

# Set the general path for the results
path_results <- "results/sim_ts_garch/"
path_graphics <- "graphics/"

# Parameters -------------------------------------------------------------------

alpha <- 0.1 # significance level
R_mc <- 1000 # number of Monte Carlo replications
N_vec <- c(50, 200, 800) # sample sizes
H_vec <- c(1, 10, 25) # length of autocorrelation vector
phi_vec <- c(0, 0.25, 0.5, 0.75, 0.95) # AR(1) parameter
# phi_vec <- c(0) # AR(1) parameter
I <- 50 # number of initialization values for the DGP

# Make a grid
params <- expand.grid(H = H_vec, phi =  phi_vec, N = N_vec)

# Make an ID
params$id <- paste0(params$H, as.numeric(gsub("0.","",params$phi)), params$N/10)

# Generate seed1 for generating y: use the same seed for each H
params$seed1 <- as.numeric(paste0(1, as.numeric(gsub("0.","",params$phi)), params$N/10))

# Generate seed2 for the inference bands
params$seed2 <- round(as.numeric(params$id)/2)

# Check for unique IDs
length(unique(params$seed2)) == nrow(params)

# Add bandwidth
params$L1 <- (params$N - params$H)^(1/2)
params$L2 <- (params$N-params$H)^(1/3)
params$L3 <- 0.75*(params$N-params$H)^(1/3)
params$L4 <- 3*(params$N-params$H)^(1/2)
params$L5 <- 5*(params$N-params$H)^(1/2)

# Define vectors to iterate over ---------------------------------------------

# Vector of bandwidth names
bw_vec <- c("L4", "L1", "L2", "L3")
names(bw_vec) <- bw_vec

# Vector of bandtypes for significance bands
sig_types <- c("sup-t", "bonferroni", "pointwise")
names(sig_types) <- sig_types

# Type of covariance matrix
covar_types <- c("robust", "MDS")
names(covar_types) <- covar_types

# Vector of bandtypes for confidence bands
conf_types <- c("sup-t", "bonferroni", "pointwise")
names(conf_types) <- conf_types


# Labels: Classic Bands
label_conf <- c("pointwise robust" = "Robust pointw. CB",
                "sup-t robust" = "Robust sup-t CB",
                "bonferroni robust" = "Robust Bonf. CB")

label_sig <- c("pointwise MDS" = "Robust pointw. SB",
               "sup-t MDS" = "Robust sup-t SB",
               "bonferroni MDS" = "Robust Bonf. SB")




