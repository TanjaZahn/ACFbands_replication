# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Simulations for time series - Setup

#Set Up -------------------------------------------------------------------------

rm(list = ls()) # Clear Work space

# Load the package
library(ACFbands)

# Name of DGP
dgp_name <- "ar1"

# Load custom functions
file_sources = list.files(path = "code/functions", pattern="*.R", include.dirs = TRUE)
sapply(file_sources, function(i) source(paste("code/functions/", i, sep="")))

# Set the general path for the results
path_results <- "results/sim_ts/"
path_graphics <- "graphics/"

# Parameters -------------------------------------------------------------------

alpha <- 0.1 # significance level
R_mc <- 1000 # number of Monte Carlo replications
N_vec <- c(50, 200, 800) # sample sizes
H_vec <- c(1, 10, 25) # length of autocorrelation vector
phi_vec <- c(0, 0.25, 0.5, 0.75) # AR(1) parameter
I <- 50 # number of initialization values for the DGP

# Make a grid
params <- expand.grid(H = H_vec, phi =  phi_vec, N = N_vec)

# Make an ID
params$id <- paste0(params$H, as.numeric(gsub("0.","",params$phi)), params$N/10)

# Generate seed1 for generating y: use the same seed for each H
params$seed1 <- as.numeric(paste0(1, as.numeric(gsub("0.","",params$phi)), params$N/10))

# Generate seed2 for the inference bands
params$seed2 <- round(as.numeric(params$id)/2)

# # Add rownumber
# params$row <- 1:nrow(params)

# Check for unique IDs
length(unique(params$seed2)) == nrow(params)

# Add bandwidth
params$L1 <- params$N^(1/2)
params$L2 <- params$N^(1/3)
params$L3 <- 0.75*params$N^(1/3)
params$L4 <- 3*params$N^(1/2)
params$L5 <- 5*params$N^(1/2)

# Define vectors to iterate over ---------------------------------------------

# Vector of bandwidth names
bw_vec <- c("L5", "L4", "L1", "L2", "L3")
names(bw_vec) <- bw_vec

# Vector of bandtypes for significance bands
sig_types <- c("simultaneous", "pointwise")
names(sig_types) <- sig_types

# Vector of bandtypes for confidence bands
conf_types <- c("sup-t", "bonferroni", "pointwise")
names(conf_types) <- c("sup_t", "bonferroni", "pointwise")

# Vector of tests
test_vec <- c("boxpierce", "ljungbox")
names(test_vec) <- test_vec



