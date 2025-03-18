# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Simulations for dynamic regressions - Setup

#Set Up -------------------------------------------------------------------------

rm(list = ls()) # Clear Work space

# Load the package
library(ACFbands)

# Additional package
library(tidyverse)

# Load custom functions
file_sources = list.files(path = "code/functions", pattern="*.R", include.dirs = TRUE)
sapply(file_sources, function(i) source(paste("code/functions/", i, sep="")))


# Set the general path for the results
path_results <- "results/sim_dyn/"
path_graphics <- "graphics/"

# Parameters ------------------------------------------------------------------

I <- 50 # number of initialization values for the DGP
alpha <- 0.1 # significance level
R_mc <- 1000 # number of Monte Carlo replications
N_vec <- c(50, 200, 800) # vector of sample sizes
H_vec <- c(1, 10, 25) # vector of maximum number of lags
a <- 0 # intercept of the AR DGP
phi1_vec <- c(0.5) # AR coefficient of lag 1
phi2_vec <- c(0, 0.125, 0.25) # AR coefficient of lag 2

# Make a grid
params <- expand.grid(phi1 = phi1_vec, phi2 = phi2_vec, N = N_vec, H = H_vec)

# Make an ID
params$id <-  paste0(gsub("0.","", params$phi1), 
                     substr(params$phi2, 3, 3), 
                     params$N/10,
                     params$H)
  

# Generate seed for data generating process: ignore H, i.e. remove the last digit from ID
params$seed1 <- as.numeric(paste0(gsub("0.","", params$phi1), 
                                   substr(params$phi2, 3, 3), 
                                   params$N/10))/100

# Seed for inference bands
params$seed2 <- round(as.numeric(params$seed1)/2)

# Check for unique IDs
length(unique(params$id)) == nrow(params)

# Define vectors to iterate over ---------------------------------------------

# Types of confidence bands
type_vec <- c("pointwise", "simultaneous")
names(type_vec) <- type_vec

# Types of errors
error_vec <- c("het", "hom")
names(error_vec) <- error_vec

# Vector of tests
test_vec <- c("ljungbox", "BG")
names(test_vec) <- test_vec

