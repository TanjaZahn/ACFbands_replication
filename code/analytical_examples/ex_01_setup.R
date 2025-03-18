# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Setup for the analytical examples.


rm(list = ls()) # Clear Work space
set.seed(42)

# Load the package
library(ACFbands)

# Load additional packages
library(mvtnorm)
library(tidyverse)
library(patchwork)

# Load custom functions
file_sources = list.files(path = "code/functions", pattern="*.R", include.dirs = TRUE)
sapply(file_sources, function(i) source(paste("code/functions/", i, sep="")))

# Set the path for graphics
path_graphics <- "graphics/"

# Set the plot and color schemes
source("code/mytheme.R")

H_vec <- setNames(1:25, 1:25)
phi_vec <- setNames(c(0, 0.25, 0.5, 0.75), c(0, 0.25, 0.5, 0.75))
names(phi_vec) <- phi_vec
alpha <- 0.1

# Nice labels
mylabels <- c(expression(paste(phi," = ", "0.0")),
              expression(paste(phi," = ", "0.25")),
              expression(paste(phi," = ", "0.5")),
              expression(paste(phi," = ", "0.75")))




