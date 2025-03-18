# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Setup for the empirical applications.

rm(list = ls()) # Clear Work space
set.seed(5667)

# Load package
library(ACFbands)

# Load further package
library(lubridate)
library(patchwork)
library(tidyverse)
library(colorspace)
library(stargazer)

# Load custom functions
file_sources = list.files(path = "code/functions", pattern="*.R", include.dirs = TRUE)
sapply(file_sources, function(i) source(paste("code/functions/", i, sep="")))

# Path of the graphics and results
path_graphics <- "graphics/"
path_results <- "results/application/"

# Set the plot theme
source("code/mytheme.R")

H <- 30
alpha <- 0.1
i <- 12 # number of lags

# Vector of bandtypes for singificance bands
sig_types <- c("pointwise", "simultaneous")
names(sig_types) <- sig_types
conf_types <- c("pointwise", "sup-t", "bonferroni")
names(conf_types) <-conf_types

# Labels (with capital letters)
label_conf <- c("pointwise" = "Pointwise",
                "sup-t" = "Sup-t",
                "bonferroni" = "Bonferroni")

label_sig <- c("pointwise" = "Pointwise",
               "simultaneous" = "Simultaneous")

# Linetypes
mylines <- rep("longdash", 3)

# Load and Prepare Data --------------------------------------------------------

# Load data
df_fred <- read.csv("data/FRED-MD-2024-07.csv", header = TRUE, sep = ",")

# Keep only relevant variables and make transformations
df_fred <- as_tibble(df_fred[ -1 , c("sasdate", "CPIAUCSL", "UNRATE")]) %>% 
  rename(cpi = CPIAUCSL,
         unrate = UNRATE) %>% 
  mutate(date = mdy(sasdate),
         inf = (cpi - dplyr::lag(cpi, 1))/ dplyr::lag(cpi, 1)*100,
         delta_inf = inf - dplyr::lag(inf, 1)) %>% 
  dplyr::select(- c(sasdate)) 


# Add lags for inflation
df_fred <- add_lags(df_fred, "inf", 1:i)

# Add lags for unemployment
df_fred <- add_lags(df_fred, "unrate", 1:i)

# Add lags for delta_inf
df_fred <- add_lags(df_fred, "delta_inf", 1:i)

# Same sample size for each case
df_fred <- df_fred %>% filter(date >= as.Date("1961-01-01"))


