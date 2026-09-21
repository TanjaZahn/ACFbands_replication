# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Estimates autocorrelations with significance bands for daily excess returns.

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
path_results <- "results/app_finance/"

# Set the plot theme
source("code/mytheme.R")

# Significance level
alpha <- 0.1

# Vector of bandtypes for singificance bands
sig_types <- c("pointwise", "sup-t", "bonferroni")
names(sig_types) <- sig_types
conf_types <- c("pointwise", "sup-t", "bonferroni")
names(conf_types) <-conf_types

# Labels: Classic Bands
label_conf <- c("pointwise" = "Classical pointw. CB",
                "sup-t" = "Classical sup-t CB",
                "bonferroni" = "Classical Bonf. CB")

label_sig <- c("pointwise" = "Classical pointw. SB",
               "sup-t" = "Classical sup-t SB")


# Labels: robust bands
label_conf_robust <- c("pointwise" = "Robust pointw. CB",
                "sup-t" = "Robust sup-t CB",
                "bonferroni" = "Robust Bonf. CB")

label_sig_robust <- c("pointwise" = "Robust pointw. SB",
               "sup-t" = "Robust sup-t SB",
               "bonferroni" = "Robust Bonf. SB")


# Load and Prepare Data --------------------------------------------------------

# Load data
df <- as_tibble(read.csv("data/fama_french_daily.txt", header = TRUE, sep = ",")) %>% 
  rename(r = Mkt.RF) %>% 
  mutate(r2 = r^2) %>% 
  mutate(date = as.Date(X,format="%Y%m%d")) %>% 
  filter(date >= as.Date("2006-01-01"))


# Drop missing values
df <- df[complete.cases(df), ]

# Maximum number of lags
N <- nrow(df)
H <- floor(10*log10(N))

# Plot the series -----------------------------

# Daily excess returns 
p1 <- ggplot(df, aes(x = date)) + geom_line(mapping = aes(y = r), color = "black") +
  labs(x = "Date", y = "Excess Returns") +
  mytheme
ggsave(paste0(path_graphics, "fama_return_series.pdf"), width = 20, height = 12, units = "cm") 


# Significance bands for returns -----------------------------------------------

# Generate classic bands
df_sig_classic <- bind_rows(lapply(sig_types[1:2], function(type){
    
    estimate <- acf_sigbands(y = df$r, H = H, alpha = alpha, type = type, covar = "iid", plot = FALSE)
    df <- data.frame(rho_hat = estimate$rho_hat , estimate$sig_band, h = 1:H)
    
  }), .id = "type") %>% 
  mutate(type = str_replace_all(type, label_sig)) # use labels

# Generate robust bands
df_sig_robust <- bind_rows(lapply(sig_types[3], function(type){
  
  estimate <- acf_sigbands(y = df$r, H = H, alpha = alpha, type = type, 
                           covar = "MDS", plot = FALSE)
  df <- data.frame(rho_hat = estimate$rho_hat , estimate$sig_band, h = 1:H)
  
}), .id = "type") %>% 
  mutate(type = str_replace_all(type, label_sig_robust)) # use labels

# Combine
df_sig <- bind_rows(df_sig_classic, df_sig_robust)

# Plot
p_sig <- make_plot_color(df = df_sig, segment = TRUE,  line_vec = mylines, color_vec = mycolors) +
  scale_y_continuous(limits = c(-0.2, 0.2), breaks = round(seq(-0.2, 0.2, 0.1), 1)) +
  theme(legend.position=c(0.2,0.15))
ggsave(paste0(path_graphics, "famafrench_autocor.pdf"), width = 17, height = 12, units = "cm") 
