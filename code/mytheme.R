# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Setup for the plots.

# Theme for Plots
mytheme <- theme_bw() +
  theme(plot.title = element_text(size= 12, hjust = 0.5), # center the title
        legend.title = element_blank(),
        legend.text=element_text(size=10), 
        legend.key.width = unit(2.5, "line"), # width of the displayed line
        axis.title.x = element_text(size = 10),
        axis.title.y =  element_text(size = 10, angle = 90),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        strip.text = element_text(size = 10)) # facet labels

# Color scheme
greys <- gray.colors(2, start = 0.1, end = 0.6, gamma = 2.2, alpha = NULL, rev = FALSE)
mycolors <- c(rep(greys[1], 2), rep(greys[2], 2))

# Linetypes
mylines <- c("longdash", "dotted", "longdash", "dotted")
