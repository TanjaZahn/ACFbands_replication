# Paper: "Simultaneous Inference bands for Autocorrelations" by Uwe Hassler, Marc-Oliver Pohle and Tanja Zahn.
# File: Create figure 5 (relative width of sup-t over h) as well as the examples for the B matrices in the Appendix.


#Set Up 
source("code/analytical_examples/ex_01_setup.R")

H <- 10

##### Converting matrix into LaTeX format -------------------------------------
# Code taken from https://www.r-bloggers.com/2020/08/matrix-to-latex/


array_to_LaTeX <- function(arr){
  rows <- apply(arr, MARGIN=1, paste, collapse = " & ")
  matrix_string <- paste(rows, collapse = " \\\\ ")
  return(paste("\\begin{bmatrix}", matrix_string, "\\end{bmatrix}"))
}

# cat(array_to_LaTeX(arr)) to print results


# Calculate examples ----------------------------------------------------------

for(phi in phi_vec){
  
  print(phi)
  B <- calc_Bartlett_true(phi = phi, H = H)
  cat(array_to_LaTeX(round(B, 3)))
  
  # Create some space
  for(i in 1:5){ print( " ")}
}
  



# Plot -------------------------------------------------------------------------

res <- lapply(phi_vec, function(phi){
  tibble(h = 1:H,  B_sqrt = sqrt(diag(calc_Bartlett_true(phi = phi, H = H))))
  
})
df_res <- bind_rows(res, .id = "phi") 

ggplot(df_res, aes(x = h, y = B_sqrt)) +
  geom_line(aes(color = phi)) +
  scale_y_continuous(limits=c(0.6, 2), breaks = seq(0.6, 2, 0.2)) +
  scale_x_continuous(limits=c(1, H), breaks = seq(1, H, 1)) +
  scale_color_manual(labels = mylabels,  values = mycolors) +
  labs(x = "h", y = "Relative width") +
  mytheme
ggsave(paste0(path_graphics, "conf_bands_width_over_h.pdf"), width = 20, height = 12, units = "cm") 

