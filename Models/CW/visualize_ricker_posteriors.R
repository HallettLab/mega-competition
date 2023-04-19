## change to long data format
posteriors_long <- posteriors2 %>%
  pivot_longer(2:24, names_to = "alpha_name", values_to = "alpha_value")


# Save figures ####
## Alphas ####
sp <- c("PLER", "BRHO", "GITR", "AVBA", "ANAR",  "TACA", "LOMU", "TWIL", "THIR", "CESO", "MICA", "AMME", "PLNO", "ACAM", "BRNI", "LENI", "CLPU", "MAEL")
rm(species)
## save model coeff
for(i in sp){
  
  alpha <- ggplot(posteriors_long[posteriors_long$alpha_name == paste0("alpha_", tolower(i)),], aes(x = alpha_value, fill = treatment, line = treatment)) + 
    geom_density() + 
    facet_wrap(~species, ncol = 3, scales = "free")+
    scale_fill_manual(values = c("#003366", "#FFA630")) +
    ggtitle("Ricker Model") + xlab(paste0("alpha_", tolower(i))) +
    geom_vline(xintercept = 0, linetype = "dashed")
  
  
  ggsave(alpha, file=paste0("models/CW/preliminary_figures/ricker_model_posteriors/", "alpha_", tolower(i), "_ricker.png"), width = 12, height = 10)
  
  
  invader <- ggplot(posteriors_long[posteriors_long$species == i,], aes(x = alpha_value, fill = treatment, line = treatment)) + 
    geom_density() + 
    facet_wrap(~alpha_name, ncol = 3, scales = "free") +
    scale_fill_manual(values = c("#003366", "#FFA630")) +
    ggtitle(paste0(i, " invader")) +
    geom_vline(xintercept = 0, linetype = "dashed")
  
  ggsave(invader, file=paste0("models/CW/preliminary_figures/ricker_model_posteriors/", i, "_inter_alphas_ricker.png"), width = 14, height = 10)
  
  
}

## Lambda ####
ggplot(posteriors2, aes(x = lambda, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free") +
  scale_fill_manual(values = c("#003366", "#FFA630"))
ggsave(file=paste0("models/CW/preliminary_figures/ricker_model_posteriors/", i, "_inter_alphas_ricker.png"), width = 14, height = 10)
