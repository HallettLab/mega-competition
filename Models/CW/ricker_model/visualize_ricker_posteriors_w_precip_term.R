


# Pairs Plots ####


# Traceplots ####
sp <- names(ricker_plots)

pdf("models/CW/ricker_model/ricker_model_diagnostics/ricker_model_trace_plots_w_precip.pdf", width = 12, height = 8)

for(i in 1:length(sp)){
  
  ## traceplots are saved in ricker_plots list
  print(ricker_plots[[i]] + ggtitle(sp[i]))
  
}

dev.off()

## looks good

date <- 20230818

ggplot(ricker_posteriors2, aes(x = lambda)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 4, scales = "free") +
  #scale_fill_manual(values = c("#003366", "#FFA630")) +
  ggtitle("Lambda, Ricker, Mean L priors")

ggsave(file=paste0("models/CW/ricker_model/posterior_figures/mean_L_priors/lambda_ricker_meanLpriors_w_precip", date, ".png"), width = 12, height = 8)

ggplot(ricker_posteriors2, aes(x = beta)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 4, scales = "free") +
  #scale_fill_manual(values = c("#003366", "#FFA630")) +
  ggtitle("Beta (precip term)") +
  geom_vline(xintercept = 0, linetype = "dashed")

ggsave(file=paste0("models/CW/ricker_model/posterior_figures/mean_L_priors/beta_ricker_meanLpriors_w_precip", date, ".png"), width = 12, height = 8)

ggplot(ricker_posteriors2, aes(x = alpha_weeds)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 4, scales = "free") +
  #scale_fill_manual(values = c("#003366", "#FFA630")) +
  ggtitle("Alpha_weeds") +
  geom_vline(xintercept = 0, linetype = "dashed")

ggsave(file=paste0("models/CW/ricker_model/posterior_figures/mean_L_priors/alpha_ricker_meanLpriors_w_precip", date, ".png"), width = 12, height = 8)
