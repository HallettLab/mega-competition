source("models/CW/import_ricker_posteriors.R")
reps <- read.csv("models/CW/replicate-info.csv")
theme_set(theme_bw())

# Format Data ####
## change to long data format
ricker_posteriors_long <- ricker_posteriors2 %>%
  pivot_longer(2:24, names_to = "alpha_name", values_to = "alpha_value") %>%
  filter(!alpha_name %in% c("alpha_figa", "alpha_gamu", "alpha_hygl", "alpha_siga", "alpha_other", "alpha_crco", "alpha_erbo", "lp__"))

## calc mean lambda
ricker_lambda_mean <- ricker_posteriors_long %>%
  group_by(species, treatment) %>%
  summarise(mean_lambda = mean(lambda), sd_lambda = sd(lambda))

## calc mean alphas
## filter to remove combos without enough replicates
good.reps <- reps %>%
  filter(true.reps > 3)

good.reps.vec <- unique(good.reps$combos)

ricker_alpha_mean <- ricker_posteriors_long %>%
  mutate(combos = paste(species, toupper(substr(alpha_name, 7, 11)), treatment, sep = "_")) %>%
  filter(combos %in% good.reps.vec) %>%
  group_by(species, treatment, alpha_name) %>%
  summarise(mean_alpha = mean(alpha_value), sd_alpha = sd(alpha_value)) %>%
  mutate(alpha = substr(alpha_name, 7, 11))

# Save figures ####
## Alphas ####
species <- c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL")

for(i in species){
  
  alpha <- ggplot(ricker_posteriors_long[ricker_posteriors_long$alpha_name == paste0("alpha_", tolower(i)),], aes(x = alpha_value, fill = treatment, line = treatment)) + 
    geom_density() + 
    facet_wrap(~species, ncol = 3, scales = "free")+
    scale_fill_manual(values = c("#003366", "#FFA630")) +
    ggtitle("Ricker Model") + xlab(paste0("alpha_", tolower(i))) +
    geom_vline(xintercept = 0, linetype = "dashed")
  
  ggsave(alpha, file=paste0("models/CW/ricker_model/posterior_figures/", "alpha_", tolower(i), "_ricker.png"), width = 12, height = 10)
  
  invader <- ggplot(ricker_posteriors_long[ricker_posteriors_long$species == i,], aes(x = alpha_value, fill = treatment, line = treatment)) + 
    geom_density() + 
    facet_wrap(~alpha_name, ncol = 3, scales = "free") +
    scale_fill_manual(values = c("#003366", "#FFA630")) +
    ggtitle(paste0(i, " invader")) +
    geom_vline(xintercept = 0, linetype = "dashed")
  
  ggsave(invader, file=paste0("models/CW/ricker_model/posterior_figures/", i, "_inter_alphas_ricker.png"), width = 14, height = 10)
  
  
}

## Lambda ####
ggplot(ricker_posteriors2, aes(x = lambda, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free") +
  scale_fill_manual(values = c("#003366", "#FFA630")) +
  ggtitle("Lambda, Ricker")

ggsave(file=paste0("models/CW/preliminary_figures/ricker_model_posteriors/lambda_ricker.png"), width = 14, height = 10)

## Means ####
ggplot(ricker_lambda_mean, aes(x=treatment, y=mean_lambda, color = treatment)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#003366", "#FFA630"))+
  geom_errorbar(aes(ymin = mean_lambda - sd_lambda, ymax = mean_lambda + sd_lambda), width = 0.25) +
  facet_wrap(~species, scales = "free") +
  ggtitle("Mean Lambda, Ricker")
ggsave("models/CW/ricker_model/posterior_figures/mean_lambdas_ricker.png", width = 12, height = 9)  

ggplot(ricker_alpha_mean, aes(x=alpha, y=mean_alpha, color = treatment)) +
  geom_point() +
  scale_color_manual(values = c("#003366", "#FFA630"))+
  geom_errorbar(aes(ymin = mean_alpha - sd_alpha, ymax = mean_alpha + sd_alpha)) +
  facet_wrap(~species, ncol = 3, nrow = 6, scales = "free") +
  ggtitle("Mean Alpha, Ricker") +
  theme(axis.text.x = element_text(angle = 20)) +
  geom_hline(yintercept = 0, linetype = "dashed")
ggsave("models/CW/ricker_model/posterior_figures/mean_alphas_ricker.png", width = 14, height = 10)  


ggplot(ricker_alpha_mean, aes(x=species, y=mean_alpha, color = treatment)) +
  geom_point() +
  scale_color_manual(values = c("#003366", "#FFA630"))+
  geom_errorbar(aes(ymin = mean_alpha - sd_alpha, ymax = mean_alpha + sd_alpha)) +
  facet_wrap(~alpha_name, ncol = 3, nrow = 6, scales = "free") +
  ggtitle("Mean Alpha, Ricker") +
  theme(axis.text.x = element_text(angle = 30)) +
  geom_hline(yintercept = 0, linetype = "dashed")
ggsave("models/CW/ricker_model/posterior_figures/mean_alphas_ricker2.png", width = 15, height = 12) 

### Mean summary
nrow(ricker_alpha_mean[ricker_alpha_mean$mean_alpha < 0,])/nrow(ricker_alpha_mean)

facilitation <- ricker_alpha_mean %>%
  filter(mean_alpha < 0, 
         alpha_name != "alpha_avba",
         alpha_name != "alpha_clpu")
ggplot(facilitation, aes(x=species, y=mean_alpha, color = treatment)) +
  geom_point() +
  scale_color_manual(values = c("#003366", "#FFA630"))+
  facet_wrap(~alpha_name, scales = "free", ncol = 3, nrow = 5) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_errorbar(aes(ymin=mean_alpha-sd_alpha, ymax = mean_alpha+sd_alpha), width = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed")
ggsave("models/CW/ricker_model/posterior_figures/mean_alphas_facilitation_ricker.png", width = 10, height = 8)
