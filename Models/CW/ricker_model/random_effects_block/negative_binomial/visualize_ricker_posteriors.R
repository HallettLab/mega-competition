source("models/CW/ricker_model/random_effects_block/import_ricker_posteriors_neg_binom.R")

reps <- read.csv("models/CW/replicate-info.csv")
theme_set(theme_classic())

## set date for figures
#date <- 20231204

# Format Data ####
## add deviation terms to base terms

ricker_post <- ricker_posteriors2 %>%
  mutate(lambda_d = lambda_base, 
         lambda_c = lambda_base + lambda_dev)

## look at epsilons
random_fx <- ricker_post[, c(1:11,50:52)] %>%
  pivot_longer(1:11, names_to = "block", values_to = "random_effects")

ggplot(random_fx, aes(x= random_effects, color = species)) +
  geom_density() +
  facet_wrap(~block)


alphas_post <- ricker_post %>%
  mutate(alpha_acam_d = alpha_acam_base,
         alpha_acam_c = alpha_acam_base + alpha_acam_dev,
         alpha_amme_d = alpha_amme_base,
         alpha_amme_c = alpha_amme_base + alpha_amme_dev,
         alpha_anar_d = alpha_anar_base,
         alpha_anar_c = alpha_anar_base + alpha_anar_dev,
         alpha_brho_d = alpha_brho_base,
         alpha_brho_c = alpha_brho_base + alpha_brho_dev,
         alpha_brni_d = alpha_brni_base,
         alpha_brni_c = alpha_brni_base + alpha_brni_dev,
         alpha_ceso_d = alpha_ceso_base,
         alpha_ceso_c = alpha_ceso_base + alpha_ceso_dev,
         alpha_gitr_d = alpha_gitr_base,
         alpha_gitr_c = alpha_gitr_base + alpha_gitr_dev, 
         alpha_leni_d = alpha_leni_base,
         alpha_leni_c = alpha_leni_base + alpha_leni_dev,
         alpha_lomu_d = alpha_lomu_base,
         alpha_lomu_c = alpha_lomu_base + alpha_lomu_dev,
         alpha_mael_d = alpha_mael_base,
         alpha_mael_c = alpha_mael_base + alpha_mael_dev,
         alpha_mica_d = alpha_mica_base,
         alpha_mica_c = alpha_mica_base + alpha_mica_dev,
         alpha_pler_d = alpha_pler_base, 
         alpha_pler_c = alpha_pler_base + alpha_pler_dev,
         alpha_plno_d = alpha_plno_base, 
         alpha_plno_c = alpha_plno_base + alpha_plno_dev,
         alpha_taca_d = alpha_taca_base,
         alpha_taca_c = alpha_taca_base + alpha_taca_dev,
         alpha_twil_d = alpha_twil_base,
         alpha_twil_c = alpha_twil_base + alpha_twil_dev,
         #alpha_thir_d = alpha_thir_base,
        # alpha_thir_c = alpha_thir_base + alpha_thir_dev,
         alpha_weeds_d = alpha_weeds_base,
         alpha_weeds_c = alpha_weeds_base + alpha_weeds_dev)

alphas_post <- alphas_post[,50:84]

## change to long data format
ricker_posteriors_long <- alphas_post %>%
  pivot_longer(4:35, names_to = "alpha_name", values_to = "alpha_value") %>%
  filter(!alpha_name %in% c("alpha_weeds_c", "alpha_weeds_d")) %>%
  mutate(treatment = substr(alpha_name, 12,12),
         alpha_name = substr(alpha_name, 1,10))

# Vis Distributions ####
## Alphas ####
species <- c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "TWIL")

for(i in species){
  
  alpha <- ggplot(ricker_posteriors_long[ricker_posteriors_long$alpha_name == paste0("alpha_", tolower(i)),], aes(x = alpha_value, fill = treatment, line = treatment)) + 
    geom_density() + 
    facet_wrap(~species, ncol = 4, scales = "free")+
    scale_fill_manual(values = c("#003366", "#FFA630")) +
    ggtitle("Ricker Model") + xlab(paste0("alpha_", tolower(i))) +
    geom_vline(xintercept = 0, linetype = "dashed")
  
  ggsave(alpha, file=paste0("models/CW/ricker_model/random_effects_block/posterior_figures/neg_binomial/", "alpha_", tolower(i), "_", date, ".png"), width = 12, height = 9)
  
}

species <- c("ACAM")

for(i in species){
  
  invader <- ggplot(ricker_posteriors_long[ricker_posteriors_long$species == i,], aes(x = alpha_value, fill = treatment, line = treatment)) + 
    geom_density() + 
    facet_wrap(~alpha_name, ncol = 4, scales = "free") +
    scale_fill_manual(values = c("#003366", "#FFA630")) +
    ggtitle(paste0(i, " invader")) +
    geom_vline(xintercept = 0, linetype = "dashed")
  
  ggsave(invader, file=paste0("models/CW/ricker_model/random_effects_block/posterior_figures/", "invader_", i, "_inter_alphas_ricker_", date, ".png"), width = 12, height = 8)
  
}

## Lambda ####
### Together ####
ggplot(ricker_post, aes()) + 
  geom_density(aes(x = lambda_d), linetype = "dashed") +
  geom_density(aes(x=lambda_c))+
  facet_wrap(~species, ncol = 4) +
  xlab("Lambda") +
  ylab("Density") +
  #scale_fill_manual(values = c("#003366", "#FFA630")) +
  ggtitle("Lambda, Neg binom w/random effects")



















## calc mean lambda
#ricker_lambda_mean <- ricker_posteriors_long %>%
 # group_by(species, treatment) %>%
  #summarise(mean_lambda = mean(lambda), sd_lambda = sd(lambda))

## calc mean alphas
## filter to remove combos without enough replicates
#good.reps <- reps %>%
 # filter(true.reps > 3)

#good.reps.vec <- unique(good.reps$combos)

#ricker_alpha_mean <- ricker_posteriors_long %>%
 # mutate(combos = paste(species, toupper(substr(alpha_name, 7, 11)), treatment, sep = "_")) %>%
#  filter(combos %in% good.reps.vec) %>%
 # group_by(species, treatment, alpha_name) %>%
  #summarise(mean_alpha = mean(alpha_value), sd_alpha = sd(alpha_value)) %>%
  #mutate(alpha = substr(alpha_name, 7, 11))

## make a dataframe specifically for facilitation
#facilitation <- ricker_alpha_mean %>%
 # filter(mean_alpha < 0)



#ggsave(file=paste0("models/CW/ricker_model/posterior_figures/mean_L_priors/lambda_ricker_meanLpriors_constrainedby_50or100", date, ".png"), width = 12, height = 8)

### Indiv distribs ####
#for (i in species) {
  
 # ggplot(ricker_posteriors2[ricker_posteriors2$species == i,], aes(x=lambda)) +
  #  geom_density() +
   # facet_wrap(~treatment, scales = "free") +
    #ggtitle(i)
  
#  ggsave(paste0("models/CW/ricker_model/posterior_figures/mean_L_priors/lambda_separated/", i, "_lambda_meanLpriors", date, ".png"), width = 4, height = 3)
  
#}

# Vis Means ####
## Lambda ####
#ggplot(ricker_lambda_mean, aes(x=treatment, y=mean_lambda, color = treatment)) +
 # geom_point(size = 3) +
  #scale_color_manual(values = c("#003366", "#FFA630"))+
#  geom_errorbar(aes(ymin = mean_lambda - sd_lambda, ymax = mean_lambda + sd_lambda), width = 0.25) +
 # facet_wrap(~species, scales = "free") +
  #ggtitle("Max Lambda, Ricker")

#ggsave(file=paste0("models/CW/ricker_model/posterior_figures/mean_L_priors/mean_lambdas_ricker_meanLpriors", date, ".png"), width = 12, height = 9)

## Alphas ####
### Faceted by invader ####
#ggplot(ricker_alpha_mean, aes(x=alpha, y=mean_alpha, color = treatment)) +
 # geom_point() +
  #scale_color_manual(values = c("#003366", "#FFA630"))+
#  geom_errorbar(aes(ymin = mean_alpha - sd_alpha, ymax = mean_alpha + sd_alpha)) +
 # facet_wrap(~species, ncol = 4, nrow = 4, scales = "free") +
  #ggtitle("Mean Alpha, Ricker") +
#  theme(axis.text.x = element_text(angle = 20)) +
 # geom_hline(yintercept = 0, linetype = "dashed")

#ggsave(paste0("models/CW/ricker_model/posterior_figures/mean_L_priors/mean_alphas_ricker_meanLpriors_facet_invader", date, ".png"), width = 14, height = 10)  

### Faceted by alpha ####
#ggplot(ricker_alpha_mean, aes(x=species, y=mean_alpha, color = treatment)) +
 # geom_point() +
  #scale_color_manual(values = c("#003366", "#FFA630"))+
#  geom_errorbar(aes(ymin = mean_alpha - sd_alpha, ymax = mean_alpha + sd_alpha)) +
 # facet_wrap(~alpha_name, ncol = 3, nrow = 6, scales = "free") +
  #ggtitle("Mean Alpha, Ricker") +
#  theme(axis.text.x = element_text(angle = 30)) +
 # geom_hline(yintercept = 0, linetype = "dashed")

#ggsave(paste0("models/CW/ricker_model/posterior_figures/mean_L_priors/mean_alphas_ricker_meanLpriors_facet_alpha", date, ".png"), width = 15, height = 12) 

### Facilitation ####
### Mean summary
#nrow(ricker_alpha_mean[ricker_alpha_mean$mean_alpha < 0,])/nrow(ricker_alpha_mean)

#ggplot(facilitation, aes(x=species, y=mean_alpha, color = treatment)) +
#  geom_point() +
 # scale_color_manual(values = c("#003366", "#FFA630"))+
  #facet_wrap(~alpha_name, scales = "free", ncol = 3, nrow = 5) +
#  theme(axis.text.x = element_text(angle = 90)) +
 # geom_errorbar(aes(ymin=mean_alpha-sd_alpha, ymax = mean_alpha+sd_alpha), width = 0.25) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
#  ggtitle("Facilitative Alphas")

#ggsave(paste0("models/CW/ricker_model/posterior_figures/mean_L_priors/mean_alphas_facilitation_ricker_meanLpriors_facet_alpha", date, ".png"), width = 10, height = 8)

