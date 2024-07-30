source("models/CW/ricker_model/random_effects_block/negative_binomial/import_ricker_posteriors_neg_binom.R")

reps <- read.csv("data/replicate-info.csv")
theme_set(theme_classic())

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

## set date for figures
date <- 20240714

# Format Data ####
## add deviation terms to base terms
ricker_post <- ricker_posteriors2 %>%
  mutate(lambda_d = lambda_base, 
         lambda_c = lambda_base + lambda_dev)

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
         alpha_thir_d = alpha_thir_base,
         alpha_thir_c = alpha_thir_base + alpha_thir_dev,
         alpha_weeds_d = alpha_weeds_base,
         alpha_weeds_c = alpha_weeds_base + alpha_weeds_dev)

alphas_post <- alphas_post[,51:87]

#write.csv(alphas_post, "data/posteriors_20231218_models.csv")

## change to long data format
ricker_posteriors_long <- alphas_post %>%
  pivot_longer(4:37, names_to = "alpha_name", values_to = "alpha_value") %>%
  filter(!alpha_name %in% c("alpha_weeds_c", "alpha_weeds_d")) %>%
  mutate(treatment = substr(alpha_name, 12,12),
         alpha_name = substr(alpha_name, 1,10))

# Random Effects ####
## look at epsilons
random_fx <- ricker_post[, c(1:11,51:53)] %>%
  pivot_longer(1:11, names_to = "block", values_to = "random_effects") %>%
  mutate(ppt = ifelse(block %in% c("V1", "V2", "V3", "V4", "V5", "V6"), "D", "C"),
         block = fct_relevel(block, c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11")))

ggplot(random_fx, aes(x= random_effects, color = species)) +
  geom_density() +
  facet_wrap(~block, ncol = 6, nrow = 2) +
  xlab("Random effect of block")

ggsave(paste0("models/CW/ricker_model/random_effects_block/negative_binomial/posterior_figures/", date, "/block_random_effects.png"), width = 7, height = 4.5)


# Vis Distributions ####
## Alphas ####
species <- c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL")

#for(i in species){
  
 # alpha <- ggplot(ricker_posteriors_long[ricker_posteriors_long$alpha_name == paste0("alpha_", tolower(i)),], aes(x = alpha_value, fill = treatment, line = treatment)) + 
  #  geom_density() + 
   # facet_wrap(~species, ncol = 4, scales = "free")+
    #scale_fill_manual(values = c("#003366", "#FFA630")) +
#    ggtitle("Ricker Model") + xlab(paste0("alpha_", tolower(i))) +
 #   geom_vline(xintercept = 0, linetype = "dashed")
  
  #ggsave(alpha, file=paste0("models/CW/ricker_model/random_effects_block/negative_binomial/posterior_figures/", date, "/alpha_", tolower(i), "_", date, ".png"), width = 12, height = 9)
  
#}

for(i in species){
  
  invader <- ggplot(ricker_posteriors_long[ricker_posteriors_long$species == i,], aes(x = alpha_value, fill = treatment, line = treatment)) + 
    geom_density() + 
    facet_wrap(~alpha_name, ncol = 4, scales = "free") +
    scale_fill_manual(values = c("#003366", "#FFA630")) +
    ggtitle(paste0(i, " invader")) +
    geom_vline(xintercept = 0, linetype = "dashed")
  
  ggsave(invader, file=paste0("models/CW/ricker_model/random_effects_block/negative_binomial/posterior_figures/", date, "/invader_", i, "_inter_alphas_ricker_", date, ".png"), width = 12, height = 8)
  
}


for(i in species){
  
  invader <- ggplot(ricker_posteriors_long[ricker_posteriors_long$species == i,], aes(x = alpha_value, fill = treatment, line = treatment)) + 
    geom_density() + 
    facet_wrap(~alpha_name, ncol = 4) +
    scale_fill_manual(values = c("#003366", "#FFA630")) +
    ggtitle(paste0(i, " invader")) +
    geom_vline(xintercept = 0, linetype = "dashed")
  
  ggsave(invader, file=paste0("models/CW/ricker_model/random_effects_block/negative_binomial/posterior_figures/", date, "/invader_", i, "_inter_alphas_ricker_", date, "_samescale.png"), width = 12, height = 8)
  
}

## Lambda ####
### Together ####
ggplot(ricker_post, aes()) + 
  geom_density(aes(x = lambda_d), linetype = "dashed") +
  geom_density(aes(x=lambda_c))+
  facet_wrap(~species, ncol = 4, scales = "free") +
  xlab("Lambda, (dashed = D, solid = C)") +
  ylab("Density") +
  #scale_fill_manual(values = c("#003366", "#FFA630")) +
  ggtitle("Lambda, Neg binom w/random effects")

ggsave(paste0("models/CW/ricker_model/random_effects_block/negative_binomial/posterior_figures/", date, "/lambda.png"), width = 12, height = 8)


mean_alphas <- ricker_posteriors_long %>%
  group_by(alpha_name, species, treatment) %>%
  summarise(mean_alpha = mean(alpha_value),
            se_alpha = calcSE(alpha_value))

ggplot(mean_alphas, aes(x=alpha_name, y=mean_alpha, color = treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_alpha-se_alpha, ymax=mean_alpha+se_alpha)) +
  scale_color_manual(values = c("#003366", "#FFA630"))+
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~species) +
  theme(axis.text.x = element_text(angle = 50, hjust = 0.95))

ggsave(paste0("models/CW/ricker_model/random_effects_block/negative_binomial/posterior_figures/", date, "/mean_alphas_facet_sp.png"), width = 14, height = 8)

ggplot(mean_alphas, aes(x=species, y=mean_alpha, color = treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_alpha-se_alpha, ymax=mean_alpha+se_alpha)) +
  scale_color_manual(values = c("#003366", "#FFA630"))+
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~alpha_name) +
  theme(axis.text.x = element_text(angle = 50, hjust = 0.95))

ggsave(paste0("models/CW/ricker_model/random_effects_block/negative_binomial/posterior_figures/", date, "/mean_alphas_facet_alpha.png"), width = 14, height = 8)
