
# Read in model posteriors ####
source("Models/CW/ricker_model/in_development/random_effects_block/read_in_tweaked_models.R")

theme_set(theme_classic())

# Tweak dfs ####
lambda <- ricker_posteriors_final %>%
  select(species, lambda_dry, lambda_wet) %>%
  pivot_longer(cols = c("lambda_dry", "lambda_wet"), names_to = "treatment", values_to = "lambda")

alphas <- ricker_posteriors_final %>%
  #select(-lambda_dry, -lambda_wet) %>%
  pivot_longer(cols = c("alpha_acam_dry", "alpha_acam_wet", "alpha_amme_dry", "alpha_amme_wet", "alpha_anar_dry", "alpha_anar_wet", "alpha_brho_dry", "alpha_brho_wet", "alpha_brni_dry", "alpha_brni_wet", "alpha_ceso_dry", "alpha_ceso_wet", "alpha_gitr_dry", "alpha_gitr_wet", "alpha_leni_dry", "alpha_leni_wet", "alpha_lomu_dry", "alpha_mael_dry", "alpha_mael_wet", "alpha_mica_dry", "alpha_mica_wet", "alpha_pler_dry", "alpha_pler_wet", "alpha_plno_dry", "alpha_plno_wet", "alpha_taca_dry", "alpha_taca_wet", "alpha_thir_dry", "alpha_thir_wet", "alpha_twil_dry", "alpha_twil_wet",
"alpha_weeds_dry", "alpha_weeds_wet"), names_to = "alpha_type", values_to = "alpha") %>%
  select(species, alpha_type, alpha) %>%
  filter(!alpha_type %in% c("alpha_weeds_dry", "alpha_weeds_wet")) %>%
  mutate(treatment = substr(alpha_type, 12,14), 
         alpha_type = substr(alpha_type, 1, 10))

# visualize ####
## lambda ####
ggplot(lambda, aes(x=lambda, fill = treatment)) +
  geom_density() +
  scale_fill_manual(values = c("#003366", "#FFA630")) +
  #geom_density(aes(x=lambda_dry)) +
  facet_wrap(~species, scales = "free")

## alphas ####
ggplot(alphas, aes(x=alpha, fill = treatment)) +
  geom_density() +
  scale_fill_manual(values = c("#003366", "#FFA630")) +
  #geom_density(aes(x=lambda_dry)) +
  facet_wrap(~alpha_type, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed")

## individual plots of alphas
species <- unique(alphas$species)

for(i in species){
  
  alpha <- ggplot(alphas[alphas$alpha_type == paste0("alpha_", tolower(i)),], aes(x = alpha, fill = treatment, line = treatment)) + 
    geom_density() + 
    facet_wrap(~species, ncol = 5, scales = "free")+
    scale_fill_manual(values = c("#FFA630", "#003366")) +
    ggtitle("Ricker Model") + xlab(paste0("alpha_", tolower(i))) +
    geom_vline(xintercept = 0, linetype = "dashed")
  
  ggsave(alpha, file=paste0("models/CW/ricker_model/in_development/random_effects_block/posterior_figures/ppt_dev_NO_RE_20230914_models/", "alpha_", tolower(i), "_", model.date, ".png"), width = 12, height = 5)
  
  invader <- ggplot(alphas[alphas$species == i,], aes(x = alpha, fill = treatment, line = treatment)) + 
    geom_density() + 
    facet_wrap(~alpha_type, ncol = 4, scales = "free") +
    scale_fill_manual(values = c("#FFA630", "#003366")) +
    ggtitle(paste0(i, " invader")) +
    geom_vline(xintercept = 0, linetype = "dashed")
  
  ggsave(invader, file=paste0("models/CW/ricker_model/in_development/random_effects_block/posterior_figures/ppt_dev_NO_RE_20230914_models/invader_", tolower(i), "_", model.date, ".png"), width = 12, height = 8)
  
}


