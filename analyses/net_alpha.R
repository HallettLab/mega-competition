
source("Models/CW/ricker_model/in_development/random_effects_block/read_in_tweaked_models_noRE.R")

theme_set(theme_classic())

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

## calculate net interaction strengths 
alphas <- ricker_posteriors_final %>%
  #select(-lambda_dry, -lambda_wet) %>%
  pivot_longer(cols = c("alpha_acam_dry", "alpha_acam_wet", "alpha_amme_dry", "alpha_amme_wet", "alpha_anar_dry", "alpha_anar_wet", "alpha_brho_dry", "alpha_brho_wet", "alpha_brni_dry", "alpha_brni_wet", "alpha_ceso_dry", "alpha_ceso_wet", "alpha_gitr_dry", "alpha_gitr_wet", "alpha_leni_dry", "alpha_leni_wet", "alpha_lomu_dry", "alpha_mael_dry", "alpha_mael_wet", "alpha_mica_dry", "alpha_mica_wet", "alpha_pler_dry", "alpha_pler_wet", "alpha_plno_dry", "alpha_plno_wet", "alpha_taca_dry", "alpha_taca_wet", "alpha_thir_dry", "alpha_thir_wet", "alpha_twil_dry", "alpha_twil_wet",
                        "alpha_weeds_dry", "alpha_weeds_wet"), names_to = "alpha_type", values_to = "alpha") %>%
  select(species, alpha_type, alpha) %>%
  filter(!alpha_type %in% c("alpha_weeds_dry", "alpha_weeds_wet")) %>%
  mutate(treatment = substr(alpha_type, 12,14), 
         alpha_type = substr(alpha_type, 1, 10))

ggplot(alphas, aes(x=alpha, fill = treatment)) +
  geom_density() +
  scale_fill_manual(values = c("#FFA630", "#003366")) +
  #geom_density(aes(x=lambda_dry)) +
  facet_wrap(~alpha_type, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed")


## net strength
netalpha <- alphas %>%
  group_by(treatment, alpha_type) %>%
  summarise(net_alpha = mean(alpha), se_alpha = calcSE(alpha),
            max_alpha = max(alpha),
            min_alpha = min(alpha), 
            median_alpha = median(alpha))


ggplot(netalpha, aes(x=alpha_type, y=net_alpha, color = treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin = net_alpha - (2*se_alpha), ymax = net_alpha + 2*(se_alpha)), width = 0.5) 


ggplot(netalpha, aes(x=alpha_type, y=median_alpha)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed")
## could bootstrap to get error bars?









