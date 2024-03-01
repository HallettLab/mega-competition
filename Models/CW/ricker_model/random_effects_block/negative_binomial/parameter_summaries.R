
## load models
source("Models/CW/ricker_model/random_effects_block/negative_binomial/import_ricker_posteriors_neg_binom.R")

reps <- read.csv("data/replicate-info.csv")

# filter to good reps only
good_reps <- reps %>%
  filter(true.reps > 3)

# Format Lambda params ####
lambdas <- ricker_posteriors2 %>%
  select(species, lambda_base, lambda_dev) %>%
  mutate(lambda_d = lambda_base, 
         lambda_c = lambda_base + lambda_dev) %>%
  select(-lambda_base, -lambda_dev) %>%
  pivot_longer(cols = c("lambda_d", "lambda_c"), names_to = "treatment", values_to = "lambda") %>%
  mutate(treatment = ifelse(treatment == "lambda_d", "D", "C"))

lambdas_sum <- lambdas %>%
  group_by(species, treatment) %>%
  summarise(mean_parameter = mean(lambda),
            median_parameter = median(lambda),
            hdi_lo = hdi(lambda, ci = 0.95)$CI_low, 
            hdi_hi = hdi(lambda, ci = 0.95)$CI_high) %>%
  mutate(parameter_type = "lambda")

# Format Other params ####
other_params <- ricker_posteriors2 %>%
  select(species, sigma, disp_dev) %>%
  pivot_longer(cols = c("sigma", "disp_dev"), names_to = "parameter_type", values_to = "parameter") %>%
  group_by(species, parameter_type) %>%
  summarise(mean_parameter = mean(parameter), 
            median_parameter = median(parameter),
            hdi_lo = hdi(parameter, ci = 0.95)$CI_low, 
            hdi_hi = hdi(parameter, ci = 0.95)$CI_high)

# Format Random effects params ####
random_fx <- ricker_posteriors2 %>%
  select(V1:V11, species) %>%
  pivot_longer(cols = c("V1": "V11"), names_to = "parameter_type", values_to = "parameter") %>%
  group_by(species, parameter_type) %>%
  summarise(mean_parameter = mean(parameter), 
            median_parameter = median(parameter),
            hdi_lo = hdi(parameter, ci = 0.95)$CI_low, 
            hdi_hi = hdi(parameter, ci = 0.95)$CI_high)
  
# Format alphas
alphas <- ricker_posteriors2 %>%
  select(species, alpha_acam_base:alpha_weeds_dev) %>%
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
         alpha_weeds_c = alpha_weeds_base + alpha_weeds_dev) %>%
  select(1, 36:69) %>%
  pivot_longer("alpha_acam_d":"alpha_weeds_c", names_to = "alpha_name", values_to = "alpha_value") %>% ## change to long format
  mutate(treatment = ifelse(substr(alpha_name, 12, 13) == "d", "D", "C"),
         alpha_name = substr(alpha_name, 1, 10)) %>%
  group_by(species, treatment, alpha_name) %>%
  summarise(mean_parameter = mean(alpha_value), 
            median_parameter = median(alpha_value),
            hdi_lo = hdi(alpha_value, ci = 0.95)$CI_low, 
            hdi_hi = hdi(alpha_value, ci = 0.95)$CI_high) %>%
  mutate(parameter_type = alpha_name) %>%
  select(-alpha_name)

# Join lambda & alphas ####
model_params <- rbind(lambdas_sum, alphas) %>%
  select(species, treatment, parameter_type, mean_parameter, median_parameter, hdi_lo, hdi_hi)

write.csv(model_params, "data/parameter_summaries_20231218_models.csv")
