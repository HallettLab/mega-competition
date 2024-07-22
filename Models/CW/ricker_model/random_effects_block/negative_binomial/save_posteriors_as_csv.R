## save model outputs, correctly formatted for structural analyses

source("models/CW/ricker_model/random_effects_block/negative_binomial/import_ricker_posteriors_neg_binom.R")

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

alphas_post_keep = alphas_post[,51:87]

write.csv(alphas_post_keep, "data/posteriors_20240714_models.csv")

