## Model diagnostics

date <- 20231204
species <- "BRHO"

## load model back in if needed
load(paste0("Models/CW/ricker_model/random_effects_block/posteriors/ricker_BRHO_posteriors_random_effects_", date, ".rdata"))

# Diagnostics ####
## check Rhat vals ####
print(PrelimFit)

## Traceplots ####
### epsilon/sigma
traceplot(PrelimFit, pars = c("epsilon[1]", "epsilon[2]", "epsilon[3]", "epsilon[4]", "epsilon[5]", "epsilon[6]", "epsilon[7]", "epsilon[8]", "epsilon[9]", "epsilon[10]", "epsilon[11]", "sigma"))

ggsave(paste0("Models/CW/ricker_model/random_effects_block/posterior_diagnostics/", date, "/", species, "_random_effects_traceplot_", date, ".png"), width = 8, height = 4)

### lambda_base
traceplot(PrelimFit, pars = c("lambda_base", "lambda_dev", "disp_dev"))

ggsave(paste0("Models/CW/ricker_model/random_effects_block/posterior_diagnostics/", date, "/", species, "lambda_traceplot_", date, ".png"), width = 6, height = 3)

### alphas 
#### part 1
traceplot(PrelimFit, pars = c("alpha_acam_base", "alpha_acam_dev", "alpha_amme_base", "alpha_amme_dev", "alpha_anar_base", "alpha_anar_dev", "alpha_brho_base", "alpha_brho_dev", "alpha_brni_base", "alpha_brni_dev", "alpha_ceso_base", "alpha_ceso_dev"))

ggsave(paste0("Models/CW/ricker_model/random_effects_block/posterior_diagnostics/", date, "/", species, "_alphas1_traceplot_", date, ".png"), width = 8, height = 4)

#### part 2
traceplot(PrelimFit, pars = c("alpha_gitr_base", "alpha_gitr_dev", "alpha_leni_base", "alpha_leni_dev", "alpha_lomu_base", "alpha_lomu_dev", "alpha_mael_base", "alpha_mael_dev", "alpha_mica_base", "alpha_mica_dev", "alpha_pler_base", "alpha_pler_dev"))

ggsave(paste0("Models/CW/ricker_model/random_effects_block/posterior_diagnostics/", date, "/", species, "_alphas2_traceplot_", date, ".png"), width = 8, height = 4)


#### part 3
traceplot(PrelimFit, pars = c("alpha_plno_base", "alpha_plno_dev", "alpha_taca_base", "alpha_taca_dev", "alpha_thir_base", "alpha_thir_dev", "alpha_twil_base", "alpha_twil_dev", "alpha_weeds_base", "alpha_weeds_dev"))

ggsave(paste0("Models/CW/ricker_model/random_effects_block/posterior_diagnostics/", date, "/", species, "_alphas3_traceplot_", date, ".png"), width = 8, height = 4)




## Pairs plots ####
### epsilon/sigma/lambda
png(file = paste0("Models/CW/ricker_model/random_effects_block/posterior_diagnostics/", date, "/", species, "_pairs_plot_epsilon_sigma_lambda.png"), width = 1800, height = 1800)

pairs(PrelimFit, pars = c("epsilon[1]", "epsilon[2]", "epsilon[3]", "epsilon[4]", "epsilon[5]", "epsilon[6]", "epsilon[7]", "epsilon[8]", "epsilon[9]", "epsilon[10]", "epsilon[11]", "sigma", "lambda_base", "lambda_dev"))

dev.off()

### alpha 1/lambda
png(file = paste0("Models/CW/ricker_model/random_effects_block/posterior_diagnostics/", date, "/", species, "_pairs_plot_lambda_alphas1.png"), width = 1800, height = 1800)

pairs(PrelimFit, pars = c("lambda_base", "lambda_dev", "alpha_acam_base", "alpha_acam_dev", "alpha_amme_base", "alpha_amme_dev", "alpha_anar_base", "alpha_anar_dev", "alpha_brho_base", "alpha_brho_dev", "alpha_brni_base", "alpha_brni_dev", "alpha_ceso_base", "alpha_ceso_dev"))

dev.off()

### alpha 2/lambda
png(file = paste0("Models/CW/ricker_model/random_effects_block/posterior_diagnostics/", date, "/", species, "_pairs_plot_lambda_alphas2.png"), width = 1800, height = 1800)

pairs(PrelimFit, pars = c("lambda_base", "lambda_dev", "alpha_gitr_base", "alpha_gitr_dev", "alpha_leni_base", "alpha_leni_dev", "alpha_lomu_base", "alpha_lomu_dev", "alpha_mael_base", "alpha_mael_dev", "alpha_mica_base", "alpha_mica_dev", "alpha_pler_base", "alpha_pler_dev"))

dev.off()

### alpha 3/lambda
png(file = paste0("Models/CW/ricker_model/random_effects_block/posterior_diagnostics/", date, "/", species, "_pairs_plot_lambda_alphas3.png"), width = 1800, height = 1800)

pairs(PrelimFit, pars = c("lambda_base", "lambda_dev", "alpha_plno_base", "alpha_plno_dev", "alpha_taca_base", "alpha_taca_dev", "alpha_thir_base", "alpha_thir_dev", "alpha_twil_base", "alpha_twil_dev", "alpha_weeds_base", "alpha_weeds_dev"))

dev.off()


### alpha, epsilon, sigma
png(file = paste0("Models/CW/ricker_model/random_effects_block/posterior_diagnostics/", date, "/", species, "_pairs_plot_epsilon_sigma_alpha1.png"), width = 1800, height = 1800)

pairs(PrelimFit, pars = c("epsilon[1]", "epsilon[2]", "epsilon[3]", "epsilon[4]", "epsilon[5]", "epsilon[6]", "sigma", "alpha_acam_base", "alpha_acam_dev", "alpha_amme_base", "alpha_amme_dev", "alpha_anar_base", "alpha_anar_dev", "alpha_brho_base", "alpha_brho_dev"))

dev.off()


png(file = paste0("Models/CW/ricker_model/random_effects_block/posterior_diagnostics/", date, "/", species, "_pairs_plot_epsilon_sigma_2_alpha1.png"), width = 1800, height = 1800)

pairs(PrelimFit, pars = c("epsilon[7]", "epsilon[8]", "epsilon[9]", "epsilon[10]", "epsilon[11]", "sigma", "alpha_acam_base", "alpha_acam_dev", "alpha_amme_base", "alpha_amme_dev", "alpha_anar_base", "alpha_anar_dev", "alpha_brho_base", "alpha_brho_dev"))

dev.off()
