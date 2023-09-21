## Extract Model Posteriors 

## set up env
library(rstan)
library(bayesplot)
library(ggplot2)
library(tidyverse)

model.date <- 20230914

# Extract Posteriors ####
## List Form ####
#species <- c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL")

species <- c("ANAR", "BRHO", "GITR", "LENI", "MICA", "PLER",  "TACA", "THIR", "CESO")

#trt <- c("C","D")
ricker_posteriors <- list()
ricker_plots <- list()

for(i in species){
  #for(j in trt){
    
    ## load non-constrained models
    load(paste0("models/CW/ricker_model/in_development/random_effects_block/posteriors/ricker_", i, "_posteriors_pptdev_", model.date, ".rdata"))
    
    ## print to see model that is loading
    print(i)
    #print(j)
    
    print(tmp)
    
    ## extract model info
    tmp2 <- rstan::extract(tmp)
    
    ## create trace plots for diagnostics
    ricker_plots[[paste0(i)]] <- traceplot(tmp, pars = names(tmp2[-20]))
    
    ## save posterior distributions
    ricker_posteriors[[paste0(i)]] <- tmp2[-20]
    
  #}
}

## DF Form ####
# unlist each of the species/rainfall combinations
ricker_posteriors2 <- data.frame()

for(i in species){
#  for(j in trt){
    tmp <- as_tibble(do.call("cbind", ricker_posteriors[[paste0(i)]])) %>%
      mutate(species = i)
    ricker_posteriors2 <- rbind(ricker_posteriors2, tmp) 
 # }
}


## posterior math
ricker_posteriors_final <- ricker_posteriors2 %>%
  mutate(lambda_dry = lambda_base,
         lambda_wet = lambda_base + lambda_dev, 
         alpha_acam_dry = alpha_acam_base, 
         alpha_acam_wet = alpha_acam_base + alpha_acam_dev, 
         alpha_amme_dry = alpha_amme_base, 
         alpha_amme_wet = alpha_amme_base + alpha_amme_dev,
         alpha_anar_dry = alpha_anar_base,
         alpha_anar_wet = alpha_anar_base + alpha_anar_dev, 
         alpha_brho_dry = alpha_brho_base, 
         alpha_brho_wet = alpha_brho_base + alpha_brho_dev,
         alpha_brni_dry = alpha_brni_base,
         alpha_brni_wet = alpha_brni_base + alpha_brni_dev,
         alpha_ceso_dry = alpha_ceso_base,
         alpha_ceso_wet = alpha_ceso_base + alpha_ceso_dev, 
         alpha_gitr_dry = alpha_gitr_base, 
         alpha_gitr_wet = alpha_gitr_base + alpha_gitr_dev,
         alpha_leni_dry = alpha_leni_base,
         alpha_leni_wet = alpha_leni_base + alpha_leni_dev,
         alpha_lomu_dry = alpha_lomu_base, 
         #alpha_lomu_wet = alpha_lomu_base + alpha_lomu_dev,
         alpha_mael_dry = alpha_mael_base, 
         alpha_mael_wet = alpha_mael_base + alpha_mael_dev,
         alpha_mica_dry = alpha_mica_base,
         alpha_mica_wet = alpha_mica_base + alpha_mica_dev,
         alpha_pler_dry = alpha_pler_base,
         alpha_pler_wet = alpha_pler_base + alpha_pler_dev,
         alpha_plno_dry = alpha_plno_base,
         alpha_plno_wet = alpha_plno_base + alpha_plno_dev, 
         alpha_taca_dry = alpha_taca_base,
         alpha_taca_wet = alpha_taca_base + alpha_taca_dev,
         alpha_thir_dry = alpha_thir_base,
         alpha_thir_wet = alpha_thir_base + alpha_thir_dev,
         alpha_twil_dry = alpha_twil_base, 
         alpha_twil_wet = alpha_twil_base + alpha_twil_dev,
         alpha_weeds_dry = alpha_weeds_base,
         alpha_weeds_wet = alpha_weeds_base + alpha_weeds_dev)





