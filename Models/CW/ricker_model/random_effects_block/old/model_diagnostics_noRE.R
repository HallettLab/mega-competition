# Set up env ####
source("models/CW/ricker_model/import_ricker_posteriors.R")

# Pairs Plots ####
species <- c("ANAR", "BRHO", "GITR", "LENI", "MICA", "PLER", "TACA", "THIR", "CESO")

for(i in species){
    
    ## load desired model
    load(paste0("models/CW/ricker_model/in_development/random_effects_block/posteriors/ricker_", i, "_posteriors_pptdev_", date, ".rdata"))
    
    ## create pdf of pairs plots for each model    
    pdf(paste0("models/CW/ricker_model/in_development/random_effects_block/posterior_diagnostics/ppt_dev_NO_RE_20230914_models/", "alpha_", tolower(i), "_", date, ".png"), width = 20, height = 20)
    
    ## code for the plot
    ## only include parameters that we will be using in models (i.e. drop the weed alphas)
    pairs(tmp, pars = c("lambda_base", "lambda_dev", "alpha_acam_base", "alpha_acam_dev", "alpha_amme_base", "alpha_amme_dev", "alpha_anar_base", "alpha_anar_dev", "alpha_brho_base", "alpha_brho_dev", "alpha_brni_base", "alpha_brni_dev", "alpha_ceso_base", "alpha_ceso_dev", "alpha_gitr_base", "alpha_gitr_dev", "alpha_leni_base", "alpha_leni_dev", "alpha_lomu_base", "alpha_mael_base", "alpha_mael_dev", "alpha_mica_base", "alpha_mica_dev", "alpha_pler_base", "alpha_pler_dev", "alpha_plno_base", "alpha_plno_dev", "alpha_taca_base", "alpha_taca_dev", "alpha_thir_base", "alpha_thir_dev", "alpha_twil_base", "alpha_twil_dev", "alpha_weeds_base", "alpha_weeds_dev"))
    
    dev.off()
    
}

# Traceplots ####
sp_trt <- names(ricker_plots)

pdf("Models/CW/ricker_model/in_development/random_effects_block/posterior_diagnostics/ppt_dev_NO_RE_20230914_models/trace_plots.pdf", width = 12, height = 8)

for(i in 1:length(sp_trt)){
  
  ## traceplots are saved in ricker_plots list
  print(ricker_plots[[i]] + ggtitle(sp_trt[i]))
  
}

dev.off()

# Problem Models ####
## Pairs Plots ####


## Trace Plots ####
### Create ####
species <- c("AMME")

trt <- c("C")

constraint <- c("25", "125", "0625")

ricker_plots_maxLpriors_constraints <- list()

for(i in species){
  for(j in trt){
    for(k in constraint) {
      
      ## load desired model
      load(paste0("models/CW/ricker_model/posteriors/seeds_", i, "_", j, "_posteriors_Ricker_meanLpriors_constrainedby_", k, ".rdata"))
      
      ## print to see n_eff and Rhat diagnostics
      print(i)
      print(j)
      print(k)
      print(tmp)
      
      ## extract model info
      tmp2 <- rstan::extract(tmp)
      
      ## create trace plots for diagnostics
      ricker_plots_maxLpriors_constraints[[paste0(i, "_", j, "_", k)]] <- traceplot(tmp, pars = names(tmp2[-20]))
      
    }
  }
}

### Save ####
sp_trt <- names(ricker_plots_maxLpriors_constraints)

pdf(paste0("models/CW/ricker_model/ricker_model_diagnostics/ricker_model_trace_plots_meanLpriors_constrained_", sp_trt, ".pdf"), width = 12, height = 8)

for(i in 1:length(sp_trt)){
  
  ## traceplots are saved in ricker_plots list
  print(ricker_plots_maxLpriors_constraints[[i]] + ggtitle(sp_trt[i]))
  
}

dev.off()


## Correlation Matrices ####
## load desired model

## ACAM_D, 
i <- "ACAM"
j <- "D"

load(paste0("models/CW/ricker_model/posteriors/seeds_", i, "_", j, "_posteriors_Ricker_meanLpriors_constrainedby_125.rdata"))


#load(paste0("models/CW/ricker_model/posteriors/lambda_prior_max/seeds_", i, "_", j, "_posteriors_Ricker.rdata"))

tmp2 <- rstan::extract(tmp)

cor(tmp2$lambda, tmp2$alpha_acam)

## AMME_C
i <- "AMME"
j <- "C"

load(paste0("models/CW/ricker_model/posteriors/seeds_", i, "_", j, "_posteriors_Ricker_maxLpriors_constrainedby_0625.rdata"))

load(paste0("models/CW/ricker_model/posteriors/lambda_prior_max/seeds_", i, "_", j, "_posteriors_Ricker.rdata"))

tmp2 <- rstan::extract(tmp)

cor(tmp2$lambda, tmp2$alpha_amme)

## BRNI_C
i <- "BRNI"
j <- "C"

load(paste0("models/CW/ricker_model/posteriors/seeds_", i, "_", j, "_posteriors_Ricker_meanLpriors_constrainedby_02.rdata"))

tmp2 <- rstan::extract(tmp)

cor(tmp2$lambda, tmp2$alpha_brni)

## BRNI_D, 
i <- "BRNI"
j <- "D"

load(paste0("models/CW/ricker_model/posteriors/seeds_", i, "_", j, "_posteriors_Ricker_meanLpriors_constrainedby_02.rdata"))

tmp2 <- rstan::extract(tmp)

cor(tmp2$lambda, tmp2$alpha_brni)

## MAEL_D

i <- "MAEL"
j <- "D"

load(paste0("models/CW/ricker_model/posteriors/lambda_prior_max/seeds_", i, "_", j, "_posteriors_Ricker.rdata"))

load(paste0("models/CW/ricker_model/posteriors/seeds_", i, "_", j, "_posteriors_Ricker_maxLpriors_constrainedby_02.rdata"))

tmp2 <- rstan::extract(tmp)

cor(tmp2$lambda, tmp2$alpha_mael)