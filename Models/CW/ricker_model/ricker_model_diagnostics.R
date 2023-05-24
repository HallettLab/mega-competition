# Set up env ####
source("models/CW/ricker_model/import_ricker_posteriors.R")

# Pairs Plots ####
trt <- c("C","D")
species <- c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL")

for(i in species){
  for(j in trt) {
    
    ## load desired model
    load(paste0("models/CW/ricker_model/posteriors/seeds_", i, "_", j, "_posteriors_Ricker_meanLpriors.rdata"))

    ## create pdf of pairs plots for each model    
    pdf(paste0("models/CW/ricker_model/ricker_model_diagnostics/ricker_model_pairs_plots_", i, "_", j, "_meanLpriors.pdf"), width = 20, height = 20)
    
    ## code for the plot
    ## only include parameters that we will be using in models (i.e. drop the weed alphas)
    pairs(tmp, pars = c("lambda", "alpha_acam", "alpha_amme", "alpha_anar", "alpha_brho", "alpha_brni", "alpha_ceso", "alpha_gitr", "alpha_leni", "alpha_lomu", "alpha_mael", "alpha_mica", "alpha_pler", "alpha_plno", "alpha_taca", "alpha_thir",  "alpha_twil"))
    
    dev.off()
    
  }
}

# Traceplots ####
sp_trt <- names(ricker_plots)

pdf("models/CW/ricker_model/ricker_model_diagnostics/ricker_model_trace_plots_meanLpriors.pdf", width = 12, height = 8)

for(i in 1:length(sp_trt)){
  
  ## traceplots are saved in ricker_plots list
  print(ricker_plots[[i]] + ggtitle(sp_trt[i]))
  
}

dev.off()

# Correlation Matrices ####
## load desired model

## ACAM_D, AMME_C, BRNI_C, BRNI_D, MAEL_D

i <- "ACAM"
j <- "D"

load(paste0("models/CW/ricker_model/posteriors/lambda_prior_mean/seeds_", i, "_", j, "_posteriors_Ricker_meanLpriors.rdata"))

cor(tmp, pars = c("lambda", "alpha_acam", "alpha_amme", "alpha_anar", "alpha_brho", "alpha_brni", "alpha_ceso", "alpha_gitr", "alpha_leni", "alpha_lomu", "alpha_mael", "alpha_mica", "alpha_pler", "alpha_plno", "alpha_taca", "alpha_thir",  "alpha_twil"))



