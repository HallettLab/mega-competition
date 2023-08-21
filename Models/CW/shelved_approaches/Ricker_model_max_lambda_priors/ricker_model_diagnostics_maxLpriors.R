# Set up env ####
source("models/CW/ricker_model/import_ricker_posteriors.R")

# Pairs Plots ####
#trt <- c("C","D")
trt <- c("D")
#species <- c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL")

#problem.species <- c("ACAM", "AMME", "BRNI", "MAEL")
problem.species <- c("ACAM")

for(i in problem.species){
  for(j in trt) {
    
    ## load desired model
    load(paste0("models/CW/ricker_model/posteriors/seeds_", i, "_", j, "_posteriors_Ricker_maxLpriors_constrainedby_25.rdata"))
    
    ## create pdf of pairs plots for each model    
    pdf(paste0("models/CW/ricker_model/ricker_model_diagnostics/ricker_model_pairs_plots_", i, "_", j, "_maxLpriors_constrainedby_25.pdf"), width = 20, height = 20)
    
    ## code for the plot
    ## only include parameters that we will be using in models (i.e. drop the weed alphas)
    pairs(tmp, pars = c("lambda", "alpha_acam", "alpha_amme", "alpha_anar", "alpha_brho", "alpha_brni", "alpha_ceso", "alpha_gitr", "alpha_leni", "alpha_lomu", "alpha_mael", "alpha_mica", "alpha_pler", "alpha_plno", "alpha_taca", "alpha_thir",  "alpha_twil"))
    
    dev.off()
    
  }
}

# Traceplots ####
sp_trt <- names(ricker_plots)

pdf("models/CW/ricker_model/ricker_model_diagnostics/ricker_model_trace_plots_meanLpriors_constrained.pdf", width = 12, height = 8)

for(i in 1:length(sp_trt)){
  
  ## traceplots are saved in ricker_plots list
  print(ricker_plots[[i]] + ggtitle(sp_trt[i]))
  
}

dev.off()

# Problem Models ####
## Pairs Plots ####
### ACAM ####
i <- "ACAM"
j <- "D"
k <- "0625" ## sd/16

## load desired model
load(paste0("models/CW/ricker_model/posteriors/seeds_", i, "_", j, "_posteriors_Ricker_maxLpriors_constrainedby_", k,".rdata"))

print(tmp)

## create pdf of pairs plots for each model    
pdf(paste0("models/CW/ricker_model/ricker_model_diagnostics/ricker_model_pairs_plots_", i, "_", j, "_maxLpriors_constrainedby_", k, ".pdf"), width = 20, height = 20)

## code for the plot
## only include parameters that we will be using in models (i.e. drop the weed alphas)
pairs(tmp, pars = c("lambda", "alpha_acam", "alpha_amme", "alpha_anar", "alpha_brho", "alpha_brni", "alpha_ceso", "alpha_gitr", "alpha_leni", "alpha_lomu", "alpha_mael", "alpha_mica", "alpha_pler", "alpha_plno", "alpha_taca", "alpha_thir",  "alpha_twil"))

dev.off()

### AMME ####
i <- "AMME"
j <- "C"
k <- "02" ## sd/50

## load desired model
load(paste0("models/CW/ricker_model/posteriors/seeds_", i, "_", j, "_posteriors_Ricker_maxLpriors_constrainedby_", k,".rdata"))

print(tmp)

## create pdf of pairs plots for each model    
pdf(paste0("models/CW/ricker_model/ricker_model_diagnostics/ricker_model_pairs_plots_", i, "_", j, "_maxLpriors_constrainedby_", k, ".pdf"), width = 20, height = 20)

## code for the plot
## only include parameters that we will be using in models (i.e. drop the weed alphas)
pairs(tmp, pars = c("lambda", "alpha_acam", "alpha_amme", "alpha_anar", "alpha_brho", "alpha_brni", "alpha_ceso", "alpha_gitr", "alpha_leni", "alpha_lomu", "alpha_mael", "alpha_mica", "alpha_pler", "alpha_plno", "alpha_taca", "alpha_thir",  "alpha_twil"))

dev.off()

### BRNI ####
#### C ####
i <- "BRNI"
j <- "C"
k <- "02" ## sd/50

## load desired model
load(paste0("models/CW/ricker_model/posteriors/seeds_", i, "_", j, "_posteriors_Ricker_maxLpriors_constrainedby_", k,".rdata"))

print(tmp)

## create pdf of pairs plots for each model    
pdf(paste0("models/CW/ricker_model/ricker_model_diagnostics/ricker_model_pairs_plots_", i, "_", j, "_maxLpriors_constrainedby_", k, ".pdf"), width = 20, height = 20)

## code for the plot
## only include parameters that we will be using in models (i.e. drop the weed alphas)
pairs(tmp, pars = c("lambda", "alpha_acam", "alpha_amme", "alpha_anar", "alpha_brho", "alpha_brni", "alpha_ceso", "alpha_gitr", "alpha_leni", "alpha_lomu", "alpha_mael", "alpha_mica", "alpha_pler", "alpha_plno", "alpha_taca", "alpha_thir",  "alpha_twil"))

dev.off()
#### D ####
i <- "BRNI"
j <- "D"
k <- "02" ## sd/50

## load desired model
load(paste0("models/CW/ricker_model/posteriors/seeds_", i, "_", j, "_posteriors_Ricker_maxLpriors_constrainedby_", k,".rdata"))

print(tmp)

## create pdf of pairs plots for each model    
pdf(paste0("models/CW/ricker_model/ricker_model_diagnostics/ricker_model_pairs_plots_", i, "_", j, "_maxLpriors_constrainedby_", k, ".pdf"), width = 20, height = 20)

## code for the plot
## only include parameters that we will be using in models (i.e. drop the weed alphas)
pairs(tmp, pars = c("lambda", "alpha_acam", "alpha_amme", "alpha_anar", "alpha_brho", "alpha_brni", "alpha_ceso", "alpha_gitr", "alpha_leni", "alpha_lomu", "alpha_mael", "alpha_mica", "alpha_pler", "alpha_plno", "alpha_taca", "alpha_thir",  "alpha_twil"))

dev.off()

### MAEL ####
i <- "MAEL"
j <- "D"
k <- "02" ## sd/50

## load desired model
load(paste0("models/CW/ricker_model/posteriors/seeds_", i, "_", j, "_posteriors_Ricker_maxLpriors_constrainedby_", k,".rdata"))

print(tmp)

## create pdf of pairs plots for each model    
pdf(paste0("models/CW/ricker_model/ricker_model_diagnostics/ricker_model_pairs_plots_", i, "_", j, "_maxLpriors_constrainedby_", k, ".pdf"), width = 20, height = 20)

## code for the plot
## only include parameters that we will be using in models (i.e. drop the weed alphas)
pairs(tmp, pars = c("lambda", "alpha_acam", "alpha_amme", "alpha_anar", "alpha_brho", "alpha_brni", "alpha_ceso", "alpha_gitr", "alpha_leni", "alpha_lomu", "alpha_mael", "alpha_mica", "alpha_pler", "alpha_plno", "alpha_taca", "alpha_thir",  "alpha_twil"))

dev.off()


## Trace Plots ####
### Create ####
species <- c("MAEL")

trt <- c("D")

constraint <- c("02")

ricker_plots_maxLpriors_constraints <- list()

for(i in species){
  for(j in trt){
    for(k in constraint) {
      
      ## load desired model
      load(paste0("models/CW/ricker_model/posteriors/seeds_", i, "_", j, "_posteriors_Ricker_maxLpriors_constrainedby_", k, ".rdata"))
      
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

pdf(paste0("models/CW/ricker_model/ricker_model_diagnostics/ricker_model_trace_plots_maxLpriors_constrained_", sp_trt, ".pdf"), width = 12, height = 8)

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

load(paste0("models/CW/ricker_model/posteriors/seeds_", i, "_", j, "_posteriors_Ricker_maxLpriors_constrainedby_125.rdata"))


#load(paste0("models/CW/ricker_model/posteriors/lambda_prior_max/seeds_", i, "_", j, "_posteriors_Ricker.rdata"))

tmp2 <- rstan::extract(tmp)

cor(tmp2$lambda, tmp2$alpha_acam)

## AMME_C
i <- "AMME"
j <- "C"

load(paste0("models/CW/ricker_model/posteriors/seeds_", i, "_", j, "_posteriors_Ricker_maxLpriors_constrainedby_02.rdata"))

load(paste0("models/CW/ricker_model/posteriors/lambda_prior_max/seeds_", i, "_", j, "_posteriors_Ricker.rdata"))

tmp2 <- rstan::extract(tmp)

cor(tmp2$lambda, tmp2$alpha_amme)

## BRNI_C
i <- "BRNI"
j <- "C"

load(paste0("models/CW/ricker_model/posteriors/seeds_", i, "_", j, "_posteriors_Ricker_maxLpriors_constrainedby_02.rdata"))

tmp2 <- rstan::extract(tmp)

cor(tmp2$lambda, tmp2$alpha_brni)

## BRNI_D, 
i <- "BRNI"
j <- "D"

load(paste0("models/CW/ricker_model/posteriors/seeds_", i, "_", j, "_posteriors_Ricker_maxLpriors_constrainedby_02.rdata"))

tmp2 <- rstan::extract(tmp)

cor(tmp2$lambda, tmp2$alpha_brni)

## MAEL_D

i <- "MAEL"
j <- "D"

load(paste0("models/CW/ricker_model/posteriors/lambda_prior_max/seeds_", i, "_", j, "_posteriors_Ricker.rdata"))

load(paste0("models/CW/ricker_model/posteriors/seeds_", i, "_", j, "_posteriors_Ricker_maxLpriors_constrainedby_02.rdata"))

tmp2 <- rstan::extract(tmp)

cor(tmp2$lambda, tmp2$alpha_mael)
