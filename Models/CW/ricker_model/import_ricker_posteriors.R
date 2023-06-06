## Extract Model Posteriors 

## set up env
library(rstan)
library(bayesplot)
library(ggplot2)
library(tidyverse)
theme_set(theme_bw())

# Extract Posteriors ####
## List Form ####
species <- c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL")
## CLPU, AVBA

## problem models
constrained50 <- c("AMME_C", "BRNI_C", "BRNI_D", "MAEL_D")
constrained16 <- c("ACAM_D")

trt <- c("C","D")
ricker_posteriors <- list()
ricker_plots <- list()

for(i in species){
  for(j in trt){
    
    ## set k based on which model is being loaded
    sp_trt <- paste0(i, "_", j)
    
    if(sp_trt %in% constrained50) {
      k <- 50
    } else if (sp_trt %in% constrained16) {
      k <- 16
    } else {
      k <- "none"
    }
    
    ## load non-constrained models
    load(paste0("models/CW/ricker_model/posteriors/lambda_prior_max/seeds_", i, "_", j, "_posteriors_Ricker_maxLpriors_constrainedby_", k, ".rdata"))
    
    ## print to see model that is loading
    print(i)
    print(j)
    print(k)

    ## extract model info
    tmp2 <- rstan::extract(tmp)

    ## create trace plots for diagnostics
    ricker_plots[[paste0(i, "_", j)]] <- traceplot(tmp, pars = names(tmp2[-20]))
    
    ## save posterior distributions
    ricker_posteriors[[paste0(i,"_",j)]] <- tmp2[-20]
    
  }
}

## DF Form ####
# unlist each of the species/rainfall combinations
ricker_posteriors2 <- data.frame()

for(i in species){
  for(j in trt){
    tmp <- as_tibble(do.call("cbind", ricker_posteriors[[paste0(i,"_",j)]])) %>%
      mutate(species = i, treatment = j)
    ricker_posteriors2 <- rbind(ricker_posteriors2, tmp) 
  }
}
