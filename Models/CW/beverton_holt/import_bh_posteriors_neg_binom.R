## Extract Model Posteriors 

## set up env
library(rstan)
library(bayesplot)
library(ggplot2)
library(tidyverse)
theme_set(theme_classic())

# Extract Posteriors ####
## List Form ####
species <- c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL")

date <- 20231218

ricker_posteriors <- list()

for(i in species){

    ## load non-constrained models
    load(paste0("models/CW/ricker_model/random_effects_block/negative_binomial/posteriors/ricker_",i, "_posteriors_random_effects_neg_binomial", date, ".rdata"))
    
    ## print to see model that is loading
    print(i)
    
    ## extract model info
    tmp2 <- rstan::extract(PrelimFit)
    
    ## save posterior distributions
    ricker_posteriors[[paste0(i)]] <- tmp2
    
  }

## DF Form ####
# unlist each of the species/rainfall combinations
ricker_posteriors2 <- data.frame()

for(i in species){
  
    tmp <- as_tibble(do.call("cbind", ricker_posteriors[[paste0(i)]])) %>%
      mutate(species = i)
    ricker_posteriors2 <- rbind(ricker_posteriors2, tmp) 
    
}
