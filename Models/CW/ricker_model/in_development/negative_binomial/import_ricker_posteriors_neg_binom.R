## Extract Model Posteriors 

## set up env
library(rstan)
library(bayesplot)
library(ggplot2)
library(tidyverse)
theme_set(theme_bw())

# Extract Posteriors ####
## List Form ####
#species <- c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL")
## CLPU, AVBA

species <- c("PLER")
trt <- c("C")

date <- 20230831

ricker_posteriors <- list()
ricker_plots <- list()

for(i in species){
  for(j in trt){
    
    ## load non-constrained models
    load(paste0("models/CW/ricker_model/in_development/negative_binomial/posteriors/seeds_",i,"_",j,"_posteriors_Ricker_meanLpriors_neg_binom_", date, ".rdata"))
    
    ## print to see model that is loading
    print(i)
    print(j)
  #  print(k)
    
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
