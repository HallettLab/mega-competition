## Extract Model Posteriors 

## set up env
library(rstan)
library(bayesplot)
library(ggplot2)
library(tidyverse)
theme_set(theme_bw())

# Extract Posteriors ####
## List Form ####
species <- c("PLER", "BRHO", "GITR", "AVBA", "ANAR",  "TACA", "LOMU", "TWIL", "THIR", "CESO", "MICA", "AMME", "PLNO", "ACAM", "BRNI", "LENI", "CLPU", "MAEL")

trt <- c("C","D")
BH_posteriors <- list()
BH_plots <- list()

for(i in species){
  for(j in trt){
    
    ## load desired model
    load(paste0("models/CW/posteriors/seeds_", i, "_", j, "_posteriors_BH_filtered_dat.rdata"))
    
    ## print to see n_eff and Rhat diagnostics
    print(i)
    print(j)
    print(tmp)
    
    ## extract model info
    tmp2 <- rstan::extract(tmp)
    
    ## create trace plots for diagnostics
    BH_plots[[paste0(i, "_", j)]] <- traceplot(tmp, pars = names(tmp2[-20]))
    
    ## save posterior distributions
    BH_posteriors[[paste0(i,"_",j)]] <- tmp2[-20]

  }
}


## DF Form ####
# unlist each of the species/rainfall combinations
BH_posteriors2 <- data.frame()

for(i in species){
  for(j in trt){
    tmp <- as_tibble(do.call("cbind", BH_posteriors[[paste0(i,"_",j)]])) %>%
      mutate(species = i, treatment = j)
    BH_posteriors2 <- rbind(BH_posteriors2, tmp) 
  }
}
