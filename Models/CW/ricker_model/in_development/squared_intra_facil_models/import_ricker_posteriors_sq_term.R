## Extract Model Posteriors 

## set up env
library(rstan)
library(bayesplot)
library(ggplot2)
library(tidyverse)
theme_set(theme_bw())

# Extract Posteriors ####
## List Form ####
sp_trt <- c("ACAM_C", "AMME_C", "PLNO_D", "THIR_C", "THIR_D")
#"TWIL_C", "TWIL_D", "CESO_D", 

## problem models
constrained50 <- c("AMME_C", "BRNI_C", "BRNI_D", "MAEL_D")
constrained16 <- c("ACAM_D")

#trt <- c("C","D")
ricker_posteriors_facil <- list()
#ricker_plots <- list()

for(i in sp_trt){
 # for(j in trt){
    
  ## get species
 # sp <- substr(i, 1, 4)
  #trt <- substr(i, 6, 6)
    
  ## set k based on which model is being loaded
  
  if(i %in% constrained50) {
      k <- 50
  } else if (i %in% constrained16) {
      k <- 16
  } else {
      k <- "none"
  }
    
    ## load non-constrained models
    load(paste0("models/CW/ricker_model/squared_intra_facil_models/posteriors/seeds_", i, "_posteriors_Ricker_maxLpriors_constrainedby_", k, "_sq_term.rdata"))
    
    ## print to see model that is loading
    print(i)
    #print(j)
    print(k)
    
    ## extract model info
    tmp2 <- rstan::extract(tmp)
    
    ## create trace plots for diagnostics
    #ricker_plots[[paste0(i, "_", j)]] <- traceplot(tmp, pars = names(tmp2[-20]))
    
    ## save posterior distributions
    ricker_posteriors_facil[[i]] <- tmp2[-20]
    
}

## DF Form ####
## Can't be put in a dataframe together as column names do not match... 

acam_c_facil <- as_tibble(do.call("cbind", ricker_posteriors_facil[["ACAM_C"]])) %>%
      mutate(species = "ACAM", treatment = "C")

amme_c_facil <- as_tibble(do.call("cbind", ricker_posteriors_facil[["AMME_C"]])) %>%
  mutate(species = "AMME", treatment = "C")

plno_d_facil <- as_tibble(do.call("cbind", ricker_posteriors_facil[["PLNO_D"]])) %>%
  mutate(species = "PLNO", treatment = "D")

thir_d_facil <- as_tibble(do.call("cbind", ricker_posteriors_facil[["THIR_D"]])) %>%
  mutate(species = "THIR", treatment = "D")

thir_c_facil <- as_tibble(do.call("cbind", ricker_posteriors_facil[["THIR_C"]])) %>%
  mutate(species = "THIR", treatment = "C")

