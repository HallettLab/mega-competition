# extract posteriors
library(rstan)
library(bayesplot)
library(ggplot2)
library(tidyverse)

### Extract means of each parameter ####
species <- c("PLER", "BRHO", "GITR", "AVBA", "ANAR",  "TACA", "LOMU", "TWIL", "THIR", "CESO", "MICA", "AMME", "PLNO", "ACAM", "BRNI", "LENI", "CLPU", "MAEL")


trt <- c("C","D")
params <- data.frame()
posteriors <- list()
plots <- list()

for(i in species){
  for(j in trt){
    load(paste0("models/CW/posteriors/seeds_", i, "_", j, "_posteriors_Ricker.rdata"))
    print(tmp)
    tmp2 <- rstan::extract(tmp)
    plots[[paste0(i, "_", j)]] <- traceplot(tmp, pars = names(tmp2[-20]))
    posteriors[[paste0(i,"_",j)]] <- tmp2[-20]
    tmp3 <- as.data.frame(lapply(tmp2, mean))[-20]
    tmp3$species <- i
    tmp3$treatment <- j
    params <- rbind(params,tmp3)
  }
}



### Extract and inspect distributions ####
# unlist each of the species/rainfall combinations
posteriors2 <- data.frame()

for(i in species){
  for(j in trt){
    tmp <- as_tibble(do.call("cbind", posteriors[[paste0(i,"_",j)]])) %>%
      mutate(species = i, treatment = j)
    posteriors2 <- rbind(posteriors2, tmp) 
  }
}


## change to long data format
#posteriors_long <- posteriors2 %>%
 # pivot_longer(2:24, names_to = "alpha_name", values_to = "alpha_value")