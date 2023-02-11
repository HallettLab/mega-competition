# extract posteriors
library(rstan)
library(bayesplot)
library(ggplot2)
library(tidyverse)

### Extract means of each parameter ####
species <- c("PLER", "BRHO", "GITR", "ACAM", "AVBA", "ANAR", "MAEL", 
 "CLPU", "TACA", "LOMU", "TWIL", "THIR", "CESO", "MICA", "AMME", "PLNO")
trt <- c("C","D")
params <- data.frame()
posteriors <- list()

for(i in species){
  for(j in trt){
    load(paste0("Models/Posteriors/seeds_", i, "_", j, "_posteriors.rdata"))
    tmp2 <- rstan::extract(tmp)
    posteriors[[paste0(i,"_",j)]] <- tmp2
    tmp3 <- as.data.frame(lapply(tmp2, mean))[-20]
    tmp3$species <- i
    tmp3$treatment <- j
    params <-rbind(params,tmp3)
  }
}

### Extract and inspect distributions ####
# unlist each of the species/rainfall combinations
posteriors2 <- data.frame()

for(i in species){
  for(j in trt){
    tmp <- as_tibble(do.call("cbind", posteriors[[paste0(i,"_",j)]])) %>%
      mutate(species = i, treatment = j) %>%
      select(-lp__)
    posteriors2 <- rbind(posteriors2, tmp) 
  }
}


# inspect dens plots

ggplot(posteriors2, aes(x = lambda, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")


ggplot(posteriors2, aes(x = alpha_avba, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")


ggplot(posteriors2, aes(x = alpha_brho, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")


