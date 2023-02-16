# extract posteriors
library(rstan)
library(bayesplot)
library(ggplot2)
library(tidyverse)

### Extract means of each parameter ####
species <- c("PLER", "BRHO", "GITR", "AVBA", "ANAR", "MAEL", "CLPU", "TACA", "LOMU", "TWIL", "THIR", "CESO", "MICA", "AMME", "PLNO", "ACAM")

trt <- c("C","D")
params <- data.frame()
posteriors <- list()
plots <- list()

for(i in species){
  for(j in trt){
    load(paste0("Models/ML/Posteriors/seeds_", i, "_", j, "_posteriors.rdata"))
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


# inspect dens plots
# species having a hard time reaching equilibrium
## dry: TWIL, CESO, PLNO
## wet: TWIL, THIR, AMME, ACAM
ggplot(posteriors2, aes(x = lambda, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_pler, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_brho, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_gitr, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_acam, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_avba, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_anar, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_mael, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_clpu, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_taca, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_lomu, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_twil, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_thir, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_ceso, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_mica, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_amme, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

ggplot(posteriors2, aes(x = alpha_plno, fill = treatment, line = treatment)) + 
  geom_density() + 
  facet_wrap(~species, ncol = 3, scales = "free")

rm(params, tmp, tmp2, tmp3, i, j)

# check scales of lambdas and alphas ####
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

posteriors2$scaled <- posteriors2$lambda/(1+rowSums(posteriors2[2:19]))

posteriors2.sum <- posteriors2 %>%
  group_by(species, treatment) %>%
  summarize(lambda.scaled.mean = mean(scaled),
         lambda.scaled.se = calcSE(scaled))
