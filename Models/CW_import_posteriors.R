library(rstan)
library(bayesplot)
library(ggplot2)
library(tidyverse)


# BRHO ####
load("./models/Posteriors/seeds_brho_c_posteriorsCW.rdata")
load("./models/Posteriors/seeds_brho_d_posteriorsCW.rdata")

brho_c <- rstan::extract(seeds_brho_c)
brho_d <- rstan::extract(seeds_brho_d)

# Convert to data frame
params_brho <- rbind(as.data.frame(lapply(brho_c, mean))[-8],
                     as.data.frame(lapply(brho_d, mean))[-8])
params_avfa$species <- "brho"
params_avfa$treatment <- c("control","drought")