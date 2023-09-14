## Model loop 

# Prep ####
#source("data_cleaning/format_model_dat.R") ## get formatted model data

## Save a copy of data
#write.csv(model.dat, "data/model_dat.csv")

model.dat <- read.csv("data/model_dat.csv")

library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library(here)

date <- 20230914

# check data for issues
#str(model.dat)

#na.issues <- model.dat %>%
 # filter(is.na(phyto.seed.out))


# Set initials ####
initials <- list(lambda_base=200,
                 lambda_dev=1,
                 alpha_acam_base=1,
                 alpha_acam_dev=1,
                 alpha_amme_base=1, 
                 alpha_amme_dev=1,
                 alpha_anar_base=1, 
                 alpha_anar_dev=1,
                 alpha_brho_base=1,
                 alpha_brho_dev=1,
                 alpha_brni_base=1, 
                 alpha_brni_dev=1, 
                 alpha_ceso_base=1,
                 alpha_ceso_dev=1,
                 alpha_gitr_base=1, 
                 alpha_gitr_dev=1, 
                 alpha_leni_base=1, 
                 alpha_leni_dev=1,
                 alpha_lomu_base=1, 
                 alpha_lomu_dev=1,
                 alpha_mael_base=1, 
                 alpha_mael_dev=1,
                 alpha_mica_base=1,
                 alpha_mica_dev=1,
                 alpha_pler_base=1, 
                 alpha_pler_dev=1,
                 alpha_plno_base=1,
                 alpha_plno_dev=1,
                 alpha_taca_base=1,
                 alpha_taca_dev=1,
                 alpha_thir_base=1, 
                 alpha_thir_dev=1,
                 alpha_twil_base=1, 
                 alpha_twil_dev=1,
                 alpha_weeds_base=1, 
                 alpha_weeds_dev=1)
                 #epsilon=1)

initials1<- list(initials, initials, initials, initials)
#initials1<-list(initials)

# Loop thru ea Species ####
#species <- c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", 
#"LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL")

species <- c("ANAR")

model.output <- list()
warnings <- list()

for(i in species){
  
  ## subset model data by species
  dat <- subset(model.dat, phyto == i)
  
  ## create vectors of the various data inputs
  Fecundity <- as.integer(round(dat$phyto.seed.out)) ## seeds out
  N_blocks <- as.integer(length(unique(dat$block))) ## number of blocks
  block <- as.integer(dat$block) ## vector of block vals
  N <- as.integer(length(Fecundity)) ## number of observations
  N_i <- as.integer(dat$phyto.seed.in) ## seeds in of focal species
  g_i <- dat$mean.germ ## germ of focal species; do NOT use as.integer() here, rounds decimals down making germ values of 0
  trt <- as.integer(dat$trt) ## treatment (binary)
  
  ## stems data
  acam <- as.integer(dat$ACAM)
  amme <- as.integer(dat$AMME)
  anar <- as.integer(dat$ANAR)
  brho <- as.integer(dat$BRHO)
  brni <- as.integer(dat$BRNI)
  ceso <- as.integer(dat$CESO)
  gitr <- as.integer(dat$GITR)
  leni <- as.integer(dat$LENI)
  lomu <- as.integer(dat$LOMU)
  mael <- as.integer(dat$MAEL)
  mica <- as.integer(dat$MICA)
  pler <- as.integer(dat$PLER)
  plno <- as.integer(dat$PLNO)
  taca <- as.integer(dat$TACA)
  thir <- as.integer(dat$THIR)
  twil <- as.integer(dat$TWIL)
  weeds <- as.integer(dat$weeds)
  
  print(i)
  
  model.output[[paste0("ricker_",i)]] <- stan(
    file = paste0("Models/CW/ricker_model/in_development/random_effects_block/Ricker_model_tweaks.stan"),
    
    data = c("N", "Fecundity", "N_i", "trt", "g_i",
             "acam", "amme", "anar", "brho","brni", "ceso","gitr", "leni", 
             "lomu", "mael", "mica", "pler", "plno", "taca","thir","twil", "weeds"), 
    
    iter = 5000, chains = 4, thin = 3, 
    
    control = list(adapt_delta = 0.95, max_treedepth = 20), init = initials1)
  
  tmp <- model.output[[paste0("ricker_",i)]] 
  
  save(tmp, file = paste0("Models/CW/ricker_model/in_development/random_effects_block/posteriors/ricker_",i,"_posteriors_pptdev_", date, ".rdata"))
  
}

