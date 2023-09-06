## Model loop 

# Prep ####
source("data_cleaning/format_model_dat.R") ## get formatted model data

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library(here)

date <- 20230906

# Set initials ####
initials <- list(lambda_base=250,
                 #lambda_dev=1,
                 alpha_brho_base=1,
                 alpha_brho_dev=1,
                 alpha_pler_base=1, 
                 alpha_pler_dev=1,
                 alpha_weeds_base=1,
                 alpha_weeds_dev=1,
                 epsilon=1)

#initials1<- list(initials, initials, initials, initials)
initials1<-list(initials)
# Loop thru ea Species ####
#species <- c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", 
#"LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL")

species <- c("BRHO")

model.output <- list()
warnings <- list()

for(i in species){
  
  ## subset model data by species
  dat <- subset(model.dat, phyto == i)
  
  ## create vectors of the various data inputs
  Fecundity <- as.integer(round(dat$phyto.seed.out)) ## seeds out
  N_blocks <- length(unique(dat$block)) ## number of blocks
  block <- as.integer(dat$block) ## vector of block vals
  N <- as.integer(length(Fecundity)) ## number of observations
  N_i <- as.integer(dat$phyto.seed.in) ## seeds in of focal species
  g_i <- as.integer(dat$mean.germ) ## germ of focal species
  trt <- as.integer(dat$trt) ## treatment (binary)
  
  ## stems data
  
  brho <- as.integer(dat$BRHO)
  
  pler <- as.integer(dat$PLER)
  
  weeds <- as.integer(dat$weeds)
  
  print(i)
  
  model.output[[paste0("ricker_",i)]] <- stan(
    file = paste0("Models/CW/ricker_model/in_development/random_effects_block/Ricker_model_ppt_random_fx_brho_pler.stan"),
    
    data = c("N", "Fecundity", "N_i", "g_i", "N_blocks", "block", "trt", 
             "brho", "pler", "weeds"), 
    
    iter = 5000, chains = 1, thin = 3, 
    
    control = list(adapt_delta = 0.95, max_treedepth = 20), init = initials1)
  
  tmp <- model.output[[paste0("ricker_",i)]] 
  
  save(tmp, file = paste0("Models/CW/ricker_model/in_development/random_effects_block/posteriors/ricker_",i,"_posteriors_random_effects_", date, ".rdata"))
  
}

