## Model 
model.dat <- read.csv("data/model_dat.csv")

date <- 20231215

library(tidyverse)
library(bayesplot)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library(here)

# Set up ####
i <- "AMME"
dat <- subset(model.dat, phyto == i)

species <- "AMME"

## create vectors of the various data inputs
Fecundity <- as.integer(round(dat$phyto.seed.out)) ## seeds out
N_blocks <- as.integer(length(unique(dat$block))) ## number of blocks
Blocks_OLD <- as.integer(dat$block) ## vector of block vals
Blocks <- rep(NA, length(Blocks_OLD)) ## Make Blocks sequential
for (i in 1:length(Blocks_OLD)) {
  
  if(Blocks_OLD[i] == 1) { 
    Blocks[i] <- 1 } else if(Blocks_OLD[i] == 3) {
      Blocks[i] <- 2} else if(Blocks_OLD[i] == 4){ 
        Blocks[i] <- 3} else if(Blocks_OLD[i] == 6){
          Blocks[i] <- 4} else if(Blocks_OLD[i] == 12) { 
            Blocks[i] <- 5} else if(Blocks_OLD[i] == 14) {
              Blocks[i] <- 6} else if(Blocks_OLD[i] == 5) {
                Blocks[i] <- 7} else if (Blocks_OLD[i] == 7) {
                  Blocks[i] <- 8} else if (Blocks_OLD[i] == 8) {
                    Blocks[i] <- 9}  else if (Blocks_OLD[i] == 15){
                      Blocks[i] <- 10} else if (Blocks_OLD[i] == 16){
                        Blocks[i] <- 11
                      }
}

N <- as.integer(length(Fecundity)) ## number of observations
N_i <- as.integer(dat$phyto.n.indiv) ## stem # of focal species
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

## make a vector of data inputs to model
data_vec <- c("N", "Fecundity", "N_i", "N_blocks", "Blocks", "trt", "acam", "amme", "anar", "brho","brni", "ceso", "gitr", "leni", "lomu", "mael", "mica", "pler", "plno", "taca","thir","twil", "weeds")

## create initials for epsilon and sigma
initials <- list(epsilon=rep(1,N_blocks), sigma = 1)
initials1<- list(initials, initials, initials)

# Model ####
PrelimFit <- stan(file = 'Models/CW/ricker_model/random_effects_block/negative_binomial/CRW_ricker_RE_neg_binomial.stan', 
                  data = data_vec, init = initials1, iter = 3000, chains = 3, thin = 2, 
                  control = list(adapt_delta = 0.9, max_treedepth = 10)) 

## save model output
save(PrelimFit, file = paste0("Models/CW/ricker_model/random_effects_block/negative_binomial/posteriors/ricker_",species,"_posteriors_random_effects_neg_binomial", date, ".rdata"))




## individually check correlation
Pfit <- rstan::extract(PrelimFit)

cor(Pfit$alpha_acam_base, Pfit$alpha_acam_dev)
cor(Pfit$alpha_amme_base, Pfit$alpha_amme_dev)
cor(Pfit$alpha_anar_base, Pfit$alpha_anar_dev)
cor(Pfit$alpha_brho_base, Pfit$alpha_brho_dev)
cor(Pfit$alpha_brni_base, Pfit$alpha_brni_dev)
cor(Pfit$alpha_ceso_base, Pfit$alpha_ceso_dev)

cor(Pfit$alpha_gitr_base, Pfit$alpha_gitr_dev) ## higher
cor(Pfit$alpha_leni_base, Pfit$alpha_leni_dev)
cor(Pfit$alpha_lomu_base, Pfit$alpha_lomu_dev) ## higher
cor(Pfit$alpha_mael_base, Pfit$alpha_mael_dev) ## higher
cor(Pfit$alpha_mica_base, Pfit$alpha_mica_dev)
cor(Pfit$alpha_pler_base, Pfit$alpha_pler_dev)

cor(Pfit$alpha_plno_base, Pfit$alpha_plno_dev)
cor(Pfit$alpha_taca_base, Pfit$alpha_taca_dev)
cor(Pfit$alpha_thir_base, Pfit$alpha_thir_dev) ## higher
cor(Pfit$alpha_twil_base, Pfit$alpha_twil_dev) ## higher


# OLD ####
#model.output <- list()
#warnings <- list()

#for(i in species){

## Prep Model Data
## subset model data by species
# dat <- subset(model.dat, phyto == i)
#dat <- subset(dat, trt == 0)
# dat <- subset(dat, phyto.seed.out != 0) #didn't help

## create vectors of the various data inputs
#Fecundity <- as.integer(round(dat$phyto.seed.out)) ## seeds out
#N_blocks <- 6#as.integer(length(unique(dat$block))) ## number of blocks
#Blocks <- as.integer(dat$block) ## vector of block vals
#N <- as.integer(length(Fecundity)) ## number of observations
#N_i <- as.integer(dat$phyto.seed.in) ## seeds in of focal species
#g_i <- dat$mean.germ ## germ of focal species; FIX by removing as.integer()
#trt <- as.integer(dat$trt) ## treatment (binary)

## stems data
# acam <- as.integer(dat$ACAM)
#amme <- as.integer(dat$AMME)
#anar <- as.integer(dat$ANAR)
#brho <- as.integer(dat$BRHO)
#brni <- as.integer(dat$BRNI)
#ceso <- as.integer(dat$CESO)
#gitr <- as.integer(dat$GITR)
#leni <- as.integer(dat$LENI)
#lomu <- as.integer(dat$LOMU)
#mael <- as.integer(dat$MAEL)
#mica <- as.integer(dat$MICA)
#pler <- as.integer(dat$PLER)
#plno <- as.integer(dat$PLNO)
#taca <- as.integer(dat$TACA)
#thir <- as.integer(dat$THIR)
#twil <- as.integer(dat$TWIL)
#weeds <- as.integer(dat$weeds)

#print(i)

## Set initials
#initials <- list(epsilon=rep(1,N), sigma = .01) ## initials from Bowler
#initials1<-list(initials)
## Crashes R session

#list.init <- function(...)list(lambda_base = array(abs(as.numeric(rnorm(1,
#                                                                   mean=log(mean(Fecundity))/N,
# sd = abs(log(Fecundity)/N)
#))),
#dim = 1))
## crashes R session

## Run Model 
#model.output[[paste0("ricker_",i)]] <- stan(
# file = paste0("Models/CW/ricker_model/in_development/random_effects_block/Ricker_model_RE_noppt.stan"),

#data = data_vec, 
#init = initials1, 
#init_r = 2,
#iter = 5000, 
#seed = 24,
#chains = 1, 
#thin = 3, ## apparently this should rarely be necessary - maybe look into this further?
#control = list(adapt_delta = 0.95, max_treedepth = 20),

#)

#  tmp <- model.output[[paste0("ricker_",i)]] 

# save(tmp, file = paste0("Models/CW/ricker_model/in_development/random_effects_block/posteriors/ricker_",i,"_posteriors_random_effects_drought", date, ".rdata"))

#}
