## Model loop 

# Prep ####
model.dat <- read.csv("data/model_dat.csv")

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library(here)

date <- 20231010

# Set initials ####
initials <- list(lambda_base=100,
                 epsilon=1,
                 sigma_0 = 1) 

#lambda_dev=1,
#alpha_acam_base=1,
#alpha_acam_dev=1,
#alpha_amme_base=1, 
#alpha_amme_dev=1,
##alpha_anar_base=1, 
# alpha_anar_dev=1,
#alpha_brho_base=1,
#alpha_brho_dev=1,
##alpha_brni_base=1, 
# alpha_brni_dev=1, 
#alpha_ceso_base=1,
#alpha_ceso_dev=1,
#alpha_gitr_base=1, 
#alpha_gitr_dev=1, 
#alpha_leni_base=1, 
#alpha_leni_dev=1,
#alpha_lomu_base=1, 
#alpha_lomu_dev=1,
#alpha_mael_base=1, 
#alpha_mael_dev=1,
#alpha_mica_base=1,
#alpha_mica_dev=1,
#alpha_pler_base=1, 
#alpha_pler_dev=1,
#alpha_plno_base=1,
#alpha_plno_dev=1,
# alpha_taca_base=1,
#alpha_taca_dev=1,
## alpha_thir_base=1, 
#alpha_thir_dev=1,
# alpha_twil_base=1, 
#alpha_twil_dev=1,
# alpha_weeds_base=1,
#alpha_weeds_dev=1,
#epsilon=rep(1,6),

#initials1<- list(initials, initials, initials, initials)
initials1<-list(initials)

#initials <- list(epsilon=rep(1,6), sigma_0 = .01) ## initials from Bowler
#initials1<-list(initials)
## this crashes R session repeatedly, whether it's sigma = 0.01 or sigma_0 = 0.01...

# Loop thru ea Species ####
#species <- c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", 
#"LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL")

data_vec <- c("N", "Fecundity", "N_i", "g_i", "N_blocks", "Blocks", "acam", "amme", "anar", "brho","brni", "ceso","gitr", "leni", "lomu", "mael", "mica", "pler", "plno", "taca","thir","twil", "weeds")

#"trt", 

species <- c("BRHO")

model.output <- list()
warnings <- list()

for(i in species){
  
  ## Prep Model Data
  ## subset model data by species
  dat <- subset(model.dat, phyto == i)
  dat <- subset(dat, trt == 0)
 # dat <- subset(dat, phyto.seed.out != 0) #didn't help
  
  ## create vectors of the various data inputs
  Fecundity <- as.integer(round(dat$phyto.seed.out)) ## seeds out
  N_blocks <- 6#as.integer(length(unique(dat$block))) ## number of blocks
  Blocks <- as.integer(dat$block) ## vector of block vals
  N <- as.integer(length(Fecundity)) ## number of observations
  N_i <- as.integer(dat$phyto.seed.in) ## seeds in of focal species
  g_i <- dat$mean.germ ## germ of focal species; FIX by removing as.integer()
  #trt <- as.integer(dat$trt) ## treatment (binary)
  
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
  model.output[[paste0("ricker_",i)]] <- stan(
    file = paste0("Models/CW/ricker_model/in_development/random_effects_block/Ricker_model_RE_noppt.stan"),
    
    data = data_vec, 
    init = initials1, 
    #init_r = 2,
    iter = 5000, 
    seed = 24,
    chains = 1, 
    thin = 3, ## apparently this should rarely be necessary - maybe look into this further?
    control = list(adapt_delta = 0.95, max_treedepth = 20),
    
    )
  
  tmp <- model.output[[paste0("ricker_",i)]] 
  
  save(tmp, file = paste0("Models/CW/ricker_model/in_development/random_effects_block/posteriors/ricker_",i,"_posteriors_random_effects_drought", date, ".rdata"))
  
}

