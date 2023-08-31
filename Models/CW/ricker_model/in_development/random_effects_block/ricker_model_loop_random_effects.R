## Model loop 

# Prep ####
source("data_cleaning/format_model_dat.R") ## get formatted model data

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library(here)

date <- 08292023

# Set initials ####
initials <- list(lambda=250, 
                 alpha_acam=1,
                 alpha_amme=1, 
                 alpha_anar=1, 
                 alpha_brho=1,
                 alpha_brni=1, 
                 alpha_ceso=1,
                 alpha_gitr=1, 
                 alpha_leni=1, 
                 alpha_lomu=1, 
                 alpha_mael=1, 
                 alpha_mica=1,
                 alpha_pler=1, 
                 alpha_plno=1,
                 alpha_taca=1,
                 alpha_thir=1, 
                 alpha_twil=1, 
                 alpha_weeds=1,
                 group_effect=1) ## I think there might need to be an initial for every parameter specified in teh model? Or maybe it works better that way? Really don't know

initials1<- list(initials, initials, initials, initials)
initials1 <- list(initials)

## Last minute data mods ####
## ok reps
good.reps <- reps %>%
  filter(true.reps > 3)

good.reps.vec <- unique(good.reps$combos)

model.dat.filtered <- model.dat %>%
  mutate(combos = paste(phyto, bkgrd, treatment, sep = "_")) %>%
  filter(combos %in% good.reps.vec) %>%
  mutate(weeds = CRCO + ERBO + FIGA + GAMU + HYGL + SIGA + other) %>% ## lump all the weeds together
  select(-CRCO, -ERBO, -FIGA, -GAMU, -HYGL, -SIGA, -other)

# Constrain Priors ####
# Loop thru ea Species ####
#species <- c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL")

species <- c("BRHO")
#trt <- c("C","D")
trt <- c("C")

model.output <- list()
warnings <- list()

for(i in species){
  for(j in trt){
    
    ## subset model data into data frame
    dat <- subset(model.dat.filtered, phyto == i)
    dat <- subset(dat, treatment == j)
    
    ## create vectors of all the various data inputs
    Fecundity <- as.integer(round(dat$phyto.seeds.out.final))
    N_blocks <- length(unique(dat$block))
    group <- as.integer(dat$block)
    
    pler <- as.integer(dat$PLER)
    anar <- as.integer(dat$ANAR)
    acam <- as.integer(dat$ACAM)
    brni <- as.integer(dat$BRNI)
    brho <- as.integer(dat$BRHO)
    gitr <- as.integer(dat$GITR)
    amme <- as.integer(dat$AMME)
    plno <- as.integer(dat$PLNO)
    thir <- as.integer(dat$THIR)
    mica <- as.integer(dat$MICA)
    ceso <- as.integer(dat$CESO)
    twil <- as.integer(dat$TWIL)
    lomu <- as.integer(dat$LOMU)
    taca <- as.integer(dat$TACA)
    mael <- as.integer(dat$MAEL)
    leni <- as.integer(dat$LENI)
    weeds <- as.integer(dat$weeds)

    
    N <- as.integer(length(Fecundity)) ## number of observations
    
    intra <- as.integer(unlist(dat[,i])) ## seeds in of focal species
    
    intra_g <- germ.sum.sp.DC[germ.sum.sp.DC$species == i & germ.sum.sp.DC$trt == j,]$avg.germ 
    
    mean_ctrl_seeds <- lambda_priors_mean[lambda_priors_mean$phyto == i & 
                                            lambda_priors_mean$treatment == j,]$mean_seeds_ctrl
    
    sd_ctrl_seeds <- lambda_priors_mean[lambda_priors_mean$phyto == i & 
                                          lambda_priors_mean$treatment == j,]$sd_seeds
    
    print(i)
    print(j)
    
    model.output[[paste0("seeds_",i,"_",j)]] <- stan(
      file = paste0("Models/CW/ricker_model/in_development/random_effects_block/Ricker_model_",j, "_random_effects.stan"),
      data = c("N", "Fecundity", "intra", "intra_g", "mean_ctrl_seeds", 
               "sd_ctrl_seeds", "acam", "amme", "anar", "brho","brni", "ceso", 
               "gitr", "leni", "lomu", "mael", "mica", "pler", "plno", "taca", 
               "thir","twil", "weeds", "N_blocks", "group"), 
      iter = 1000, chains = 4, thin = 3, 
      control = list(adapt_delta = 0.95, max_treedepth = 20),
                                                     init = initials1)
    
    tmp <- model.output[[paste0("seeds_",i,"_",j)]] 
    
    save(tmp, file = paste0("Models/CW/ricker_model/in_development/random_effects_block/posteriors/seeds_",i,"_",j,"_posteriors_Ricker_random_effects_", date, ".rdata"))
  }
}

