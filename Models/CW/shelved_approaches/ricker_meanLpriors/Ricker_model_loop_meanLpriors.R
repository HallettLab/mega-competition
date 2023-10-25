## Model loop 

# Prep ####
source("data_cleaning/format_model_dat.R") ## get formatted model data

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library(here)

date <- 20230831

# Set initials ####
initials <- list(lambda=250, 
                 alpha_pler=1, 
                 alpha_anar=1, 
                 alpha_acam=1,
                 alpha_brni=1,
                 alpha_brho=1,
                 alpha_gitr=1, 
                 alpha_amme=1, 
                 alpha_plno=1,
                 alpha_thir=1, 
                 alpha_mica=1, 
                 alpha_ceso=1,
                 alpha_twil=1, 
                 alpha_lomu=1, 
                 alpha_taca=1,
                 alpha_mael=1, 
                 alpha_leni=1,
                 alpha_crco=1,
                 alpha_erbo=1,
                 alpha_figa=1,
                 alpha_gamu=1,
                 alpha_hygl=1,
                 alpha_siga=1, 
                 alpha_other=1)

initials1<- list(initials, initials, initials, initials)

## Last minute data mods ####
## ok reps
good.reps <- reps %>%
  filter(true.reps > 3)

good.reps.vec <- unique(good.reps$combos)

model.dat.filtered <- model.dat %>%
  mutate(combos = paste(phyto, bkgrd, treatment, sep = "_")) %>%
  filter(combos %in% good.reps.vec)

# Loop thru ea Species ####
#species <- c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL")

species <- c("PLER")
trt <- c("C")

model.output <- list()
warnings <- list()

for(i in species){
  for(j in trt){
    
    ## subset correct species and treatment from model data
    dat <- subset(model.dat.filtered, phyto == i)
    dat <- subset(dat, treatment == j)
    
    ## set fecundity as an integer vector
    Fecundity <- as.integer(round(dat$phyto.seeds.out.final))
    
    ## seeds in of all interacting species
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
    crco <- as.integer(dat$CRCO)
    erbo <- as.integer(dat$ERBO)
    figa <- as.integer(dat$FIGA)
    gamu <- as.integer(dat$GAMU)
    hygl <- as.integer(dat$HYGL)
    siga <- as.integer(dat$SIGA)
    other <- as.integer(dat$other)
    
    ## number of obsercations
    N <- as.integer(length(Fecundity)) 
    
    ## seeds in of focal species
    intra <- as.integer(unlist(dat[,i])) 
    
    ## germination value
    intra_g <- germ.sum.sp.DC[germ.sum.sp.DC$species == i & germ.sum.sp.DC$trt == j,]$avg.germ 

    ## informed priors - based on mean seeds in controls
    mean_ctrl_seeds <- lambda_priors_mean[lambda_priors_mean$phyto == i & lambda_priors_mean$treatment == j,]$mean_seeds_ctrl
    
    sd_ctrl_seeds <- lambda_priors_mean[lambda_priors_mean$phyto == i & lambda_priors_mean$treatment == j,]$sd_seeds
    
    print(i)
    print(j)
    
    model.output[[paste0("seeds_",i,"_",j)]] <- stan(file = paste0("Models/CW/ricker_model/current_working_version_ricker_meanLpriors/18_species_Ricker_model_",j, ".stan"), data = c("N", "Fecundity", "intra", "intra_g", "mean_ctrl_seeds", "sd_ctrl_seeds", "acam", "amme", "anar", "brho","brni", "ceso", "gitr", "leni", "lomu", "mael", "mica", "pler", "plno", "taca", "thir","twil","crco", "erbo", "figa", "gamu", "hygl", "siga", "other"), iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20), init = initials1)
    
    tmp <- model.output[[paste0("seeds_",i,"_",j)]] 
    
    save(tmp, file = paste0("Models/CW/ricker_model/posteriors/lambda_prior_mean/seeds_",i,"_",j,"_posteriors_Ricker_meanLpriors_", date, ".rdata"))
  }
}

