## Model loop 

# Prep ####
source("data_cleaning/format_model_dat.R") ## get formatted model data

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library(here)

# Set initials ####
initials <- list(lambda=250, 
                 alpha_pler=1, 
                 alpha_anar=1, 
                 alpha_acam=1,
                 alpha_brni=1, 
                 #alpha_clpu=1, 
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
                 # alpha_avba=1, 
                 alpha_crco=1,
                 alpha_erbo=1,
                 alpha_figa=1,
                 alpha_gamu=1,
                 alpha_hygl=1,
                 alpha_siga=1, 
                 alpha_other=1)

initials1<- list(initials, initials, initials, initials)

# Last minute data mods ####

# replacing 0s with small number for now
#model.dat[model.dat$phyto.seeds.out.final == 0,]$phyto.seeds.out.final <- 1
# DO NOT DO THIS


## ok reps
good.reps <- reps %>%
  filter(true.reps > 3)

good.reps.vec <- unique(good.reps$combos)

model.dat.filtered <- model.dat %>%
  mutate(combos = paste(phyto, bkgrd, treatment, sep = "_")) %>%
  filter(combos %in% good.reps.vec)

# Constrain Priors ####
## Problem models
    ## ACAM D, AMME C, BRNI C, BRNI D, and MAEL D.
lambda_priors_max_C <- lambda_priors_max %>%
  mutate(sd_constrained = ifelse(phyto == "BRNI", sd_seeds/50, 
                                 ifelse(phyto == "ACAM" & treatment == "D", sd_seeds/16, 
                                        ifelse(phyto == "AMME" & treatment == "C", sd_seeds/50, 
                                               ifelse(phyto == "MAEL" & treatment == "D", sd_seeds/50, sd_seeds)))))
## constraining the problem models by making standard deviation of priors smaller

# Loop thru ea Species ####
species <- c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL")
## "CLPU", "AVBA", 

constrained50 <- c("AMME_C", "BRNI_C", "BRNI_D", "MAEL_D")
constrained16 <- c("ACAM_D")

trt <- c("C","D")

model.output <- list()
warnings <- list()

for(i in species){
  for(j in trt){
    
    ## set constraint value, k, to put in posterior file path
    sp_trt <- paste0(i, "_", j)
    
    if(sp_trt %in% constrained50) {
      k <- 50
    } else if (sp_trt %in% constrained16) {
      k <- 16
    } else {
      k <- "none"
    }
    
    ## subset data
    dat <- subset(model.dat.filtered, phyto == i)
    dat <- subset(dat, treatment == j)
    
    Fecundity <- as.integer(round(dat$phyto.seeds.out.final))
    
    pler <- as.integer(dat$PLER)
    anar <- as.integer(dat$ANAR)
    acam <- as.integer(dat$ACAM)
    brni <- as.integer(dat$BRNI)
    #clpu <- as.integer(dat$CLPU)
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
    #avba <- as.integer(dat$AVBA)
    crco <- as.integer(dat$CRCO)
    erbo <- as.integer(dat$ERBO)
    figa <- as.integer(dat$FIGA)
    gamu <- as.integer(dat$GAMU)
    hygl <- as.integer(dat$HYGL)
    siga <- as.integer(dat$SIGA)
    other <- as.integer(dat$other)
    
    N <- as.integer(length(Fecundity)) 
    
    intra <- as.integer(unlist(dat[,i])) ## seeds in of focal species
    
    intra_g <- germ.sum.sp.DC[germ.sum.sp.DC$species == i & germ.sum.sp.DC$trt == j,]$avg.germ 
    
    mean_ctrl_seeds <- lambda_priors_max_C[lambda_priors_max_C$phyto == i & lambda_priors_max_C$treatment == j,]$max_seeds_ctrl
    
    sd_ctrl_seeds <- lambda_priors_max_C[lambda_priors_max_C$phyto == i & lambda_priors_max_C$treatment == j,]$sd_constrained
    
    print(i)
    print(j)
    
    model.output[[paste0("seeds_",i,"_",j)]] <- stan(file = paste0("Models/CW/ricker_model/18_species_Ricker_model_",j, ".stan"), data = c("N", "Fecundity", "intra", "intra_g", "mean_ctrl_seeds", "sd_ctrl_seeds", "acam", "amme", "anar", "brho","brni", "ceso", "gitr", "leni", "lomu", "mael", "mica", "pler", "plno", "taca", "thir","twil","crco", "erbo", "figa", "gamu", "hygl", "siga", "other"), iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                                                     init = initials1) 
    ## "clpu","avba",
    
    tmp <- model.output[[paste0("seeds_",i,"_",j)]] 
    
    save(tmp, file = paste0("Models/CW/ricker_model/posteriors/lambda_prior_max/seeds_",i,"_",j,"_posteriors_Ricker_maxLpriors_constrainedby_", k, ".rdata"))
  }
}

