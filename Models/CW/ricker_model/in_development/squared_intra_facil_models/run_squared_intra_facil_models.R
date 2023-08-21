
# Prep ####
source("data_cleaning/format_model_dat.R") ## get formatted model data

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library(here)

## intraspecific facilitation models
## from max prior models
## "ACAM_C" "AMME_C" "CESO_D" "PLNO_D" "THIR_C" "THIR_D" "TWIL_C" "TWIL_D"

## from mean prior models
## "ACAM_C" "ACAM_D" "AMME_C" "AMME_D" "CESO_D" "MAEL_D" "PLNO_D" "THIR_C" "THIR_D" "TWIL_C" "TWIL_D"


# Last minute data mods ####
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
lambda_priors_mean_C <- lambda_priors_mean %>%
  mutate(sd_constrained = sd_seeds/50,
         sd_constrained = ifelse(phyto %in% c("BRNI", "ANAR", "LENI"), sd_seeds/100, sd_constrained))

constrained100 <- c("ANAR", "BRNI", "LENI")

# ACAM C & D ####
## Set initials ####
initials <- list(lambda=250, 
                 alpha_pler=1, 
                 alpha_anar=1, 
                 alpha_acam_a=1,
                 alpha_acam_b=1, ## add second term for intra facil
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

## Loop thru ea Species ####
species <- c("ACAM")
trt <- c("C", "D")

model.output <- list()
warnings <- list()

for(i in species){
  for(j in trt){
    
    ## set constraint value, k, to put in posterior file path
    if(i %in% constrained100) {
      k <- 100
      # } else if (sp_trt %in% constrained16) {
      #  k <- 16
    } else {
      k <- 50
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
    
    mean_ctrl_seeds <- lambda_priors_mean_C[lambda_priors_mean_C$phyto == i & lambda_priors_mean_C$treatment == j,]$mean_seeds_ctrl
    
    sd_ctrl_seeds <- lambda_priors_mean_C[lambda_priors_mean_C$phyto == i & lambda_priors_mean_C$treatment == j,]$sd_constrained
    
    print(i)
    print(j)
    print(k)
    
    model.output[[paste0("seeds_",i,"_",j)]] <- stan(file = paste0("Models/CW/ricker_model/squared_intra_facil_models/18_species_Ricker_model_",j, "_sq_term_ACAM_", j, ".stan"), data = c("N", "Fecundity", "intra", "intra_g", "mean_ctrl_seeds", "sd_ctrl_seeds", "acam", "amme", "anar", "brho","brni", "ceso", "gitr", "leni", "lomu", "mael", "mica", "pler", "plno", "taca", "thir","twil","crco", "erbo", "figa", "gamu", "hygl", "siga", "other"), iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                                                     init = initials1) 
    tmp <- model.output[[paste0("seeds_",i,"_",j)]] 
    
    save(tmp, file = paste0("Models/CW/ricker_model/squared_intra_facil_models/posteriors/seeds_",i,"_",j,"_posteriors_Ricker_meanLpriors_constrainedby_", k, "_sq_term.rdata"))
  }
}

## Diagnostics ####
## create pdf of pairs plots for each model    
pdf(paste0("models/CW/ricker_model/squared_intra_facil_models/diagnostics/ricker_model_pairs_plots_", i, "_", j, "_maxLpriors_constrainedby_", k, "_sq_term.pdf"), width = 20, height = 20)

## code for the plot
## only include parameters that we will be using in models (i.e. drop the weed alphas)
pairs(tmp, pars = c("lambda", "alpha_acam_a", "alpha_acam_b", "alpha_amme", "alpha_anar", "alpha_brho", "alpha_brni", "alpha_ceso", "alpha_gitr", "alpha_leni", "alpha_lomu", "alpha_mael", "alpha_mica", "alpha_pler", "alpha_plno", "alpha_taca", "alpha_thir",  "alpha_twil"))

dev.off()

# AMME C & D ####
## Set initials ####
initials <- list(lambda=250, 
                 alpha_pler=1, 
                 alpha_anar=1, 
                 alpha_acam=1,
                 alpha_brni=1, 
                 alpha_brho=1,
                 alpha_gitr=1, 
                 alpha_amme_a=1, 
                 alpha_amme_b=1, ## add second term for intra facil
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

## Loop thru ea Species ####
species <- c("AMME")
trt <- c("C", "D")

model.output <- list()
warnings <- list()

for(i in species){
  for(j in trt){
    
    ## set constraint value, k, to put in posterior file path
    if(i %in% constrained100) {
      k <- 100
      # } else if (sp_trt %in% constrained16) {
      #  k <- 16
    } else {
      k <- 50
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
    
    mean_ctrl_seeds <- lambda_priors_mean_C[lambda_priors_mean_C$phyto == i & lambda_priors_mean_C$treatment == j,]$mean_seeds_ctrl
    
    sd_ctrl_seeds <- lambda_priors_mean_C[lambda_priors_mean_C$phyto == i & lambda_priors_mean_C$treatment == j,]$sd_constrained
    
    print(i)
    print(j)
    print(k)
    
    model.output[[paste0("seeds_",i,"_",j)]] <- stan(file = paste0("Models/CW/ricker_model/squared_intra_facil_models/18_species_Ricker_model_",j, "_sq_term_AMME_", j, ".stan"), data = c("N", "Fecundity", "intra", "intra_g", "mean_ctrl_seeds", "sd_ctrl_seeds", "acam", "amme", "anar", "brho","brni", "ceso", "gitr", "leni", "lomu", "mael", "mica", "pler", "plno", "taca", "thir","twil","crco", "erbo", "figa", "gamu", "hygl", "siga", "other"), iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                                                     init = initials1) 
    tmp <- model.output[[paste0("seeds_",i,"_",j)]] 
    
    save(tmp, file = paste0("Models/CW/ricker_model/squared_intra_facil_models/posteriors/seeds_",i,"_",j,"_posteriors_Ricker_meanLpriors_constrainedby_", k, "_sq_term.rdata"))
  }
}

# CESO D ####
## Set initials ####
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
                 alpha_ceso_a=1,
                 alpha_ceso_b=1, ## add second term for intra facil
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

## Loop thru ea Species ####
species <- c("CESO")
trt <- c("D")

model.output <- list()
warnings <- list()

for(i in species){
  for(j in trt){
    
    ## set constraint value, k, to put in posterior file path
    if(i %in% constrained100) {
      k <- 100
      # } else if (sp_trt %in% constrained16) {
      #  k <- 16
    } else {
      k <- 50
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
    
    mean_ctrl_seeds <- lambda_priors_mean_C[lambda_priors_mean_C$phyto == i & lambda_priors_mean_C$treatment == j,]$mean_seeds_ctrl
    
    sd_ctrl_seeds <- lambda_priors_mean_C[lambda_priors_mean_C$phyto == i & lambda_priors_mean_C$treatment == j,]$sd_constrained
    
    print(i)
    print(j)
    print(k)
    
    model.output[[paste0("seeds_",i,"_",j)]] <- stan(file = paste0("Models/CW/ricker_model/squared_intra_facil_models/18_species_Ricker_model_",j, "_sq_term_CESO_", j, ".stan"), data = c("N", "Fecundity", "intra", "intra_g", "mean_ctrl_seeds", "sd_ctrl_seeds", "acam", "amme", "anar", "brho","brni", "ceso", "gitr", "leni", "lomu", "mael", "mica", "pler", "plno", "taca", "thir","twil","crco", "erbo", "figa", "gamu", "hygl", "siga", "other"), iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                                                     init = initials1) 

    tmp <- model.output[[paste0("seeds_",i,"_",j)]] 
    
    save(tmp, file = paste0("Models/CW/ricker_model/squared_intra_facil_models/posteriors/seeds_",i,"_",j,"_posteriors_Ricker_meanLpriors_constrainedby_", k, "_sq_term.rdata"))
  }
}

# MAEL D ####
## Set initials ####
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
                 alpha_mael_a=1,
                 alpha_mael_b=1,
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

## Loop thru ea Species ####
species <- c("MAEL")
trt <- c("D")

model.output <- list()
warnings <- list()

for(i in species){
  for(j in trt){
    
    ## set constraint value, k, to put in posterior file path
    if(i %in% constrained100) {
      k <- 100
      # } else if (sp_trt %in% constrained16) {
      #  k <- 16
    } else {
      k <- 50
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
    
    mean_ctrl_seeds <- lambda_priors_mean_C[lambda_priors_mean_C$phyto == i & lambda_priors_mean_C$treatment == j,]$mean_seeds_ctrl
    
    sd_ctrl_seeds <- lambda_priors_mean_C[lambda_priors_mean_C$phyto == i & lambda_priors_mean_C$treatment == j,]$sd_constrained
    
    print(i)
    print(j)
    print(k)
    
    model.output[[paste0("seeds_",i,"_",j)]] <- stan(file = paste0("Models/CW/ricker_model/squared_intra_facil_models/18_species_Ricker_model_",j, "_sq_term_MAEL_", j, ".stan"), data = c("N", "Fecundity", "intra", "intra_g", "mean_ctrl_seeds", "sd_ctrl_seeds", "acam", "amme", "anar", "brho","brni", "ceso", "gitr", "leni", "lomu", "mael", "mica", "pler", "plno", "taca", "thir","twil","crco", "erbo", "figa", "gamu", "hygl", "siga", "other"), iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                                                     init = initials1) 

    tmp <- model.output[[paste0("seeds_",i,"_",j)]] 
    
    save(tmp, file = paste0("Models/CW/ricker_model/squared_intra_facil_models/posteriors/seeds_",i,"_",j,"_posteriors_Ricker_meanLpriors_constrainedby_", k, "_sq_term.rdata"))
  }
}


# PLNO D #### 
## Set initials ####
initials <- list(lambda=250, 
                 alpha_pler=1, 
                 alpha_anar=1, 
                 alpha_acam=1,
                 alpha_brni=1, 
                 #alpha_clpu=1, 
                 alpha_brho=1,
                 alpha_gitr=1, 
                 alpha_amme=1, 
                 alpha_plno_a=1,
                 alpha_plno_b=1, ## add second term for intra facil
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

## Loop thru ea Species ####
species <- c("PLNO")
trt <- c("D")

model.output <- list()
warnings <- list()

for(i in species){
  for(j in trt){
    
    ## set constraint value, k, to put in posterior file path
    if(i %in% constrained100) {
      k <- 100
      # } else if (sp_trt %in% constrained16) {
      #  k <- 16
    } else {
      k <- 50
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
    
    mean_ctrl_seeds <- lambda_priors_mean_C[lambda_priors_mean_C$phyto == i & lambda_priors_mean_C$treatment == j,]$mean_seeds_ctrl
    
    sd_ctrl_seeds <- lambda_priors_mean_C[lambda_priors_mean_C$phyto == i & lambda_priors_mean_C$treatment == j,]$sd_constrained
    
    print(i)
    print(j)
    print(k)
    
    model.output[[paste0("seeds_",i,"_",j)]] <- stan(file = paste0("Models/CW/ricker_model/squared_intra_facil_models/18_species_Ricker_model_",j, "_sq_term_PLNO_", j, ".stan"), data = c("N", "Fecundity", "intra", "intra_g", "mean_ctrl_seeds", "sd_ctrl_seeds", "acam", "amme", "anar", "brho","brni", "ceso", "gitr", "leni", "lomu", "mael", "mica", "pler", "plno", "taca", "thir","twil","crco", "erbo", "figa", "gamu", "hygl", "siga", "other"), iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                                                     init = initials1) 
    tmp <- model.output[[paste0("seeds_",i,"_",j)]] 
    
    save(tmp, file = paste0("Models/CW/ricker_model/squared_intra_facil_models/posteriors/seeds_",i,"_",j,"_posteriors_Ricker_meanLpriors_constrainedby_", k, "_sq_term.rdata"))
  }
}

# THIR C & D ####
## Set initials ####
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
                 alpha_thir_a=1, 
                 alpha_thir_b=1, ## add second term for intra facil
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

## Loop thru ea Species ####
species <- c("THIR")
trt <- c("C", "D")

model.output <- list()
warnings <- list()

for(i in species){
  for(j in trt){
    
    ## set constraint value, k, to put in posterior file path
    if(i %in% constrained100) {
      k <- 100
      # } else if (sp_trt %in% constrained16) {
      #  k <- 16
    } else {
      k <- 50
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
    
    mean_ctrl_seeds <- lambda_priors_mean_C[lambda_priors_mean_C$phyto == i & lambda_priors_mean_C$treatment == j,]$mean_seeds_ctrl
    
    sd_ctrl_seeds <- lambda_priors_mean_C[lambda_priors_mean_C$phyto == i & lambda_priors_mean_C$treatment == j,]$sd_constrained
    
    print(i)
    print(j)
    print(k)
    
    model.output[[paste0("seeds_",i,"_",j)]] <- stan(file = paste0("Models/CW/ricker_model/squared_intra_facil_models/18_species_Ricker_model_",j, "_sq_term_THIR_", j, ".stan"), data = c("N", "Fecundity", "intra", "intra_g", "mean_ctrl_seeds", "sd_ctrl_seeds", "acam", "amme", "anar", "brho","brni", "ceso", "gitr", "leni", "lomu", "mael", "mica", "pler", "plno", "taca", "thir","twil","crco", "erbo", "figa", "gamu", "hygl", "siga", "other"), iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                                                     init = initials1) 

    tmp <- model.output[[paste0("seeds_",i,"_",j)]] 
    
    save(tmp, file = paste0("Models/CW/ricker_model/squared_intra_facil_models/posteriors/seeds_",i,"_",j,"_posteriors_Ricker_meanLpriors_constrainedby_", k, "_sq_term.rdata"))
  }
}

# TWIL C & D ####
## Set initials ####
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
                 alpha_twil_a=1, 
                 alpha_twil_b=1, ## add second term for intra facil
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

## Loop thru ea Species ####
species <- c("TWIL")
trt <- c("C", "D")

model.output <- list()
warnings <- list()

for(i in species){
  for(j in trt){
    
    ## set constraint value, k, to put in posterior file path
    if(i %in% constrained100) {
      k <- 100
      # } else if (sp_trt %in% constrained16) {
      #  k <- 16
    } else {
      k <- 50
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
    
    mean_ctrl_seeds <- lambda_priors_mean_C[lambda_priors_mean_C$phyto == i & lambda_priors_mean_C$treatment == j,]$mean_seeds_ctrl
    
    sd_ctrl_seeds <- lambda_priors_mean_C[lambda_priors_mean_C$phyto == i & lambda_priors_mean_C$treatment == j,]$sd_constrained
    
    print(i)
    print(j)
    print(k)
    
    model.output[[paste0("seeds_",i,"_",j)]] <- stan(file = paste0("Models/CW/ricker_model/squared_intra_facil_models/18_species_Ricker_model_",j, "_sq_term_TWIL_", j, ".stan"), data = c("N", "Fecundity", "intra", "intra_g", "mean_ctrl_seeds", "sd_ctrl_seeds", "acam", "amme", "anar", "brho","brni", "ceso", "gitr", "leni", "lomu", "mael", "mica", "pler", "plno", "taca", "thir","twil","crco", "erbo", "figa", "gamu", "hygl", "siga", "other"), iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                                                     init = initials1) 

    tmp <- model.output[[paste0("seeds_",i,"_",j)]] 
    
    save(tmp, file = paste0("Models/CW/ricker_model/squared_intra_facil_models/posteriors/seeds_",i,"_",j,"_posteriors_Ricker_meanLpriors_constrainedby_", k, "_sq_term.rdata"))
  }
}
