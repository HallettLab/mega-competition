## Model loop 

# Prep #

source("data_cleaning/format_model_dat.R")

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library(here)

# things to work out: germination rates, seed survival rates

initials <- list(lambda=250, 
                 alpha_pler=1, 
                 alpha_anar=1, 
                 alpha_acam=1,
                 alpha_brni=1, 
                 alpha_clpu=1, 
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
                 alpha_avba=1)

initials1<- list(initials, initials, initials, initials) 

species <- c("PLER", "BRHO", "GITR", "ACAM", "AVBA", "ANAR", "MAEL",
             "CLPU", "TACA", "LOMU", "TWIL", "THIR", "CESO", "MICA", "AMME", "PLNO")


trt <- c("C","D")

model.output <- list()
warnings <- list()

# This is placeholder until we figure out what to do about ANAR backgrounds that either have 0 stems or where germ estimates were zero (but clearly they werent)
model.dat <- model.dat.filtered
model.dat[!is.finite(model.dat$ANAR),]$ANAR <- mean(model.dat[is.finite(model.dat$ANAR) & model.dat$bkgrd == "ANAR",]$ANAR, na.rm = T)

model.dat <- model.dat[model.dat$unique.ID != 5462,] #remove missing CESO background

# replacing 0s with small number for now
model.dat[model.dat$phyto.seeds.out.final == 0,]$phyto.seeds.out.final <- 1

for(i in species){
  for(j in trt){
    dat <- subset(model.dat, phyto == i)
    dat <- subset(dat, treatment == j)
    
    Fecundity <- as.integer(round(dat$phyto.seeds.out.final))
    
    pler <- as.integer(dat$PLER)
    anar <- as.integer(dat$ANAR)
    acam <- as.integer(dat$ACAM)
    brni <- as.integer(dat$BRNI)
    clpu <- as.integer(dat$CLPU)
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
    avba <- as.integer(dat$AVBA)
    
    N <- as.integer(length(Fecundity))
    
    intra <- as.integer(unlist(dat[,i]))
    
    intra_g <- germ.sum.sp.DC[germ.sum.sp.DC$species == i & germ.sum.sp.DC$trt == j,]$avg.germ 
    #intra_s <- ignoring this for now
    
    print(i)
    print(j)
    
    model.output[[paste0("seeds_",i,"_",j)]] <- stan(file = paste0("Models/ML/species_BH_model_",j, ".stan"), data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael", "avba", "brni", "leni"),
                           iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                           init = initials1) 
    
    tmp <- model.output[[paste0("seeds_",i,"_",j)]] 
    
    save(tmp, file = paste0("Models/ML/Posteriors-normal-unbounded/seeds_",i,"_",j,"_posteriors.rdata"))
  }
}
