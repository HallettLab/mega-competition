## Script Purpose: run models for control plots only to determine the combined effects of precip treatment and species interactions on lambda values.

# Set up env ####
source("data_cleaning/format_model_dat_controls_w_precip_term.R") ## get formatted model data with controls only

## load packages
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(here)

# Set initials ####
initials <- list(lambda=250, 
                 alpha_weeds=1)

initials1<- list(initials, initials, initials, initials)

# Loop thru ea Species ####
species <- c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL")

model.output <- list()
warnings <- list()

for(i in species){

    dat <- subset(model.dat, phyto == i) ## subset correct phyto

    Fecundity <- as.integer(round(dat$phyto.seed.out)) ## make seeds out an integer
    
    precip <- dat$treat ## vector of precip treatment, 0 or 1
    
    weeds <- as.integer(dat$weeds) ## weed stems in
    
    N <- as.integer(length(Fecundity)) 
    
    intra <- as.integer(dat$phyto.seed.in) ## seeds in of focal species
    
    intra_g <- germ.sum.sp.DC[germ.sum.sp.DC$species == i & germ.sum.sp.DC$trt == "C",]$avg.germ 
    ## use control for now, may need to readjust
    
    mean_ctrl_seeds <- lambda_priors_mean[lambda_priors_mean$phyto == i,]$mean_seeds_ctrl
    
    sd_ctrl_seeds <- lambda_priors_mean[lambda_priors_mean$phyto == i,]$sd_seeds
    
    print(i)

    model.output[[paste0("seeds_",i, "_precip")]] <- stan(file = paste0("Models/CW/ricker_model/18_species_Ricker_model_with_precip_term.stan"), data = c("N", "Fecundity", "intra", "intra_g", "mean_ctrl_seeds", "sd_ctrl_seeds", "weeds", "precip"), iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                                                     init = initials1) 
    
    tmp <- model.output[[paste0("seeds_",i,"_precip")]] 
    
    save(tmp, file = paste0("Models/CW/ricker_model/posteriors/controls_with_precip_term/seeds_",i,"_posteriors_Ricker_meanLpriors_precip_term.rdata"))

    }

