
## testing out cmdstan

file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
mod <- cmdstan_model(file)


mod$print()

# names correspond to the data block in the Stan program
data_list <- list(N = 10, y = c(0,1,0,0,0,0,0,0,0,1))



fit <- mod$sample(
  data = data_list,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)


file2 <- file.path("Models/CW/ricker_model/in_development/random_effects_block/Ricker_model_C_random_effects.stan")
mod2 <- cmdstan_model(file2)

data_list2 <- list(Fecundity = as.integer(round(dat$phyto.seeds.out.final)),
                   N_blocks = length(unique(dat$block)),
                   group = as.integer(dat$block),
                   N = as.integer(length(Fecundity)), ## number of observations
                   intra = as.integer(unlist(dat[,i])), ## seeds in of focal species
                   intra_g = germ.sum.sp.DC[germ.sum.sp.DC$species == i & germ.sum.sp.DC$trt == j,]$avg.germ) # ,
                  # mean_ctrl_seeds = lambda_priors_mean[lambda_priors_mean$phyto == i & 
                                                           #lambda_priors_mean$treatment == j,]$mean_seeds_ctrl,
                 #  sd_ctrl_seeds = lambda_priors_mean[lambda_priors_mean$phyto == i & 
                                                      #   lambda_priors_mean$treatment == j,]$sd_seeds)

fit2 <- mod2$sample(
  data = data_list2,
  seed = 14,
  chains = 4,
  parallel_chains = 4,
  refresh = 500
)



regfile2 <- file.path("Models/CW/ricker_model/current_working_version_ricker_meanLpriors/18_species_Ricker_model_C.stan")

mod3<-cmdstan_model(regfile2)

dat_list3 <- list(Fecundity=as.integer(round(dat$phyto.seeds.out.final)),
                  
                  ## seeds in of all interacting species
                  pler=as.integer(dat$PLER),
                  anar=as.integer(dat$ANAR),
                  acam=as.integer(dat$ACAM),
                  brni=as.integer(dat$BRNI),
                  brho=as.integer(dat$BRHO),
                  gitr=as.integer(dat$GITR),
                  amme=as.integer(dat$AMME),
                  plno=as.integer(dat$PLNO),
                  thir=as.integer(dat$THIR),
                  mica=as.integer(dat$MICA),
                  ceso=as.integer(dat$CESO),
                  twil=as.integer(dat$TWIL),
                  lomu=as.integer(dat$LOMU),
                  taca=as.integer(dat$TACA),
                  mael=as.integer(dat$MAEL),
                  leni=as.integer(dat$LENI),
                  crco=as.integer(dat$CRCO),
                  erbo=as.integer(dat$ERBO),
                  figa=as.integer(dat$FIGA),
                  gamu=as.integer(dat$GAMU),
                  hygl=as.integer(dat$HYGL),
                  siga=as.integer(dat$SIGA),
                  other=as.integer(dat$other),
                  
                  ## number of obsercations
                  N=as.integer(length(Fecundity)) ,
                  
                  ## seeds in of focal species
                  intra=as.integer(unlist(dat[,i])) ,
                  
                  ## germination value
                  intra_g=germ.sum.sp.DC[germ.sum.sp.DC$species == i & germ.sum.sp.DC$trt == j,]$avg.germ, 
                  
                  ## informed priors - based on mean seeds in controls
                  mean_ctrl_seeds = lambda_priors_mean[lambda_priors_mean$phyto == i & lambda_priors_mean$treatment == j,]$mean_seeds_ctrl,
                  
                  sd_ctrl_seeds = lambda_priors_mean[lambda_priors_mean$phyto == i & lambda_priors_mean$treatment == j,]$sd_seeds)


fit3 <- mod3$sample(
  data = dat_list3,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 500
)





