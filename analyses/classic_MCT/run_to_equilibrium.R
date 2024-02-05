# run coexistence models for dry versus wet

# Set up ####
## Read in Data ####
## model posteriors
source("Models/CW/ricker_model/random_effects_block/negative_binomial/posterior_processing.R")

## seed survival data
source("data_cleaning/seed-survival_data-cleaning/seed-survival_data-cleaning.R")

## germination data
source("data_cleaning/germination_data-cleaning/germination_rates.R")

germ.sum <- germ.sum.sp.DC ## rename so it's easier to use later on
rm(germ.sum.sp.DC)

## replicate information
reps <- read.csv("models/CW/replicate-info.csv")

theme_set(theme_classic())

date <- 20231218

# Equations ####
## 'As-Is' ####
### equilibrium abundance of resident sp
run.to.equilibrium <- function(surv, germ, lambda, alpha_intra, Nt, alpha_inter, germ_inter, inter_abund) {
  Ntp1 <- (1-germ)*surv*Nt + germ*lambda*Nt*exp(-alpha_intra *germ* Nt - alpha_inter*germ_inter*inter_abund)
  return(Ntp1)
}

# Set params ####
## Germ Rates ####
# germ rates dry
dry[["ACAM"]]$germ <- germ.sum[germ.sum$species == "ACAM" & germ.sum$trt == "D", ]$avg.germ
dry[["AMME"]]$germ <- germ.sum[germ.sum$species == "AMME" & germ.sum$trt == "D", ]$avg.germ
dry[["ANAR"]]$germ <- germ.sum[germ.sum$species == "ANAR" & germ.sum$trt == "D", ]$avg.germ
dry[["BRHO"]]$germ <- germ.sum[germ.sum$species == "BRHO" & germ.sum$trt == "D", ]$avg.germ
dry[["BRNI"]]$germ <- germ.sum[germ.sum$species == "BRNI" & germ.sum$trt == "D", ]$avg.germ
dry[["CESO"]]$germ <- germ.sum[germ.sum$species == "CESO" & germ.sum$trt == "D", ]$avg.germ
dry[["GITR"]]$germ <- germ.sum[germ.sum$species == "GITR" & germ.sum$trt == "D", ]$avg.germ
dry[["LENI"]]$germ <- germ.sum[germ.sum$species == "LENI" & germ.sum$trt == "D", ]$avg.germ
dry[["LOMU"]]$germ <- germ.sum[germ.sum$species == "LOMU" & germ.sum$trt == "D", ]$avg.germ
dry[["MAEL"]]$germ <- germ.sum[germ.sum$species == "MAEL" & germ.sum$trt == "D", ]$avg.germ
dry[["MICA"]]$germ <- germ.sum[germ.sum$species == "MICA" & germ.sum$trt == "D", ]$avg.germ
dry[["PLER"]]$germ <- germ.sum[germ.sum$species == "PLER" & germ.sum$trt == "D", ]$avg.germ
dry[["PLNO"]]$germ <- germ.sum[germ.sum$species == "PLNO" & germ.sum$trt == "D", ]$avg.germ
dry[["TACA"]]$germ <- germ.sum[germ.sum$species == "TACA" & germ.sum$trt == "D", ]$avg.germ
dry[["THIR"]]$germ <- germ.sum[germ.sum$species == "THIR" & germ.sum$trt == "D", ]$avg.germ
dry[["TWIL"]]$germ <- germ.sum[germ.sum$species == "TWIL" & germ.sum$trt == "D", ]$avg.germ

# germ rates wet
wet[["ACAM"]]$germ <- germ.sum[germ.sum$species == "ACAM" & germ.sum$trt == "C", ]$avg.germ
wet[["AMME"]]$germ <- germ.sum[germ.sum$species == "AMME" & germ.sum$trt == "C", ]$avg.germ
wet[["ANAR"]]$germ <- germ.sum[germ.sum$species == "ANAR" & germ.sum$trt == "C", ]$avg.germ
wet[["BRHO"]]$germ <- germ.sum[germ.sum$species == "BRHO" & germ.sum$trt == "C", ]$avg.germ
wet[["BRNI"]]$germ <- germ.sum[germ.sum$species == "BRNI" & germ.sum$trt == "C", ]$avg.germ
wet[["CESO"]]$germ <- germ.sum[germ.sum$species == "CESO" & germ.sum$trt == "C", ]$avg.germ
wet[["GITR"]]$germ <- germ.sum[germ.sum$species == "GITR" & germ.sum$trt == "C", ]$avg.germ
wet[["LENI"]]$germ <- germ.sum[germ.sum$species == "LENI" & germ.sum$trt == "C", ]$avg.germ
wet[["LOMU"]]$germ <- germ.sum[germ.sum$species == "LOMU" & germ.sum$trt == "C", ]$avg.germ
wet[["MAEL"]]$germ <- germ.sum[germ.sum$species == "MAEL" & germ.sum$trt == "C", ]$avg.germ
wet[["MICA"]]$germ <- germ.sum[germ.sum$species == "MICA" & germ.sum$trt == "C", ]$avg.germ
wet[["PLER"]]$germ <- germ.sum[germ.sum$species == "PLER" & germ.sum$trt == "C", ]$avg.germ
wet[["PLNO"]]$germ <- germ.sum[germ.sum$species == "PLNO" & germ.sum$trt == "C", ]$avg.germ
wet[["TACA"]]$germ <- germ.sum[germ.sum$species == "TACA" & germ.sum$trt == "C", ]$avg.germ
wet[["THIR"]]$germ <- germ.sum[germ.sum$species == "THIR" & germ.sum$trt == "C", ]$avg.germ
wet[["TWIL"]]$germ <- germ.sum[germ.sum$species == "TWIL" & germ.sum$trt == "C", ]$avg.germ

## Seed Survival ####
## add seed survival to models
dry[["ACAM"]]$surv <- surv.sum[surv.sum$species == "ACAM",]$surv.mean.p
dry[["AMME"]]$surv <- surv.sum[surv.sum$species == "AMME",]$surv.mean.p
dry[["ANAR"]]$surv <- surv.sum[surv.sum$species == "ANAR",]$surv.mean.p
dry[["BRHO"]]$surv <- surv.sum[surv.sum$species == "BRHO",]$surv.mean.p
dry[["BRNI"]]$surv <- surv.sum[surv.sum$species == "BRNI",]$surv.mean.p
dry[["CESO"]]$surv <- surv.sum[surv.sum$species == "CESO",]$surv.mean.p
dry[["GITR"]]$surv <- surv.sum[surv.sum$species == "GITR",]$surv.mean.p
dry[["LENI"]]$surv <- surv.sum[surv.sum$species == "LENI",]$surv.mean.p
dry[["LOMU"]]$surv <- surv.sum[surv.sum$species == "LOMU",]$surv.mean.p
dry[["MAEL"]]$surv <- surv.sum[surv.sum$species == "MAEL",]$surv.mean.p
dry[["MICA"]]$surv <- surv.sum[surv.sum$species == "MICA",]$surv.mean.p
dry[["PLER"]]$surv <- surv.sum[surv.sum$species == "PLER",]$surv.mean.p
dry[["PLNO"]]$surv <- surv.sum[surv.sum$species == "PLNO",]$surv.mean.p
dry[["TACA"]]$surv <- surv.sum[surv.sum$species == "TACA",]$surv.mean.p
dry[["THIR"]]$surv <- surv.sum[surv.sum$species == "THIR",]$surv.mean.p
dry[["TWIL"]]$surv <- surv.sum[surv.sum$species == "TWIL",]$surv.mean.p

wet[["ACAM"]]$surv <- surv.sum[surv.sum$species == "ACAM",]$surv.mean.p
wet[["AMME"]]$surv <- surv.sum[surv.sum$species == "AMME",]$surv.mean.p
wet[["ANAR"]]$surv <- surv.sum[surv.sum$species == "ANAR",]$surv.mean.p
wet[["BRHO"]]$surv <- surv.sum[surv.sum$species == "BRHO",]$surv.mean.p
wet[["BRNI"]]$surv <- surv.sum[surv.sum$species == "BRNI",]$surv.mean.p
wet[["CESO"]]$surv <- surv.sum[surv.sum$species == "CESO",]$surv.mean.p
wet[["GITR"]]$surv <- surv.sum[surv.sum$species == "GITR",]$surv.mean.p
wet[["LENI"]]$surv <- surv.sum[surv.sum$species == "LENI",]$surv.mean.p
wet[["LOMU"]]$surv <- surv.sum[surv.sum$species == "LOMU",]$surv.mean.p
wet[["MAEL"]]$surv <- surv.sum[surv.sum$species == "MAEL",]$surv.mean.p
wet[["MICA"]]$surv <- surv.sum[surv.sum$species == "MICA",]$surv.mean.p
wet[["PLER"]]$surv <- surv.sum[surv.sum$species == "PLER",]$surv.mean.p
wet[["PLNO"]]$surv <- surv.sum[surv.sum$species == "PLNO",]$surv.mean.p
wet[["TACA"]]$surv <- surv.sum[surv.sum$species == "TACA",]$surv.mean.p
wet[["THIR"]]$surv <- surv.sum[surv.sum$species == "THIR",]$surv.mean.p
wet[["TWIL"]]$surv <- surv.sum[surv.sum$species == "TWIL",]$surv.mean.p

# Run to Equilibrium ####
## Set up loop ####
all_intra <- c("alpha_acam",  
               "alpha_amme", 
               "alpha_anar", 
               "alpha_brho",  
               "alpha_brni", 
               "alpha_ceso",
               "alpha_gitr", 
               "alpha_leni",
               "alpha_lomu", 
               "alpha_mael", 
               "alpha_mica", 
               "alpha_pler", 
               "alpha_plno",  
               "alpha_taca", 
               "alpha_thir", 
               "alpha_twil") 

options <- length(all_intra)

time <- 300
runs <- 200

N <- array(NA, c(options, runs, time))
N[,,1] <- 10 # start with 100 individuals in every case
## create an array where each of the rows is one of the species-treatment combos arranged in the order of all_intra. 
## Each of the columns is one separate run of the model
## Each of the stacked matrices represents a particular time slice

## create empty dataframes
residents_dry <- as.data.frame(matrix(data = NA, nrow = 200, ncol = 1))
residents_wet <- as.data.frame(matrix(data = NA, nrow = 200, ncol = 1))

set.seed(40)

### Loop thru all posteriors ####
#### DRY ####
for(i in 1:length(names(dry))) {
  
  ## select the species
  datset <- dry[[i]] 
  
  ## set the intraspecific alpha name
  intra <- paste0("alpha_", tolower(substr(names(dry)[i], 1, 4)))
  
  ## make a vector of the length of the posterior distribution
  post_length <- length(datset$lambda)
  
  ## get list of all intraspecific alphas
  all_intras <- datset[[intra]]
  
  ## loop thru each time step
  for(t in 1:(time-1)) {
    
    ## randomly sample indices from the length of posterior distrib 200x
    posts <- sample(post_length, runs, replace=TRUE)
    
    ## use these indices to select 200 lambda values
    lambda <- datset$lambda[posts]
    
    ## use again to select 200 intra_alpha values
    alpha_intra <- all_intras[posts]
    
    ## for each sp x treat combo use the run to equil function to fill one row of data that uses the abundance at time t and outputs the abundance at time t+1
    ## as the model loops thru sp x treat combos, more rows of the array are filled out
    N[i, ,t+1] <- run.to.equilibrium(germ = datset$germ, 
                                     surv = datset$surv,
                                     lambda = lambda, 
                                     alpha_intra = alpha_intra, 
                                     Nt = N[i, ,t], 
                                     alpha_inter = 0,
                                     germ_inter = 0,
                                     inter_abund = 0) 
    
  }
  
  tmp.df <- data.frame(N[i,,300])
  
  ## change the column name to be the species
  names(tmp.df) <- substr(names(ricker_posteriors)[i], 1, 4)
  
  ## append temporary df to the empty df created earlier
  residents_dry <-  cbind(residents_dry,  tmp.df)
  
}

# remove first column
residents_dry <- residents_dry[,-1]

#### WET ####
for(i in 1:length(names(wet))) {
  
  ## select the species
  datset <- wet[[i]] 
  
  ## set the intraspecific alpha name
  intra <- paste0("alpha_", tolower(substr(names(dry)[i], 1, 4)))
  
  ## make a vector of the length of the posterior distribution
  post_length <- length(datset$lambda)
  
  ## get list of all intraspecific alphas
  all_intras <- datset[[intra]]
  
  ## loop thru each time step
  for(t in 1:(time-1)) {
    
    ## randomly sample indices from the length of posterior distrib 200x
    posts <- sample(post_length, runs, replace=TRUE)
    
    ## use these indices to select 200 lambda values
    lambda <- datset$lambda[posts]
    
    ## use again to select 200 intra_alpha values
    alpha_intra <- all_intras[posts]
    
    ## for each sp x treat combo use the run to equil function to fill one row of data that uses the abundance at time t and outputs the abundance at time t+1
    ## as the model loops thru sp x treat combos, more rows of the array are filled out
    N[i, ,t+1] <- run.to.equilibrium(germ = datset$germ, 
                                     surv = datset$surv,
                                     lambda = lambda, 
                                     alpha_intra = alpha_intra, 
                                     Nt = N[i, ,t], 
                                     alpha_inter = 0,
                                     germ_inter = 0,
                                     inter_abund = 0) 
    
  }
  
  tmp.df <- data.frame(N[i,,300])
  
  ## change the column name to be the species
  names(tmp.df) <- substr(names(ricker_posteriors)[i], 1, 4)
  
  ## append temporary df to the empty df created earlier
  residents_wet <-  cbind(residents_wet,  tmp.df)
  
}

# remove first column
residents_wet <- residents_wet[,-1] 

# Check Equil Abundances ####
## change to long format for visualization
residents_wet_long <- residents_wet %>%
  mutate(num = 1:length(PLER)) %>%
  pivot_longer(1:16, names_to = "species", values_to = "equil_abund")

residents_dry_long <- residents_dry %>%
  mutate(num = 1:length(PLER)) %>%
  pivot_longer(1:16, names_to = "species", values_to = "equil_abund")

## visualize
ggplot(residents_wet_long, aes(x=num, y=equil_abund)) +
  geom_point() +
  facet_wrap(~species, scales = "free") +
  xlab("Run Number") + ylab("Equilibrium Abundance") +
  ggtitle("Ambient Conditions, Equilibrium, Nt1 = 10, 12/18/23")

ggsave(paste0("analyses/classic_MCT/preliminary_equil_abundance/", date, "/equil_abund_ricker_negbinom_C_Nt10_", date, ".png"), height = 6, width = 10)

ggplot(residents_dry_long, aes(x=num, y=equil_abund)) +
  geom_point() +
  facet_wrap(~species, scales = "free") +
  xlab("Run Number") + ylab("Equilibrium Abundance") +
  ggtitle("Drought Conditions, Equilibrium, Nt1 = 1000, 12/18/23")

ggsave(paste0("analyses/classic_MCT/preliminary_equil_abundance/", date, "/equil_abund_ricker_negbinom_D_Nt1000", date, ".png"), height = 6, width = 10)
