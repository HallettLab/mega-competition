# run coexistence models for dry versus wet

# Set up ####
## Read in Data ####
## model posteriors
source("Models/CW/ricker_model/import_ricker_posteriors.R")

## seed survival data
source("data_cleaning/seed-survival_data-cleaning/seed-survival_data-cleaning.R")

## germination data
source("data_cleaning/germination_data-cleaning/germination_rates.R")

germ.sum <- germ.sum.sp.DC ## rename so it's easier to use later on
rm(germ.sum.sp.DC)

## replicate information
reps <- read.csv("models/CW/replicate-info.csv")

theme_set(theme_bw())

date <- 20231218

# Equations ####
## 'As-Is' ####
### equilibrium abundance of resident sp
run.to.equilibrium <- function(surv, germ, lambda, alpha_intra, Nt, alpha_inter, germ_inter, inter_abund) {
   Ntp1 <- (1-germ)*surv*Nt + germ*lambda*Nt*exp(-alpha_intra *germ* Nt - alpha_inter*germ_inter*inter_abund)
   return(Ntp1)
}

### calc invasion GR 
run.invader <- function(surv, germ, lambda, alpha_inter, resid_abund, invader_abund, germ_resid) {
   Ntp1 <- (1-germ)*surv*invader_abund + germ*lambda*invader_abund*exp(-alpha_intra*invader_abund*germ - alpha_inter*resid_abund*germ_resid)
   LDGR <- log(Ntp1/invader_abund)
   return(LDGR)
 }

## Intra Facilitation ####
### equilibrium abundance of resident sp
### adds a squared term to intraspecific interaction coeff to prevent runaway population sizes when intraspecific facil is present
run.to.equilibrium.facilitation <- function(surv, germ, lambda, alpha_intra_a,alpha_intra_b, Nt, alpha_inter, germ_inter, inter_abund) {
  Ntp1 <- (1-germ)*surv*Nt + germ*lambda*Nt*exp(-alpha_intra_a*germ*Nt - alpha_intra_b*germ*(Nt^2) - alpha_inter*germ_inter*inter_abund)
  return(Ntp1)
}

## calc invasion GR with same squared term for consistency
run.invader.facilitation <- function(surv, germ, lambda, alpha_inter, resid_abund, invader_abund, germ_resid) {
  Ntp1 <- (1-germ)*surv*invader_abund + germ*lambda*invader_abund*exp(-alpha_intra*invader_abund*germ - alpha_intra*germ*(invader_abund^2) - alpha_inter*resid_abund*germ_resid)
  LDGR <- log(Ntp1/invader_abund)
  return(LDGR)
  
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

all_intra <- c("alpha_acam", "alpha_acam", 
               "alpha_amme", "alpha_amme", 
               "alpha_anar", "alpha_anar",
               "alpha_brho", "alpha_brho", 
               "alpha_brni", "alpha_brni",
               "alpha_ceso", "alpha_ceso", 
               "alpha_gitr", "alpha_gitr",
               "alpha_leni", "alpha_leni",
               "alpha_lomu", "alpha_lomu", 
               "alpha_mael", "alpha_mael",
               "alpha_mica", "alpha_mica", 
               "alpha_pler", "alpha_pler",
               "alpha_plno", "alpha_plno", 
               "alpha_taca", "alpha_taca",
               "alpha_thir", "alpha_thir",
               "alpha_twil", "alpha_twil") 

# Run to Equilibrium ####
## Consistent Params ####
### Set up loop ####
options <- length(all_intra)

time <- 300
runs <- 200

N <- array(NA, c(options, runs, time))
N[,,1] <- 100 # start with 100 individuals in every case
## create an array where each of the rows is one of the species-treatment combos arranged in the order of all_intra. 
## Each of the columns is one separate run of the model
## Each of the stacked matrices represents a particular time slice

## create empty dataframes
residents_dry <- as.data.frame(matrix(data = NA, nrow = 200, ncol = 1))
residents_wet <- as.data.frame(matrix(data = NA, nrow = 200, ncol = 1))

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










# This was sloppily done by Marina, remove first column
residents_wet <- residents_wet[,-1] 
residents_dry <- residents_dry[,-1]


## Add Sq. Term ####
#ricker_posteriors_facil[["THIR_D"]]$germ <- germ.sum[germ.sum$species == "THIR" & germ.sum$trt == "D", ]$avg.germ

#ricker_posteriors_facil[["THIR_D"]]$surv <- surv.sum[surv.sum$species == "THIR",]$surv.mean.p

### Set up loop ####
#thir_intra <- c("alpha_thir_a", "alpha_thir_b")
#options <- length(thir_intra)

#time <- 300
#runs <- 200

#N_facil <- array(NA, c(options, runs, time))
#N_facil[,,1] <- 100 # start with 100 individuals in every case
## create an array where each of the rows is one of the species-treatment combos arranged in the order of all_intra. 
## Each of the columns is one separate run of the model
## Each of the stacked matrices represents a particular time slice

## create empty dataframes
#residents_dry_facil <- as.data.frame(matrix(data = NA, nrow = 200, ncol = 1))
#residents_wet_facil <- as.data.frame(matrix(data = NA, nrow = 200, ncol = 1))

#intra_facil2 <- c("THIR_D")

### Loop thru all posteriors ####
#for(i in 1:length(intra_facil2)) {
  
  ## select a particular species treatment combo
  #sp_trt <- intra_facil2[i]
  
  #datset <- ricker_posteriors_facil[[sp_trt]] 
  
  ## set the intraspecific alpha name
  #intra_a <- paste0("alpha_", tolower(substr(sp_trt, 1, 4)), "_a")
  #intra_b <- paste0("alpha_", tolower(substr(sp_trt, 1, 4)), "_b")
  
  ## make a vector of the length of the posterior distribution
  #post_length <- length(datset$lambda)
  
  ## get list of all intraspecific alphas
  ##all_intras_a <- datset[[intra_a]]
  #all_intras_b <- datset[[intra_b]]
  
  
  ## loop thru each time step
  #for(t in 1:(time-1)) {
    
    ## randomly sample indices from the length of posterior distrib 200x
   # posts <- sample(post_length, runs, replace=TRUE)
    
    ## use these indices to select 200 lambda values
    #lambda <- datset$lambda[posts]
    
    ## use again to select 200 intra_alpha values
    #alpha_intra_a <- all_intras_a[posts]
    
    #alpha_intra_b <- all_intras_b[posts]
    ## for each sp x treat combo use the run to equil function to fill one row of data that uses the abundance at time t and outputs the abundance at time t+1
    ## as the model loops thru sp x treat combos, more rows of the array are filled out
   # N_facil[i, ,t+1] <- run.to.equilibrium.facilitation(germ = datset$germ, 
    #                                 surv = datset$surv,
     #                                lambda = lambda, 
      #                               alpha_intra_a = alpha_intra_a,
       #                              alpha_intra_b = alpha_intra_b,
        #                             Nt = N_facil[i, ,t], 
                                     #Nt = 100,
         #                            alpha_inter = 0,
          #                           germ_inter = 0,
           #                          inter_abund = 0) 
    
  #}
  
  #surv, germ, lambda, alpha_intra, Nt, alpha_inter, germ_inter, inter_abund
  
  ## if it is a drought model
 # if(str_sub(sp_trt, start = 6, end= 6) == "D"){
    
    ## tmp.df is a vector of the final abundance of all 200 separate runs - will become a column in the output dataframe
  #  tmp.df <- data.frame(N_facil[i,,300])
    
    ## change the column name to be the species
   # names(tmp.df) <- sp_trt
    
    ## append temporary df to the empty df created earlier
    #residents_dry_facil <-  cbind(residents_dry_facil,  tmp.df)
    
#  }
  
  ## if it is a control model
 # else{
    
    ## same steps as above, just appended to different output dataframe
  #  tmp.df <- data.frame(N_facil[i,,300])
   # names(tmp.df) <- sp_trt
    #residents_wet_facil <-  cbind(residents_wet_facil,  tmp.df)
    
  #}
#}

# This was sloppily done by Marina, remove first column
#residents_wet_facil <- residents_wet_facil[,-1] 
#residents_dry_facil <- residents_dry_facil[,-1]


## Shuffle Params ea. Run ####




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
  facet_wrap(~species) +
  xlab("Run Number") + ylab("Equilibrium Abundance") +
  ggtitle("Ambient Conditions, Ricker Model, Filt Dat, 6/09/23")

ggsave(paste0("analyses/classic_MCT/preliminary_equil_abundance/equil_abund_ricker_ambient_maxLpriors_consistent_params_per_run", date, ".png"), height = 6, width = 10)

ggplot(residents_dry_long, aes(x=num, y=equil_abund)) +
  geom_point() +
  facet_wrap(~species) +
  xlab("Run Number") + ylab("Equilibrium Abundance") +
  ggtitle("Drought Conditions, Ricker Model, Filt Dat, 6/09/23")

ggsave(paste0("analyses/classic_MCT/preliminary_equil_abundance/equil_abund_ricker_drought_maxLpriors_consistent_params_per_run", date, ".png"), height = 6, width = 10)




# Invade into residents ####

## Filter species ####

## didn't have an equilibrium abundance
#rm <- c("THIR") 
#residents_wet <- residents_wet[,!colnames(residents_wet) %in% rm]
#residents_dry <- residents_dry[,!colnames(residents_dry) %in% rm]

## Prep Loop ####
#update species list
#species <- species[!(species %in% rm)]
species <- c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL")


## set number of replicates
reps <- 200
runs <- 200

## I don't currently understand why we have 200 runs and replicates. For this it seems redundant since we calc LDGR once, not in 200 sequential time steps like the equilibrium abundance.


## create empty list to contain output
tmp <- list()

## Run Loop ####
## i = invader, j = resident
## for each invader
for(i in species) {
  
  ## for each resident
  for(j in species) {
    
    ## when invader and resident are different species
    if(i != j) {
      
      ## for each treatment
      for(k in trt){
        
        ## create one element of the list
        tmp[[paste0(i, "_into_", j, "_", k)]] <- matrix(NA, reps, runs)
        
        ## set invader abundance to 1
        invader_abund <- 1
        
        ## create a vector the length of the posterior distrib
        post_length <- length(ricker_posteriors[[paste0(i,"_", k)]]$lambda)
        
        ## for each replicate
        for(r in 1:reps) {
          
          ## randomly sample 200 times from the length of the posterior distrib
          posts <- sample(post_length, runs, replace=TRUE)
          
          ## use the run.invader function to add rows to the matrix
          tmp[[paste0(i, "_into_", j, "_", k)]][r,] <- run.invader(surv = ricker_posteriors[[paste0(i,"_", k)]]$surv, 
            germ = ricker_posteriors[[paste0(i,"_", k)]]$germ, 
            germ_resid = ricker_posteriors[[paste0(j,"_", k)]]$germ,
            lambda = ricker_posteriors[[paste0(i,"_", k)]]$lambda[posts], 
            alpha_inter = unlist(ricker_posteriors[[paste0(i,"_", k)]][paste0("alpha_", tolower(j))], use.names = F)[posts],
            ## use the appropriate dataframe depending on treat
            resid_abund = if(k == "D"){
              residents_dry[,j] 
            }
            else {
              residents_wet[,j]
            },
            
            invader_abund = invader_abund)
        }
      }
    }
  }
}


## Change to DF ####
invasion_dry <- list()
invasion_wet <- list()

for(i in names(tmp)){
  if(str_sub(names(tmp[i]), start = -1) == "C"){
    tmp2 <- as.vector(tmp[[i]])
    invasion_wet[[i]] <- tmp2
  }
  
  else {
    tmp3 <- as.vector(tmp[[i]])
    invasion_dry[[i]] <- tmp3
  }
}

invasion_dry <- data.frame(invasion_dry)
invasion_wet <- data.frame(invasion_wet)

# Filter by Combos ####
## filter to remove combos without enough replicates
good.reps <- reps %>%
  filter(true.reps > 3)

good.reps.vec <- unique(good.reps$combos)

invasion_dry_long <- invasion_dry %>%
  pivot_longer(1:240, names_to = "scenario", values_to = "GRWR") %>%
  mutate(invader = substr(scenario, 1, 4),
         resident = substr(scenario, 11, 14), 
         combos = paste(invader, resident, "D", sep = "_")) %>%
  filter(combos %in% good.reps.vec)

invasion_wet_long <- invasion_wet %>%
  pivot_longer(1:240, names_to = "scenario", values_to = "GRWR") %>%
  mutate(invader = substr(scenario, 1, 4),
         resident = substr(scenario, 11, 14),
         combos = paste(invader, resident, "C", sep = "_")) %>%
  filter(combos %in% good.reps.vec)

# Visualize ####
for(i in 1:length(species)){
  
  p_dry <- ggplot(invasion_dry_long[invasion_dry_long$invader == species[i],], aes(x=GRWR)) +
    geom_histogram(bins = 100) +  
    facet_wrap(~resident, scales = "free") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    ggtitle(paste0(species[i], "Invader Dry, Ricker"))
  
  ggsave(p_dry, file=paste0("analyses/classic_MCT/preliminary_GRWR/Ricker_06092023/", species[i], "_ricker_dry_", date, ".png"), width = 14, height = 10)
  
  p_wet <- ggplot(invasion_wet_long[invasion_wet_long$invader == species[i],], aes(x=GRWR)) +
    geom_histogram(bins = 100) +  
    facet_wrap(~resident, scales = "free") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    ggtitle(paste0(species[i], "Invader Wet, Ricker"))
  
  ggsave(p_wet, file=paste0("analyses/classic_MCT/preliminary_GRWR/Ricker_06092023/", species[i], "_ricker_wet_", date, ".png"), width = 14, height = 10)
  
}

rm(p_dry, p_wet)



for(j in 1:length(species)) {
  
  for(k in 1:length(species)) {
    
    p_dry <- ggplot(invasion_dry_long[(invasion_dry_long$invader == species[j] | invasion_dry_long$resident == species[j]) & (invasion_dry_long$invader == species[k] | invasion_dry_long$resident == species[k]),], aes(x=GRWR)) +
      geom_histogram(bins = 100) +  
      facet_wrap(~scenario, scales = "free") +
      geom_vline(xintercept = 0, linetype = "dashed")
    
    ggsave(p_dry, file=paste0("analyses/classic_MCT/preliminary_GRWR/", species[j], "_", species[k], "_ricker_pairwise_dry_", date, ".png"), width = 6, height = 3)
    
  }

}



pdf("analyses/classic_MCT/preliminary_GRWR/ricker_dry_pairwise_combos.pdf", width = 6, height = 3)

for(j in 1:length(species)) {
  
  for(k in 1:length(species)) {
    
    #if(paste(species[j], species[k], "D", sep = "_") %in% good.reps.vec) {
      
     p <- ggplot(invasion_dry_long[(invasion_dry_long$invader == species[j] | invasion_dry_long$resident == species[j]) & (invasion_dry_long$invader == species[k] | invasion_dry_long$resident == species[k]),], aes(x=GRWR)) +
        geom_histogram(bins = 100) +  
        facet_wrap(~scenario, scales = "free") +
        geom_vline(xintercept = 0, linetype = "dashed")
      
     print(p)
     
    #}
    
  }
  
}

dev.off()


lomu.check <- invasion_dry_long %>%
  filter(invader == "LOMU") 

ggplot(lomu.check, aes(x=resident, y=GRWR)) +
  geom_boxplot() +
  facet_wrap(~resident, scales = "free")

ggsave("models/CW/classic_MCT/lomu_GRWR_exploration.png", width = 14, height = 10)



# Save Data ####
#write.csv(invasion_dry, file = "models/CW/classic_MCT/GRWR_dry.csv")
#write.csv(invasion_wet, file = "models/CW/classic_MCT/GRWR_wet.csv")

rm(invasion_dry, invasion_wet)