# run coexistence models for dry versus wet
# survival isn't in these models for now

source("Models/CW/import_ricker_posteriors.R")
source("data_cleaning/seed-survival_data-cleaning/seed-survival_data-cleaning.R")
## load seed survival data

theme_set(theme_bw())


# Set up ####
## Equations ####
run.to.equilibrium <- function(surv, germ, lambda, alpha_intra, Nt) {
   Ntp1 <- (1-germ)*surv*Nt + germ*lambda*Nt*exp(-alpha_intra *germ* Nt)
   return(Ntp1)
}
## equilibrium model experiences INTRAspecific competition, but not inter

run.invader <- function(surv, germ, lambda, alpha_inter, resid_abund, invader_abund) {
   Ntp1 <- (1-germ)*surv*invader_abund + germ*lambda*invader_abund*exp(-alpha_inter * resid_abund * germ_resid)
   LDGR <- log(Ntp1/invader_abund)
   return(LDGR)
 
 }

## Germ Rates ####
# germ rates dry
posteriors[["PLER_D"]]$germ <- 0.53
posteriors[["ANAR_D"]]$germ <- 0.07
posteriors[["ACAM_D"]]$germ <- 0.52
posteriors[["BRNI_D"]]$germ <- 0.39
posteriors[["CLPU_D"]]$germ <- 0.04
posteriors[["BRHO_D"]]$germ <- 1
posteriors[["GITR_D"]]$germ <- 0.91
posteriors[["AMME_D"]]$germ <- 0.14
posteriors[["PLNO_D"]]$germ <- 0.35
posteriors[["THIR_D"]]$germ <- 0.38
posteriors[["MICA_D"]]$germ <- 0.13
posteriors[["CESO_D"]]$germ <- 0.74
posteriors[["TWIL_D"]]$germ <- 0.02
posteriors[["LOMU_D"]]$germ <- 0.87
posteriors[["TACA_D"]]$germ <- 0.86
posteriors[["MAEL_D"]]$germ <- 0.18
posteriors[["LENI_D"]]$germ <- 0.3
posteriors[["AVBA_D"]]$germ <- 0.88

# germ rates wet
posteriors[["PLER_C"]]$germ <- 0.8
posteriors[["ANAR_C"]]$germ <- 0.15
posteriors[["ACAM_C"]]$germ <- 0.66
posteriors[["BRNI_C"]]$germ <- 0.69
posteriors[["CLPU_C"]]$germ <- 0.37
posteriors[["BRHO_C"]]$germ <- 0.97
posteriors[["GITR_C"]]$germ <- 0.98
posteriors[["AMME_C"]]$germ <- 0.88
posteriors[["PLNO_C"]]$germ <- 0.66
posteriors[["THIR_C"]]$germ <- 0.79
posteriors[["MICA_C"]]$germ <- 0.72
posteriors[["CESO_C"]]$germ <- 0.92
posteriors[["TWIL_C"]]$germ <- 0.44
posteriors[["LOMU_C"]]$germ <- 0.96
posteriors[["TACA_C"]]$germ <- 0.87
posteriors[["MAEL_C"]]$germ <- 0.59
posteriors[["LENI_C"]]$germ <- 0.85
posteriors[["AVBA_C"]]$germ <- 0.96

## Seed Survival ####
## add seed survival to models
posteriors[["PLER_D"]]$surv <- surv.sum[species == "PLER",]$surv.mean.p
posteriors[["ANAR_D"]]$surv <- surv.sum[species == "ANAR",]$surv.mean.p
posteriors[["ACAM_D"]]$surv <- surv.sum[species == "ACAM",]$surv.mean.p
posteriors[["BRNI_D"]]$surv <- surv.sum[species == "BRNI",]$surv.mean.p
posteriors[["CLPU_D"]]$surv <- surv.sum[species == "CLPU",]$surv.mean.p
posteriors[["BRHO_D"]]$surv <- surv.sum[species == "BRHO",]$surv.mean.p
posteriors[["GITR_D"]]$surv <- surv.sum[species == "GITR",]$surv.mean.p
posteriors[["AMME_D"]]$surv <- surv.sum[species == "AMME",]$surv.mean.p
posteriors[["PLNO_D"]]$surv <- surv.sum[species == "PLNO",]$surv.mean.p
posteriors[["THIR_D"]]$surv <- surv.sum[species == "THIR",]$surv.mean.p
posteriors[["MICA_D"]]$surv <- surv.sum[species == "MICA",]$surv.mean.p
posteriors[["CESO_D"]]$surv <- surv.sum[species == "CESO",]$surv.mean.p
posteriors[["TWIL_D"]]$surv <- surv.sum[species == "TWIL",]$surv.mean.p
posteriors[["LOMU_D"]]$surv <- surv.sum[species == "LOMU",]$surv.mean.p
posteriors[["TACA_D"]]$surv <- surv.sum[species == "TACA",]$surv.mean.p
posteriors[["MAEL_D"]]$surv <- surv.sum[species == "MAEL",]$surv.mean.p
posteriors[["LENI_D"]]$surv <- surv.sum[species == "LENI",]$surv.mean.p
posteriors[["AVBA_D"]]$surv <- surv.sum[species == "AVBA",]$surv.mean.p

posteriors[["PLER_C"]]$surv <- surv.sum[species == "PLER",]$surv.mean.p
posteriors[["ANAR_C"]]$surv <- surv.sum[species == "ANAR",]$surv.mean.p
posteriors[["ACAM_C"]]$surv <- surv.sum[species == "ACAM",]$surv.mean.p
posteriors[["BRNI_C"]]$surv <- surv.sum[species == "BRNI",]$surv.mean.p
posteriors[["CLPU_C"]]$surv <- surv.sum[species == "CLPU",]$surv.mean.p
posteriors[["BRHO_C"]]$surv <- surv.sum[species == "BRHO",]$surv.mean.p
posteriors[["GITR_C"]]$surv <- surv.sum[species == "GITR",]$surv.mean.p
posteriors[["AMME_C"]]$surv <- surv.sum[species == "AMME",]$surv.mean.p
posteriors[["PLNO_C"]]$surv <- surv.sum[species == "PLNO",]$surv.mean.p
posteriors[["THIR_C"]]$surv <- surv.sum[species == "THIR",]$surv.mean.p
posteriors[["MICA_C"]]$surv <- surv.sum[species == "MICA",]$surv.mean.p
posteriors[["CESO_C"]]$surv <- surv.sum[species == "CESO",]$surv.mean.p
posteriors[["TWIL_C"]]$surv <- surv.sum[species == "TWIL",]$surv.mean.p
posteriors[["LOMU_C"]]$surv <- surv.sum[species == "LOMU",]$surv.mean.p
posteriors[["TACA_C"]]$surv <- surv.sum[species == "TACA",]$surv.mean.p
posteriors[["MAEL_C"]]$surv <- surv.sum[species == "MAEL",]$surv.mean.p
posteriors[["LENI_C"]]$surv <- surv.sum[species == "LENI",]$surv.mean.p
posteriors[["AVBA_C"]]$surv <- surv.sum[species == "AVBA",]$surv.mean.p

# need to add BRNI and LENI when ready; also fix, dont like how sketchy this is
all_intra <- c("alpha_pler", "alpha_pler", 
               "alpha_anar", "alpha_anar", 
               "alpha_acam", "alpha_acam", 
               "alpha_brni", "alpha_brni",
               "alpha_clpu", "alpha_clpu", 
               "alpha_brho", "alpha_brho", 
               "alpha_gitr", "alpha_gitr", 
               "alpha_amme", "alpha_amme", 
               "alpha_plno", "alpha_plno", 
               "alpha_thir", "alpha_thir", 
               "alpha_mica", "alpha_mica", 
               "alpha_ceso", "alpha_ceso", 
               "alpha_twil", "alpha_twil", 
               "alpha_lomu", "alpha_lomu", 
               "alpha_taca", "alpha_taca", 
               "alpha_mael", "alpha_mael", 
               "alpha_leni", "alpha_leni", 
               "alpha_avba", "alpha_avba")

# Run to Equilibrium ####
## set up loop ####
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

## loop thru all posteriors ####
for(i in 1:length(names(posteriors))) {
  
  ## select a particular species treatment combo
  datset <- posteriors[[i]] 
  
  ## set the intraspecific alpha name
  intra <- paste0("alpha_", tolower(substr(names(posteriors)[i], 1, 4)))
  
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
                                     Nt = N[i, ,t]) 
    
  }
  
  ## if it is a drought model
  if(str_sub(names(posteriors)[i], start = -1) == "D"){
    
    ## tmp.df is a vector of the final abundance of all 200 separate runs - will become a column in the output dataframe
    tmp.df <- data.frame(N[i,,300])
    
    ## change the column name to be the species
    names(tmp.df) <- substr(names(posteriors)[i], 1, 4)
    
    ## append temporary df to the empty df created earlier
    residents_dry <-  cbind(residents_dry,  tmp.df)
    
  }
  
  ## if it is a control model
  else{
    
    ## same steps as above, just appended to different output dataframe
    tmp.df <- data.frame(N[i,,300])
    names(tmp.df) <- substr(names(posteriors)[i], 1, 4)
    residents_wet <-  cbind(residents_wet,  tmp.df)
    
  }
}

# This was sloppily done by Marina, remove first column
residents_wet <- residents_wet[,-1] 
residents_dry <- residents_dry[,-1]

## Check Equil Abundances ####

## change to long format for visualization
residents_wet_long <- residents_wet %>%
  mutate(num = 1:length(PLER)) %>%
  pivot_longer(1:18, names_to = "species", values_to = "equil_abund")

residents_dry_long <- residents_dry %>%
  mutate(num = 1:length(PLER)) %>%
  pivot_longer(1:18, names_to = "species", values_to = "equil_abund")

## visualize
ggplot(residents_wet_long, aes(x=num, y=equil_abund)) +
  geom_point() +
  facet_wrap(~species) +
  xlab("Run Number") + ylab("Equilibrium Abundance") +
  ggtitle("Ambient Conditions, Ricker Model, 4/12/23")

#ggsave("models/CW/classic_MCT/equil_abund_ricker_ambient.png", height = 6, width = 10)

ggplot(residents_dry_long, aes(x=num, y=equil_abund)) +
  geom_point() +
  facet_wrap(~species) +
  xlab("Run Number") + ylab("Equilibrium Abundance") +
  ggtitle("Drought Conditions, Ricker Model, 4/12/23")

#ggsave("models/CW/classic_MCT/equil_abund_ricker_drought.png", height = 6, width = 10)




# Invade into residents ####
## Filter species ####
# species having a hard time reaching equilibrium
## dry: TWIL, CESO, PLNO
## wet: TWIL, THIR, AMME, ACAM

rm_dry <- c("TWIL", "CESO", "PLNO", "MAEL")
rm_wet <- c("TWIL", "THIR", "AMME", "ACAM")

rm <- c("TWIL", "THIR", "AMME", "ACAM", "CESO", "PLNO", "MAEL")

#residents_wet <- residents_wet[,!colnames(residents_wet) %in% rm_wet]
#residents_dry <- residents_dry[,!colnames(residents_dry) %in% rm_dry]


#update species list
species <- species[!(species %in% rm)]


reps <- 200

tmp <- list()

# i = invader, j = resident
for(i in species) {
  for(j in species) {
    if(i != j) {
      for(k in trt){
        tmp[[paste0(i, "_into_", j, "_", k)]] <- matrix(NA, reps, runs)
        invader_abund <- 1
        post_length <- length(posteriors[[paste0(i,"_", k)]]$lambda)
        
        for(r in 1:reps) {
          posts <- sample(post_length, runs, replace=TRUE)
          
          tmp[[paste0(i, "_into_", j, "_", k)]][r,] <- run.invader(#surv = avfa_dry$surv, 
            germ = posteriors[[paste0(i,"_", k)]]$germ, 
            lambda = posteriors[[paste0(i,"_", k)]]$lambda[posts], 
            alpha_inter = unlist(posteriors[[paste0(i,"_", k)]][paste0("alpha_", tolower(j))], use.names = F)[posts],
            resid_abund = residents_dry[,j], 
            invader_abund = invader_abund)
        }
      }
    }
  }
}

# Put together ####
invasion_dry <- list()
invasion_wet <- list()

for(i in names(tmp)){
  if(str_sub(names(tmp[i]), start = -1) == "C"){
    tmp2 <- as.vector(tmp[[i]])
    invasion_dry[[i]] <- tmp2
  }
  
  else {
    tmp3 <- as.vector(tmp[[i]])
    invasion_wet[[i]] <- tmp3
  }
}

invasion_dry <- data.frame(invasion_dry)
invasion_wet <- data.frame(invasion_wet)


# mean abundances ####
equil_abund <- as.data.frame(rbind(apply(residents_dry, 2, mean),
                                   apply(residents_wet, 2, mean)))
equil_abund$trt <- c("dry","wet")

equil_abund <- equil_abund %>%
  pivot_longer(cols = PLER:MICA, names_to = "species", values_to = "abundance")


#rm(list=setdiff(ls(), c("invasion_dry", "invasion_wet","equil_abund","params")))

# plot ####

dry_means <- invasion_dry %>% 
  summarise_all(list(mean))

wet_means <- invasion_wet %>% 
  summarise_all(list(mean))

colnames(wet_means) <- str_sub(names(wet_means), start = 1, end = 14)
colnames(dry_means) <- str_sub(names(dry_means), start = 1, end = 14)

invasion_means <- rbind(dry_means, wet_means)
invasion_means$trt <- c("dry","wet")

invasion_means <- invasion_means %>%
  pivot_longer(cols = PLER_into_BRHO:MICA_into_LOMU, 
               names_to = "invasion", values_to = "growth")

invasion_means$invader <- str_sub(invasion_means$invasion, start = 1, end = 4)
invasion_means$resident <- str_sub(invasion_means$invasion, start = 11, end = 14)

ggplot(invasion_means, aes(x = resident, y = growth, col = trt, group = trt)) + 
  geom_point(size = 3) + 
  facet_wrap(~invader, ncol = 3, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed")

#trait <- read.csv("/users/Marina/Documents/Dropbox/Mega_Competition/Data/Traits/Megacomp_adult-traits.csv")

invasion_means <- merge(invasion_means, trait[,c(2,3,6)], by.x = "invader", by.y = "code_4")

names(invasion_means)[6] <- "invader.nativity"
names(invasion_means)[7] <- "invader.growth_form"

invasion_means <- merge(invasion_means, trait[,c(2,3,6)], by.x = "resident", by.y = "code_4")

names(invasion_means)[8] <- "resident.nativity"
names(invasion_means)[9] <- "resident.growth_form"

invasion_means$invader.fungroup <- paste(invasion_means$invader.nativity, invasion_means$invader.growth_form, sep = " ")

invasion_means$resident.fungroup <- paste(invasion_means$resident.nativity, invasion_means$resident.growth_form, sep = " ")

ggplot(invasion_means, aes(x = resident.fungroup, y = growth, fill = trt)) +
  geom_boxplot() +
  facet_wrap(~invader.fungroup, ncol = 3, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed")

invasion_means_summary <- invasion_means %>%
  group_by(trt, invader.fungroup, resident.fungroup) %>%
  summarize(ldgr.mean = mean(growth),
            ldgr.se = calcSE(growth))

ggplot(invasion_means_summary, aes(x = resident.fungroup, y = ldgr.mean, col = trt, group = trt)) +
  geom_point() +
  geom_errorbar(aes(ymin = ldgr.mean - ldgr.se, ymax = ldgr.mean + ldgr.se, width = 0.2)) +
  facet_wrap(~invader.fungroup, ncol = 3, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed")
