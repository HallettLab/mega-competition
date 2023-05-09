# run coexistence models for dry versus wet
# survival isn't in these models for now

source("Models/CW/import_BH_posteriors.R")
source("data_cleaning/seed-survival_data-cleaning/seed-survival_data-cleaning.R")
source("data_cleaning/germination_data-cleaning/germination_rates.R") ## for germ data

reps.df <- read.csv("models/CW/replicate-info.csv")

germ.rates <- germ.sum.sp.DC %>%
  mutate(species = ifelse(species == "TWIL-I", "TWIL", species), 
         species = ifelse(species == "THIR-I", "THIR", species))

theme_set(theme_bw())

# Set up Models ####

## run resident species to equilibrium in isolation
run.to.equilibrium <- function(surv, germ, lambda, alpha_intra, Nt) {
  Ntp1 <- (1-germ)*surv*Nt + germ*lambda*Nt/(1 + alpha_intra * germ*Nt)
  return(Ntp1)
  
}
## added germination term to the competitive effect reducing lambda - don't know why this wasn't included initially- seems important.

## invasion growth rate
run.invader <- function(surv, germ, lambda, alpha_inter, resid_abund, invader_abund) {
  Ntp1 <- (1-germ)*surv*invader_abund + germ*lambda*invader_abund/(1 + alpha_inter * resid_abund)
  LDGR <- log(Ntp1/invader_abund)
  return(LDGR)
  
}
    ## need to look at the invasion growth rate formula - Lauren's code does not take the log of Ntp1/invader_abund.
    ## Andrew's code does 
    ## is there a difference in approach?

# Set Germ Rates ####
# germ rates dry
BH_posteriors[["PLER_D"]]$germ <- germ.rates[germ.rates$species == "PLER" & germ.rates$trt == "D",]$avg.germ
BH_posteriors[["ANAR_D"]]$germ <- germ.rates[germ.rates$species == "ANAR" & germ.rates$trt == "D",]$avg.germ
BH_posteriors[["ACAM_D"]]$germ <- germ.rates[germ.rates$species == "ACAM" & germ.rates$trt == "D",]$avg.germ
BH_posteriors[["BRNI_D"]]$germ <- germ.rates[germ.rates$species == "BRNI" & germ.rates$trt == "D",]$avg.germ
BH_posteriors[["CLPU_D"]]$germ <- germ.rates[germ.rates$species == "CLPU" & germ.rates$trt == "D",]$avg.germ
BH_posteriors[["BRHO_D"]]$germ <- germ.rates[germ.rates$species == "BRHO" & germ.rates$trt == "D",]$avg.germ
BH_posteriors[["GITR_D"]]$germ <- germ.rates[germ.rates$species == "GITR" & germ.rates$trt == "D",]$avg.germ
BH_posteriors[["AMME_D"]]$germ <- germ.rates[germ.rates$species == "AMME" & germ.rates$trt == "D",]$avg.germ
BH_posteriors[["PLNO_D"]]$germ <- germ.rates[germ.rates$species == "PLNO" & germ.rates$trt == "D",]$avg.germ
BH_posteriors[["THIR_D"]]$germ <- germ.rates[germ.rates$species == "THIR" & germ.rates$trt == "D",]$avg.germ
BH_posteriors[["MICA_D"]]$germ <- germ.rates[germ.rates$species == "MICA" & germ.rates$trt == "D",]$avg.germ
BH_posteriors[["CESO_D"]]$germ <- germ.rates[germ.rates$species == "CESO" & germ.rates$trt == "D",]$avg.germ
BH_posteriors[["TWIL_D"]]$germ <- germ.rates[germ.rates$species == "TWIL" & germ.rates$trt == "D",]$avg.germ
BH_posteriors[["LOMU_D"]]$germ <- germ.rates[germ.rates$species == "LOMU" & germ.rates$trt == "D",]$avg.germ
BH_posteriors[["TACA_D"]]$germ <- germ.rates[germ.rates$species == "TACA" & germ.rates$trt == "D",]$avg.germ
BH_posteriors[["MAEL_D"]]$germ <- germ.rates[germ.rates$species == "MAEL" & germ.rates$trt == "D",]$avg.germ
BH_posteriors[["LENI_D"]]$germ <- germ.rates[germ.rates$species == "LENI" & germ.rates$trt == "D",]$avg.germ
BH_posteriors[["AVBA_D"]]$germ <- germ.rates[germ.rates$species == "AVBA" & germ.rates$trt == "D",]$avg.germ

# germ rates wet
BH_posteriors[["PLER_C"]]$germ <- germ.rates[germ.rates$species == "PLER" & germ.rates$trt == "C",]$avg.germ
BH_posteriors[["ANAR_C"]]$germ <- germ.rates[germ.rates$species == "ANAR" & germ.rates$trt == "C",]$avg.germ
BH_posteriors[["ACAM_C"]]$germ <- germ.rates[germ.rates$species == "ACAM" & germ.rates$trt == "C",]$avg.germ
BH_posteriors[["BRNI_C"]]$germ <- germ.rates[germ.rates$species == "BRNI" & germ.rates$trt == "C",]$avg.germ
BH_posteriors[["CLPU_C"]]$germ <- germ.rates[germ.rates$species == "CLPU" & germ.rates$trt == "C",]$avg.germ
BH_posteriors[["BRHO_C"]]$germ <- germ.rates[germ.rates$species == "BRHO" & germ.rates$trt == "C",]$avg.germ
BH_posteriors[["GITR_C"]]$germ <- germ.rates[germ.rates$species == "GITR" & germ.rates$trt == "C",]$avg.germ
BH_posteriors[["AMME_C"]]$germ <- germ.rates[germ.rates$species == "AMME" & germ.rates$trt == "C",]$avg.germ
BH_posteriors[["PLNO_C"]]$germ <- germ.rates[germ.rates$species == "PLNO" & germ.rates$trt == "C",]$avg.germ
BH_posteriors[["THIR_C"]]$germ <- germ.rates[germ.rates$species == "THIR" & germ.rates$trt == "C",]$avg.germ
BH_posteriors[["MICA_C"]]$germ <- germ.rates[germ.rates$species == "MICA" & germ.rates$trt == "C",]$avg.germ
BH_posteriors[["CESO_C"]]$germ <- germ.rates[germ.rates$species == "CESO" & germ.rates$trt == "C",]$avg.germ
BH_posteriors[["TWIL_C"]]$germ <- germ.rates[germ.rates$species == "TWIL" & germ.rates$trt == "C",]$avg.germ
BH_posteriors[["LOMU_C"]]$germ <- germ.rates[germ.rates$species == "LOMU" & germ.rates$trt == "C",]$avg.germ
BH_posteriors[["TACA_C"]]$germ <- germ.rates[germ.rates$species == "TACA" & germ.rates$trt == "C",]$avg.germ
BH_posteriors[["MAEL_C"]]$germ <- germ.rates[germ.rates$species == "MAEL" & germ.rates$trt == "C",]$avg.germ
BH_posteriors[["LENI_C"]]$germ <- germ.rates[germ.rates$species == "LENI" & germ.rates$trt == "C",]$avg.germ
BH_posteriors[["AVBA_C"]]$germ <- germ.rates[germ.rates$species == "AVBA" & germ.rates$trt == "C",]$avg.germ

## Seed Survival ####
## add seed survival to models
BH_posteriors[["PLER_D"]]$surv <- surv.sum[surv.sum$species == "PLER",]$surv.mean.p
BH_posteriors[["ANAR_D"]]$surv <- surv.sum[surv.sum$species == "ANAR",]$surv.mean.p
BH_posteriors[["ACAM_D"]]$surv <- surv.sum[surv.sum$species == "ACAM",]$surv.mean.p
BH_posteriors[["BRNI_D"]]$surv <- surv.sum[surv.sum$species == "BRNI",]$surv.mean.p
BH_posteriors[["CLPU_D"]]$surv <- surv.sum[surv.sum$species == "CLPU",]$surv.mean.p
BH_posteriors[["BRHO_D"]]$surv <- surv.sum[surv.sum$species == "BRHO",]$surv.mean.p
BH_posteriors[["GITR_D"]]$surv <- surv.sum[surv.sum$species == "GITR",]$surv.mean.p
BH_posteriors[["AMME_D"]]$surv <- surv.sum[surv.sum$species == "AMME",]$surv.mean.p
BH_posteriors[["PLNO_D"]]$surv <- surv.sum[surv.sum$species == "PLNO",]$surv.mean.p
BH_posteriors[["THIR_D"]]$surv <- surv.sum[surv.sum$species == "THIR",]$surv.mean.p
BH_posteriors[["MICA_D"]]$surv <- surv.sum[surv.sum$species == "MICA",]$surv.mean.p
BH_posteriors[["CESO_D"]]$surv <- surv.sum[surv.sum$species == "CESO",]$surv.mean.p
BH_posteriors[["TWIL_D"]]$surv <- surv.sum[surv.sum$species == "TWIL",]$surv.mean.p
BH_posteriors[["LOMU_D"]]$surv <- surv.sum[surv.sum$species == "LOMU",]$surv.mean.p
BH_posteriors[["TACA_D"]]$surv <- surv.sum[surv.sum$species == "TACA",]$surv.mean.p
BH_posteriors[["MAEL_D"]]$surv <- surv.sum[surv.sum$species == "MAEL",]$surv.mean.p
BH_posteriors[["LENI_D"]]$surv <- surv.sum[surv.sum$species == "LENI",]$surv.mean.p
BH_posteriors[["AVBA_D"]]$surv <- surv.sum[surv.sum$species == "AVBA",]$surv.mean.p

BH_posteriors[["PLER_C"]]$surv <- surv.sum[surv.sum$species == "PLER",]$surv.mean.p
BH_posteriors[["ANAR_C"]]$surv <- surv.sum[surv.sum$species == "ANAR",]$surv.mean.p
BH_posteriors[["ACAM_C"]]$surv <- surv.sum[surv.sum$species == "ACAM",]$surv.mean.p
BH_posteriors[["BRNI_C"]]$surv <- surv.sum[surv.sum$species == "BRNI",]$surv.mean.p
BH_posteriors[["CLPU_C"]]$surv <- surv.sum[surv.sum$species == "CLPU",]$surv.mean.p
BH_posteriors[["BRHO_C"]]$surv <- surv.sum[surv.sum$species == "BRHO",]$surv.mean.p
BH_posteriors[["GITR_C"]]$surv <- surv.sum[surv.sum$species == "GITR",]$surv.mean.p
BH_posteriors[["AMME_C"]]$surv <- surv.sum[surv.sum$species == "AMME",]$surv.mean.p
BH_posteriors[["PLNO_C"]]$surv <- surv.sum[surv.sum$species == "PLNO",]$surv.mean.p
BH_posteriors[["THIR_C"]]$surv <- surv.sum[surv.sum$species == "THIR",]$surv.mean.p
BH_posteriors[["MICA_C"]]$surv <- surv.sum[surv.sum$species == "MICA",]$surv.mean.p
BH_posteriors[["CESO_C"]]$surv <- surv.sum[surv.sum$species == "CESO",]$surv.mean.p
BH_posteriors[["TWIL_C"]]$surv <- surv.sum[surv.sum$species == "TWIL",]$surv.mean.p
BH_posteriors[["LOMU_C"]]$surv <- surv.sum[surv.sum$species == "LOMU",]$surv.mean.p
BH_posteriors[["TACA_C"]]$surv <- surv.sum[surv.sum$species == "TACA",]$surv.mean.p
BH_posteriors[["MAEL_C"]]$surv <- surv.sum[surv.sum$species == "MAEL",]$surv.mean.p
BH_posteriors[["LENI_C"]]$surv <- surv.sum[surv.sum$species == "LENI",]$surv.mean.p
BH_posteriors[["AVBA_C"]]$surv <- surv.sum[surv.sum$species == "AVBA",]$surv.mean.p

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

## create output dataframes - 200 rows, one for each run
residents_dry <- as.data.frame(matrix(data = NA, nrow = 200, ncol = 1))
residents_wet <- as.data.frame(matrix(data = NA, nrow = 200, ncol = 1))

## loop thru all posteriors ####
for(i in 1:length(names(BH_posteriors))) {
  
  ## select a particular species treatment combo
  datset <- BH_posteriors[[i]]
  
  ## set the intraspecific alpha name
  intra <- paste0("alpha_", tolower(substr(names(BH_posteriors)[i], 1, 4)))
  
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
  if(str_sub(names(BH_posteriors)[i], start = -1) == "D"){
    
    ## tmp.df is a vector of the final abundance of all 200 separate runs - will become a column in the output dataframe
    tmp.df <- data.frame(N[i,,300])
    
    ## change the column name to be the species
    names(tmp.df) <- substr(names(BH_posteriors)[i], 1, 4)
    
    ## append temporary df to the empty df created earlier
    residents_dry <-  cbind(residents_dry,  tmp.df)
    
  }
  
  ## if it is a control model
  else{
    
    ## same steps as above, just appended to different output dataframe
    tmp.df <- data.frame(N[i,,300])
    names(tmp.df) <- substr(names(BH_posteriors)[i], 1, 4)
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
  facet_wrap(~species, scales = "free") +
  xlab("Run Number") + ylab("Equilibrium Abundance") +
  ggtitle("Ambient Conditions, BH Model, Filt Dat, 4/25/23") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed")

ggsave("models/CW/classic_MCT/preliminary_equil_abundance/equil_abund_BH_ambient.png", height = 6, width = 10)

ggplot(residents_dry_long, aes(x=num, y=equil_abund)) +
  geom_point() +
  facet_wrap(~species, scales = "free") +
  xlab("Run Number") + ylab("Equilibrium Abundance") +
  ggtitle("Drought Conditions, BH Model, Filt Dat, 4/25/23") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed")

ggsave("models/CW/classic_MCT/preliminary_equil_abundance/equil_abund_BH_drought.png", height = 6, width = 10)

# species with negative abundances
## dry: ACAM, AMME, MAEL, PLNO, THIR, TWIL
## wet: ACAM, AMME, CESO, THIR

# Invade into residents ####
## Prep Loop ####
rm <- c("TWIL", "CESO", "PLNO", "THIR", "AMME", "ACAM", "MAEL")

residents_wet <- residents_wet[,!colnames(residents_wet) %in% rm]
residents_dry <- residents_dry[,!colnames(residents_dry) %in% rm]

#update species list
species <- colnames(residents_dry)

## set number of replicates
reps <- 200

## create empty list to contain output
tmp <- list()

# i = invader, j = resident
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
        post_length <- length(BH_posteriors[[paste0(i,"_", k)]]$lambda)
        
        ## for each replicate
        for(r in 1:reps) {
          
          ## randomly sample 200 times from the length of the posterior distrib
          posts <- sample(post_length, runs, replace=TRUE)
          
          ## use the run.invader function to add rows to the matrix
          tmp[[paste0(i, "_into_", j, "_", k)]][r,] <- run.invader(surv = BH_posteriors[[paste0(i,"_", k)]]$surv, 
            germ = BH_posteriors[[paste0(i,"_", k)]]$germ, 
            lambda = BH_posteriors[[paste0(i,"_", k)]]$lambda[posts], 
            alpha_inter = unlist(BH_posteriors[[paste0(i,"_", k)]][paste0("alpha_", tolower(j))], use.names = F)[posts],
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

# Change to DF ####
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

# Filter by Combos ####
## filter to remove combos without enough replicates
good.reps <- reps.df %>%
  filter(true.reps > 3)

good.reps.vec <- unique(good.reps$combos)

invasion_dry_long <- invasion_dry %>%
  pivot_longer(1:110, names_to = "scenario", values_to = "GRWR") %>%
  mutate(invader = substr(scenario, 1, 4),
         resident = substr(scenario, 11, 14), 
         combos = paste(invader, resident, "D", sep = "_")) %>%
  filter(combos %in% good.reps.vec)

invasion_wet_long <- invasion_wet %>%
  pivot_longer(1:110, names_to = "scenario", values_to = "GRWR") %>%
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
    ggtitle(paste0(species[i], "Invader Dry, BH"))
  
  ggsave(p_dry, file=paste0("models/CW/classic_MCT/preliminary_GRWR/", species[i], "_BH_dry.png"), width = 14, height = 10)
  
  p_wet <- ggplot(invasion_wet_long[invasion_wet_long$invader == species[i],], aes(x=GRWR)) +
    geom_histogram(bins = 100) +  
    facet_wrap(~resident, scales = "free") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    ggtitle(paste0(species[i], "Invader Wet, BH"))
  
  ggsave(p_wet, file=paste0("models/CW/classic_MCT/preliminary_GRWR/", species[i], "_BH_wet.png"), width = 14, height = 10)
  
}






 













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

ggplot(invasion_means[invasion_means$resident != "AVBA",], aes(x = resident, y = growth, col = trt, group = trt)) + 
  geom_point(size = 3) + 
  facet_wrap(~invader, ncol = 3, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 45))

ggsave("prelim_GRWR.png", width = 8, height = 7)

lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Traits/"

trait <- read.csv(paste0(lead, "Megacomp_adult-traits.csv"))

invasion_means <- merge(invasion_means, trait[,c(2,3,6,9,10,11,12)], by.x = "invader", by.y = "code_4")

names(invasion_means)[6] <- "invader.nativity"
names(invasion_means)[7] <- "invader.growth_form"

invasion_means <- merge(invasion_means, trait[,c(2,3,6)], by.x = "resident", by.y = "code_4")

names(invasion_means)[12] <- "resident.nativity"
names(invasion_means)[13] <- "resident.growth_form"

invasion_means$invader.fungroup <- paste(invasion_means$invader.nativity, invasion_means$invader.growth_form, sep = " ")

invasion_means$resident.fungroup <- paste(invasion_means$resident.nativity, invasion_means$resident.growth_form, sep = " ")

ggplot(invasion_means, aes(x = resident.fungroup, y = growth, fill = trt)) +
  geom_boxplot() +
  facet_wrap(~invader.fungroup, ncol = 3, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 45))
  

ggsave("grwr.by.fg.png", width = 5, height = 3)

invasion_means_summary <- invasion_means %>%
  group_by(trt, invader.fungroup, resident.fungroup) %>%
  summarize(ldgr.mean = mean(growth),
            ldgr.se = calcSE(growth))

ggplot(invasion_means_summary, aes(x = resident.fungroup, y = ldgr.mean, col = trt, group = trt)) +
  geom_point() +
  geom_errorbar(aes(ymin = ldgr.mean - ldgr.se, ymax = ldgr.mean + ldgr.se, width = 0.2)) +
  facet_wrap(~invader.fungroup, ncol = 3, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed")

ggplot(invasion_means, aes(x=Height_cm, y=growth, color = trt)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(invasion_means, aes(x=LWC, y=growth)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(invasion_means, aes(x=CN, y=growth)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(invasion_means, aes(x=Height_cm, y=growth, color = invader)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(invasion_means, aes(x=SLA_cm2.g, y=growth)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~invader.fungroup)


ggplot(invasion_means, aes(x=Height_cm, y=growth)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~invader.fungroup)


ggplot(invasion_means, aes(x=LWC, y=growth)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~invader.fungroup)

