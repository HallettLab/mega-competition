## Invasion Growth Rates


## reps
replicates <- read.csv("data/replicate-info.csv")


### calc invasion GR 
run.invader <- function(surv, germ, lambda, alpha_inter, resid_abund, invader_abund, germ_resid) {
  Ntp1 <- (1-germ)*surv*invader_abund + germ*lambda*invader_abund*exp(-alpha_intra*invader_abund*germ - alpha_inter*resid_abund*germ_resid)
  LDGR <- log(Ntp1/invader_abund)
  return(LDGR)
}

### calc invasion GR 
run.invader.link <- function(surv, germ, lambda, alpha_inter, resid_abund, invader_abund, germ_resid) {
  Ntp1 <- (1-germ)*surv*invader_abund + germ*invader_abund*exp(lambda*exp(-alpha_intra*invader_abund*germ - alpha_inter*resid_abund*germ_resid))
  LDGR <- log(Ntp1/invader_abund)
  return(LDGR)
}


# Invade into residents ####

## Filter species ####

## didn't have an equilibrium abundance
sp_keep <- c("BRHO", "GITR", "MICA", "TACA")
#residents_wet <- residents_wet[, colnames(residents_wet) %in% sp_keep]
residents_dry <- residents_dry[,colnames(residents_dry) %in% sp_keep]

## Prep Loop ####
#update species list
#species <- species[!(species %in% rm)]
species <- c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL")

## set number of replicates
#reps <- 200
runs <- 200

## I don't currently understand why we have 200 runs and replicates. For this it seems redundant since we calc LDGR once, not in 200 sequential time steps like the equilibrium abundance.

trt <- ("D")

## create empty list to contain output
tmp <- list()

## Run Loop ####
## i = invader, j = resident
## for each invader
for(i in species) {
  
  ## for each resident
  for(j in sp_keep) {
    
    ## when invader and resident are different species
    if(i != j) {
      
      ## for each treatment
      #for(k in trt){
        
        ## create one element of the list
        tmp[[paste0(i, "_into_", j)]] <- matrix(NA, runs)
        
        ## set invader abundance to 1
        invader_abund <- 1
        
        ## create a vector the length of the posterior distrib
        post_length <- length(dry[[i]]$lambda)
        
        ## for each replicate
        #for(r in 1:runs) {
          
          ## randomly sample 200 times from the length of the posterior distrib
        posts <- sample(post_length, runs, replace=TRUE)
          
          ## use the run.invader function to add rows to the matrix
        tmp[[paste0(i, "_into_", j)]] <- run.invader.link(surv = dry[[i]]$surv, 
                                                                  germ = dry[[i]]$germ, 
                                                                  germ_resid = dry[[j]]$germ,
                                                                  lambda = dry[[i]]$lambda[posts], 
                                                                   alpha_inter = unlist(dry[[i]][paste0("alpha_", tolower(j))], use.names = F)[posts],
                                                                   resid_abund = residents_dry[,j],
                                                                   invader_abund = invader_abund)
        
      #}
    }
  }
}


## Change to DF ####
invasion_dry <- list()
invasion_wet <- list()

for(i in names(tmp)){
  #if(str_sub(names(tmp[i]), start = -1) == "C"){
    tmp2 <- as.vector(tmp[[i]])
    invasion_dry[[i]] <- tmp2
 # }
  
 # else {
   ## tmp3 <- as.vector(tmp[[i]])
    #invasion_dry[[i]] <- tmp3
  }
#}

invasion_dry_df <- data.frame(invasion_dry)
#invasion_ <- data.frame(invasion_dry)
invasion_wet_df <- data.frame(invasion_wet)

# Filter by Combos ####
## filter to remove combos without enough replicates
good.reps <- replicates %>%
  filter(true.reps > 3)

good.reps.vec <- unique(good.reps$combos)

invasion_dry_long <- invasion_dry_df %>%
  pivot_longer(1:60, names_to = "scenario", values_to = "GRWR") %>%
  mutate(invader = substr(scenario, 1, 4),
         resident = substr(scenario, 11, 14), 
         combos = paste(invader, resident, "D", sep = "_")) %>%
  filter(combos %in% good.reps.vec)

invasion_wet_long <- invasion_wet_df %>%
  pivot_longer(1:105, names_to = "scenario", values_to = "GRWR") %>%
  mutate(invader = substr(scenario, 1, 4),
         resident = substr(scenario, 11, 14),
         combos = paste(invader, resident, "C", sep = "_")) %>%
  filter(combos %in% good.reps.vec)


sum.GRWR <- invasion_dry_long %>%
  group_by(scenario, invader, resident) %>%
  summarise(mean.GRWR = mean(GRWR, na.rm = T),
            se.GRWR = calcSE(GRWR))

ggplot(sum.GRWR, aes(x=resident, y=mean.GRWR)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean.GRWR - se.GRWR, ymax = mean.GRWR + se.GRWR)) +
  facet_wrap(~invader, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed")













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