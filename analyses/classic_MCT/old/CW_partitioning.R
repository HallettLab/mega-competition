### Setup
#setwd('./UO Hallett/Projects/usda-climvar')
library(tidyverse)
library(rstan)

### Create historical rainfall conditions ----
# First determine how common each environmental type is
# what about for what we actually see in terms of the number of years in each env. condition
# first read in the data
lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Climate/"

rain <- read.csv(paste0(lead, "PRISM_brownsvalley_long.csv"), skip = 10) %>%
  mutate(ppt = ppt..inches.*2.54*10) %>%
  separate(Date, c("year", "month")) %>%
  mutate(year = as.numeric(year),
         month = as.numeric(month)) %>%
  mutate(year = ifelse(month == 12 | month == 11 | month == 10 | month == 9, year + 1, year)) %>%
  mutate(season = "Early",
         season = ifelse(month == 2 | month == 3 | month == 4, "Late", season)) %>%
  filter(month != 5, month != 6, month!= 7, month != 8)

## Summarize by year 
## Using 50% as the cutoff 
rainsummary <-  rain %>%
  group_by(year, season) %>%
  summarize(ppt = sum(ppt)) %>%
  spread(season, ppt) %>%
  mutate(Total = Early + Late) 

rainsummary <- rainsummary %>%
  mutate(raintype = "fallWet",
         raintype = ifelse(Early < quantile(rainsummary$Early, .5), "fallDry", raintype))

fall.dry <- length(which(rainsummary$raintype == "fallDry")) / nrow(rainsummary)
fall.wet <- length(which(rainsummary$raintype == "fallWet")) / nrow(rainsummary)

## Germination and peak growing season correlation
rain <- read_csv("../avena-erodium/Data/PRISM_brownsvalley_long.csv", skip = 10) %>%
  mutate(ppt = `ppt (inches)`*2.54*10) %>%
  separate(Date, c("year", "month")) %>%
  mutate(year = as.numeric(year),
         month = as.numeric(month)) %>%
  filter(month %in% c(9,10,11,12,1,2,3,4)) %>%
  mutate(year = ifelse(month == 12 | month == 11 | month == 10 | month == 9, year + 1, year)) %>%
  mutate(growth_season = ifelse(month == 12 | month == 11 | month == 10 | month == 9, "early", "late")) %>%
  group_by(year, growth_season) %>%
  summarise(tot_rainfall = sum(ppt)) %>%
  pivot_wider(names_from = growth_season, values_from = tot_rainfall)

plot(rain$early, rain$late)
abline(lm(rain$late ~ rain$early))
#points(rain$early, rain$late)

cor.test(rain$early, rain$late)

### Load or set model parameters ----
source('./Competition/Model-fit/import_posteriors_AM.R')

# Set germination and survival rates
params$germ[params$species == "avfa"] <- 0.78
params$germ[params$species == "brho"] <- 0.8
params$germ[params$species == "vumy"] <- 0.6
params$germ[params$species == "laca"] <- 0.8
params$germ[params$species == "esca"] <- 0.92
params$germ[params$species == "trhi"] <- 0.20

params$surv[params$species == "avfa"] <- 0.01
params$surv[params$species == "brho"] <- 0.013
params$surv[params$species == "vumy"] <- 0.045
params$surv[params$species == "laca"] <- 0.013
params$surv[params$species == "esca"] <- 0.013
params$surv[params$species == "trhi"] <- 0.01

# Create weighted parameters for mechanism partitioning
params_weighted <- params[params$treatment == "fallDry",1:7]*fall.dry + params[params$treatment == "fallWet",1:7]*fall.wet
params_weighted$species <- c("avfa","vumy","brho","esca","laca","trhi")
params_weighted <- left_join(params_weighted, unique(params[,c("species","germ","surv")]))

### Coexistence functions ----
# Determine equilibrium conditions for each species in isolation 
pop.equilibrium <- function (N0, s, g, a_intra, lambda) {
  # to run for only a single timestep
  N <- s*(1-g)*N0 + N0*(lambda*g)/(1+a_intra*N0)
  return(N)
}

# invader population growth rate one time step forward
pop.invade <- function (N0, resident, s, g, a_inter, lambda) {
  # to run for only a single timestep
  N <- s*(1-g)*N0 + N0*(lambda*g)/(1+a_inter*resident)
  return(N)
}

# resident population growth rate one time step forward
pop.resident <- function (N0, resident, s, g, a_intra, a_inter, lambda) {
  # to run for only a single timestep
  N <- s*(1-g)*resident + resident*(lambda*g)/(1+a_intra*resident+a_inter*N0)
  return(N)
}
### Coexistence - No partitioning ----

## Run all species to equilibrium in isolation
# Species to loop across and time in years
species <- c("avfa","brho","vumy","laca","esca","trhi")
time <- length(rainsummary$raintype)

# Pre-allocate empty object to store results, and set initial conditions
N <- matrix(NA, nrow = time+1, ncol = length(species))
colnames(N) <- species
N[1,] <- 300

## Run to equilibrium given historical rainfall conditions
for(i in 1:length(species)){

  resident <- params[params$species == species[i],]
  resident_a <- paste0("alpha_", species[i])
  
  for (t in 1:time) {
    
    sim <- resident[resident$treatment == rainsummary$raintype[t],]
    
    N[t+1,i] <- pop.equilibrium(N0 = N[t,i], s = sim$surv, g = sim$germ, lambda = sim$lambda,
                                a_intra = as.numeric(sim[resident_a]))
  
    }
}

## Run pairwise invasions into equilibrium resident 

# Pre-allocate empty object to store results, a separate matrix for each invader
# Invader
coexist_out <- list(avfa = matrix(NA, nrow = 72, ncol = length(species)-1),
                    brho = matrix(NA, nrow = 72, ncol = length(species)-1),
                    vumy = matrix(NA, nrow = 72, ncol = length(species)-1),
                    laca = matrix(NA, nrow = 72, ncol = length(species)-1),
                    esca = matrix(NA, nrow = 72, ncol = length(species)-1),
                    trhi = matrix(NA, nrow = 72, ncol = length(species)-1))
colnames(coexist_out[['avfa']]) <- species[species != 'avfa']
colnames(coexist_out[['brho']]) <- species[species != 'brho']
colnames(coexist_out[['vumy']]) <- species[species != 'vumy']
colnames(coexist_out[['laca']]) <- species[species != 'laca']
colnames(coexist_out[['esca']]) <- species[species != 'esca']
colnames(coexist_out[['trhi']]) <- species[species != 'trhi']

# Resident
coexist_out_r <- list(avfa = matrix(NA, nrow = 72, ncol = length(species)-1),
                    brho = matrix(NA, nrow = 72, ncol = length(species)-1),
                    vumy = matrix(NA, nrow = 72, ncol = length(species)-1),
                    laca = matrix(NA, nrow = 72, ncol = length(species)-1),
                    esca = matrix(NA, nrow = 72, ncol = length(species)-1),
                    trhi = matrix(NA, nrow = 72, ncol = length(species)-1))
colnames(coexist_out[['avfa']]) <- species[species != 'avfa']
colnames(coexist_out[['brho']]) <- species[species != 'brho']
colnames(coexist_out[['vumy']]) <- species[species != 'vumy']
colnames(coexist_out[['laca']]) <- species[species != 'laca']
colnames(coexist_out[['esca']]) <- species[species != 'esca']
colnames(coexist_out[['trhi']]) <- species[species != 'trhi']

# Loop once for each invader
for (i in 1:length(species)){
  
  invader <- params[params$species == species[i],]
  invader_a <- paste0("alpha_",species[i])
  
  invader_out <- coexist_out[[species[i]]]
  resident_out <- coexist_out_r[[species[i]]]
  
  inter <- species[species != species[i]]
  
  # Loop once for each potential resident that isn't the invader
  for (j in 1:length(inter)){
  
    resident <- params[params$species == inter[j],]
    resident_a <- paste0("alpha_",unique(resident$species))
    
    temp <- 1
    
    # Calculate GRWR for each year after burn-in
    for (t in 50:time) {
      
      sim <- invader[invader$treatment == rainsummary$raintype[t],] 
      sim_r <- resident[resident$treatment == rainsummary$raintype[t],]
      
      invader_out[temp,j] <- pop.invade(N0 = 1, s = sim$surv, g = sim$germ, lambda = sim$lambda,
                                        resident = as.numeric(N[t,sim_r$species]),
                                        a_inter = as.numeric(sim[resident_a]))
      
      resident_out[temp,j] <- pop.resident(N0 = 1, s = sim_r$surv, g = sim_r$germ, lambda = sim_r$lambda,
                                           resident = as.numeric(N[t,sim_r$species]),
                                           a_intra = as.numeric(sim_r[resident_a]),
                                           a_inter = as.numeric(sim_r[invader_a]))/as.numeric(N[t,sim_r$species])
      
      temp  <- temp + 1 
    }
  }
  
  # Save in output object
  coexist_out[[species[i]]] <- invader_out
  coexist_out_r[[species[i]]] <- resident_out
  
}


# Calculate LGDR
invader_no_mech <- lapply(coexist_out, colMeans)
invader_no_mech <- lapply(invader_no_mech, log)

resident_no_mech <- lapply(coexist_out_r, colMeans)
resident_no_mech <- lapply(resident_no_mech, log)

### Coexistence - Variation-independent mechanisms ----

## Run all species to equilibrium with average rainfall conditions
# Species to loop across and time in years
species <- c("avfa","brho","vumy","laca","esca","trhi")
time <- length(rainsummary$raintype)

# Pre-allocate empty object to store results, and set initial conditions
N_no_var <- matrix(NA, nrow = time+1, ncol = length(species))
colnames(N_no_var) <- species
N_no_var[1,] <- 300

## Run to equilibrium given historical rainfall conditions
for(i in 1:length(species)){
  
  resident <- params_weighted[params_weighted$species == species[i],]
  resident_a <- paste0("alpha_", species[i])
  
  for (t in 1:time) {
    
    N_no_var[t+1,i] <- pop.equilibrium(N0 = N_no_var[t,i], s = resident$surv, g = resident$germ, lambda = resident$lambda,
                                a_intra = as.numeric(resident[resident_a]))
  }
}

## Run pairwise invasions into equilibrium resident 

# Pre-allocate empty object to store results, a separate matrix for each invader
# Invader object
invader_no_var <- list(avfa = matrix(NA, nrow = 72, ncol = length(species)-1),
                    brho = matrix(NA, nrow = 72, ncol = length(species)-1),
                    vumy = matrix(NA, nrow = 72, ncol = length(species)-1),
                    laca = matrix(NA, nrow = 72, ncol = length(species)-1),
                    esca = matrix(NA, nrow = 72, ncol = length(species)-1),
                    trhi = matrix(NA, nrow = 72, ncol = length(species)-1))
colnames(invader_no_var[['avfa']]) <- species[species != 'avfa']
colnames(invader_no_var[['brho']]) <- species[species != 'brho']
colnames(invader_no_var[['vumy']]) <- species[species != 'vumy']
colnames(invader_no_var[['laca']]) <- species[species != 'laca']
colnames(invader_no_var[['esca']]) <- species[species != 'esca']
colnames(invader_no_var[['trhi']]) <- species[species != 'trhi']

# Resident object
resident_no_var <- list(avfa = matrix(NA, nrow = 72, ncol = length(species)-1),
                       brho = matrix(NA, nrow = 72, ncol = length(species)-1),
                       vumy = matrix(NA, nrow = 72, ncol = length(species)-1),
                       laca = matrix(NA, nrow = 72, ncol = length(species)-1),
                       esca = matrix(NA, nrow = 72, ncol = length(species)-1),
                       trhi = matrix(NA, nrow = 72, ncol = length(species)-1))
colnames(resident_no_var[['avfa']]) <- species[species != 'avfa']
colnames(resident_no_var[['brho']]) <- species[species != 'brho']
colnames(resident_no_var[['vumy']]) <- species[species != 'vumy']
colnames(resident_no_var[['laca']]) <- species[species != 'laca']
colnames(resident_no_var[['esca']]) <- species[species != 'esca']
colnames(resident_no_var[['trhi']]) <- species[species != 'trhi']

# Loop once for each invader
for (i in 1:length(species)){
  
  # Grab weighted invader parameters
  invader <- params_weighted[params_weighted$species == species[i],]
  
  # Grab empty output matrices for relevant invader 
  invader_out <- invader_no_var[[species[i]]]
  resident_out <- resident_no_var[[species[i]]]
  
  # Create a reference for all potential interspecific competitors with the invader i.e. residents
  inter <- species[species != species[i]]
  
  # Loop once for each potential resident that isn't the invader
  for (j in 1:length(inter)){
    
    # Grab weighted resident parameters
    resident <- params_weighted[params_weighted$species == inter[j],]
    # Create a string for referencing the resident's alpha in both invader and resident parameters
    resident_a <- paste0("alpha_",resident$species)
    
    # Create a separate index for row subsets because we're starting out looping variable at 50 instead of 1
    temp <- 1
    
    # Calculate GRWR for each year after burn-in (after 50 years)
    for (t in 50:time) {
      
      # Calculate the population growth rate for both invader and resident and store output 
      invader_out[temp,j] <- pop.invade(N0 = 1, s = invader$surv, g = invader$germ, lambda = invader$lambda,
                                        resident = as.numeric(N_no_var[t,resident$species]),
                                        a_inter = as.numeric(invader[resident_a]))
      
      resident_out[temp,j] <- pop.resident(N0 = 1, s = resident$surv, g = resident$germ, lambda = resident$lambda,
                                           resident = as.numeric(N_no_var[t,resident$species]),
                                           a_intra = as.numeric(resident[resident_a]),
                                           a_inter = as.numeric(invader[resident_a]))/as.numeric(N_no_var[t,resident$species])
      
      temp  <- temp + 1 
    }
  }
  
  # Save invader out and resident out
  invader_no_var[[species[i]]] <- invader_out
  resident_no_var[[species[i]]] <- resident_out
  
}

# Calculate log LGDR for invader and resident
invader_no_var <- lapply(invader_no_var, colMeans)
invader_eps_0 <- lapply(invader_no_var, log)

resident_no_var <- lapply(resident_no_var, colMeans)
resident_eps_0 <- lapply(resident_no_var, log)

# Subtract the resident from the invader to attain the mechanism partition
ir_eps_0 <- Map('-', invader_eps_0, resident_eps_0)

### Coexistence - Variable lambda ----

## Run all species to equilibrium with variable lambda
# Species to loop across and time in years
species <- c("avfa","brho","vumy","laca","esca","trhi")
time <- length(rainsummary$raintype)

# Pre-allocate empty object to store results, and set initial conditions
N_var_lamb <- matrix(NA, nrow = time+1, ncol = length(species))
colnames(N_var_lamb) <- species
N_var_lamb[1,] <- 300

## Run to equilibrium given historical rainfall conditions
for(i in 1:length(species)){
  
  resident <- params_weighted[params_weighted$species == species[i],]
  resident_var <- params[params$species == species[i],]
  resident_a <- paste0("alpha_", species[i])
  
  for (t in 1:time) {
    
    sim <- resident_var[resident_var$treatment == rainsummary$raintype[t],]
    
    N_var_lamb[t+1,i] <- pop.equilibrium(N0 = N_var_lamb[t,i], s = resident$surv, g = resident$germ, lambda = sim$lambda,
                                       a_intra = as.numeric(resident[resident_a]))
  }
}

## Run pairwise invasions into equilibrium resident 

# Pre-allocate empty object to store results, a separate matrix for each invader
# Invader
invader_var_lamb <- list(avfa = matrix(NA, nrow = 72, ncol = length(species)-1),
                       brho = matrix(NA, nrow = 72, ncol = length(species)-1),
                       vumy = matrix(NA, nrow = 72, ncol = length(species)-1),
                       laca = matrix(NA, nrow = 72, ncol = length(species)-1),
                       esca = matrix(NA, nrow = 72, ncol = length(species)-1),
                       trhi = matrix(NA, nrow = 72, ncol = length(species)-1))
colnames(invader_var_lamb[['avfa']]) <- species[species != 'avfa']
colnames(invader_var_lamb[['brho']]) <- species[species != 'brho']
colnames(invader_var_lamb[['vumy']]) <- species[species != 'vumy']
colnames(invader_var_lamb[['laca']]) <- species[species != 'laca']
colnames(invader_var_lamb[['esca']]) <- species[species != 'esca']
colnames(invader_var_lamb[['trhi']]) <- species[species != 'trhi']

# Resident object
resident_var_lamb <- list(avfa = matrix(NA, nrow = 72, ncol = length(species)-1),
                         brho = matrix(NA, nrow = 72, ncol = length(species)-1),
                         vumy = matrix(NA, nrow = 72, ncol = length(species)-1),
                         laca = matrix(NA, nrow = 72, ncol = length(species)-1),
                         esca = matrix(NA, nrow = 72, ncol = length(species)-1),
                         trhi = matrix(NA, nrow = 72, ncol = length(species)-1))
colnames(resident_var_lamb[['avfa']]) <- species[species != 'avfa']
colnames(resident_var_lamb[['brho']]) <- species[species != 'brho']
colnames(resident_var_lamb[['vumy']]) <- species[species != 'vumy']
colnames(resident_var_lamb[['laca']]) <- species[species != 'laca']
colnames(resident_var_lamb[['esca']]) <- species[species != 'esca']
colnames(resident_var_lamb[['trhi']]) <- species[species != 'trhi']

# Loop once for each invader
for (i in 1:length(species)){
  
  # Grab weighted invader parameters
  invader <- params_weighted[params_weighted$species == species[i],]
  # Grab all invader parameters to vary by time
  invader_var <- params[params$species == species[i],]
  
  # Grab empty output matrices for relevant invader
  invader_out <- invader_var_lamb[[species[i]]]
  resident_out <- resident_var_lamb[[species[i]]]
  
  # Create a reference for all potential interspecific competitors with the invader i.e. residents
  inter <- species[species != species[i]]
  
  # Loop once for each potential resident that isn't the invader
  for (j in 1:length(inter)){
    
    # Grab weighted resident parameters
    resident <- params_weighted[params_weighted$species == inter[j],]
    # Grab all resident parameters to vary by time
    resident_var <- params[params$species == inter[j],]
    # Create a string for referencing the resident's alpha in both invader and resident parameters
    resident_a <- paste0("alpha_",resident$species)
    
    # Create a separate index for row subsets because we're starting out looping variable at 50 instead of 1
    temp <- 1
    
    # Calculate GRWR for each year after burn-in
    for (t in 50:time) {
      
      # Subset out the specific parameters associated with the current year's (t) conditions
      sim <- invader_var[invader_var$treatment == rainsummary$raintype[t],]
      sim_r <- resident_var[resident_var$treatment == rainsummary$raintype[t],]
      
      # Calculate the population growth rate for both invader and resident and store output
      # Subset lambda specifically from "sim", allowing it to vary with time, while all other parameters are weighted
      invader_out[temp,j] <- pop.invade(N0 = 1, s = invader$surv, g = invader$germ, lambda = sim$lambda,
                                        resident = as.numeric(N_var_lamb[t,resident$species]),
                                        a_inter = as.numeric(invader[resident_a]))
      
      resident_out[temp,j] <- pop.resident(N0 = 1, s = resident$surv, g = resident$germ, lambda = sim_r$lambda,
                                           resident = as.numeric(N_var_lamb[t,resident$species]),
                                           a_intra = as.numeric(resident[resident_a]),
                                           a_inter = as.numeric(invader[resident_a]))/as.numeric(N_var_lamb[t,resident$species])
      
      temp  <- temp + 1 
    }
  }
  
  # Save in output object
  invader_var_lamb[[species[i]]] <- invader_out
  resident_var_lamb[[species[i]]] <- resident_out
  
}

# Calculate log LGDR for invader and resident, then subtract out influence of variation-independent mechanisms
invader_var_lamb <- lapply(invader_var_lamb, colMeans)
invader_var_lamb <- lapply(invader_var_lamb, log)
invader_eps_lamb <- Map('-', invader_var_lamb, invader_eps_0)

resident_var_lamb <- lapply(resident_var_lamb, colMeans)
resident_var_lamb <- lapply(resident_var_lamb, log)
resident_eps_lamb <- Map('-', resident_var_lamb, resident_eps_0)

# Subtract the resident from the invader to attain the mechanism partition
ir_eps_lamb <- Map('-', invader_eps_lamb, resident_eps_lamb)

### Coexistence - variable alpha ----

## Run all species to equilibrium with variable lambda
# Species to loop across and time in years
species <- c("avfa","brho","vumy","laca","esca","trhi")
time <- length(rainsummary$raintype)

# Pre-allocate empty object to store results, and set initial conditions
N_var_alph <- matrix(NA, nrow = time+1, ncol = length(species))
colnames(N_var_alph) <- species
N_var_alph[1,] <- 300

## Run to equilibrium given historical rainfall conditions
for(i in 1:length(species)){
  
  resident <- params_weighted[params_weighted$species == species[i],]
  resident_var <- params[params$species == species[i],]
  resident_a <- paste0("alpha_", species[i])
  
  for (t in 1:time) {
    
    sim <- resident_var[resident_var$treatment == rainsummary$raintype[t],]
    
    N_var_alph[t+1,i] <- pop.equilibrium(N0 = N_var_alph[t,i], s = resident$surv, g = resident$germ, lambda = resident$lambda,
                                         a_intra = as.numeric(sim[resident_a]))
  }
}

## Run pairwise invasions into equilibrium resident 

# Pre-allocate empty object to store results, a separate matrix for each invader
# Invader
invader_var_alph <- list(avfa = matrix(NA, nrow = 72, ncol = length(species)-1),
                         brho = matrix(NA, nrow = 72, ncol = length(species)-1),
                         vumy = matrix(NA, nrow = 72, ncol = length(species)-1),
                         laca = matrix(NA, nrow = 72, ncol = length(species)-1),
                         esca = matrix(NA, nrow = 72, ncol = length(species)-1),
                         trhi = matrix(NA, nrow = 72, ncol = length(species)-1))
colnames(invader_var_alph[['avfa']]) <- species[species != 'avfa']
colnames(invader_var_alph[['brho']]) <- species[species != 'brho']
colnames(invader_var_alph[['vumy']]) <- species[species != 'vumy']
colnames(invader_var_alph[['laca']]) <- species[species != 'laca']
colnames(invader_var_alph[['esca']]) <- species[species != 'esca']
colnames(invader_var_alph[['trhi']]) <- species[species != 'trhi']

# Resident
resident_var_alph <- list(avfa = matrix(NA, nrow = 72, ncol = length(species)-1),
                         brho = matrix(NA, nrow = 72, ncol = length(species)-1),
                         vumy = matrix(NA, nrow = 72, ncol = length(species)-1),
                         laca = matrix(NA, nrow = 72, ncol = length(species)-1),
                         esca = matrix(NA, nrow = 72, ncol = length(species)-1),
                         trhi = matrix(NA, nrow = 72, ncol = length(species)-1))
colnames(resident_var_alph[['avfa']]) <- species[species != 'avfa']
colnames(resident_var_alph[['brho']]) <- species[species != 'brho']
colnames(resident_var_alph[['vumy']]) <- species[species != 'vumy']
colnames(resident_var_alph[['laca']]) <- species[species != 'laca']
colnames(resident_var_alph[['esca']]) <- species[species != 'esca']
colnames(resident_var_alph[['trhi']]) <- species[species != 'trhi']

# Loop once for each invader
for (i in 1:length(species)){
  
  # Grab weighted invader parameters
  invader <- params_weighted[params_weighted$species == species[i],]
  # Grab all invader parameters to vary by time
  invader_var <- params[params$species == species[i],]
  
  # Grab empty output matrices for relevant invader 
  invader_out <- invader_var_alph[[species[i]]]
  resident_out <- resident_var_alph[[species[i]]]
  
  # Create a reference for all potential interspecific competitors with the invader i.e. residents
  inter <- species[species != species[i]]
  
  # Loop once for each potential resident that isn't the invader
  for (j in 1:length(inter)){
    
    # Grab weighted resident parameters
    resident <- params_weighted[params_weighted$species == inter[j],]
    # Grab all resident parameters to vary by time
    resident_var <- params[params$species == inter[j],]
    # Create a string for referencing the resident's alpha in both invader and resident parameters
    resident_a <- paste0("alpha_",resident$species)
    invader_a <- paste0("alpha_",invader$species)
    
    # Create a separate index for row subsets because we're starting out looping variable at 50 instead of 1
    temp <- 1
    
    # Calculate GRWR for each year after burn-in
    for (t in 50:time) {
      
      # Subset out the specific parameters associated with the current year's (t) conditions
      sim <- invader_var[invader_var$treatment == rainsummary$raintype[t],]
      sim_r <- resident_var[resident_var$treatment == rainsummary$raintype[t],]
      
      # Calculate the population growth rate for both invader and resident and store output 
      # Subset alphas specifically from "sim", allowing them to vary with time, while all other parameters are weighted
      invader_out[temp,j] <- pop.invade(N0 = 1, s = invader$surv, g = invader$germ, lambda = invader$lambda,
                                        resident = as.numeric(N_var_alph[t,resident$species]),
                                        a_inter = as.numeric(sim[resident_a]))
      
      resident_out[temp,j] <- pop.resident(N0 = 1, s = resident$surv, g = resident$germ, lambda = resident$lambda,
                                           resident = as.numeric(N_var_alph[t,resident$species]),
                                           a_intra = as.numeric(sim_r[resident_a]),
                                           a_inter = as.numeric(sim_r[invader_a]))/as.numeric(N_var_alph[t,resident$species])
      
      temp  <- temp + 1 
    }
  }
  
  # Save in output object
  invader_var_alph[[species[i]]] <- invader_out
  resident_var_alph[[species[i]]] <- resident_out
}

# Calculate log LGDR for invader and resident, then subtract out influence of variation-independent mechanisms
invader_var_alph <- lapply(invader_var_alph, colMeans)
invader_var_alph <- lapply(invader_var_alph, log)
invader_eps_alpha <- Map('-', invader_var_alph, invader_eps_0)

resident_var_alph <- lapply(resident_var_alph, colMeans)
resident_var_alph <- lapply(resident_var_alph, log)
resident_eps_alpha <- Map('-', resident_var_alph, resident_eps_0)

# Subtract the resident from the invader to attain the mechanism partition
ir_eps_alpha <- Map('-', invader_eps_alpha, resident_eps_alpha)

### Coexistence - interaction term ----
## Add all partitioned terms together
invader_eps_int <- Map('+', invader_eps_0, invader_eps_alpha)
invader_eps_int <- Map('+', invader_eps_int, invader_eps_lamb)

resident_eps_int <- Map('+', resident_eps_0, resident_eps_alpha)
resident_eps_int <- Map('+', resident_eps_int, resident_eps_lamb)

## Subtract partitioned terms from overall invader growth rate
invader_eps_int <- Map('-', invader_no_mech, invader_eps_int)
resident_eps_int <- Map('-', resident_no_mech, resident_eps_int)

ir_eps_int <- Map('-', invader_eps_int, resident_eps_int)

## Check everything adds up
invader_check <- Map('+', invader_eps_0, invader_eps_alpha)
invader_check <- Map('+', invader_check, invader_eps_lamb)
invader_check <- Map('+', invader_check, invader_eps_int)

# Check for approximate equality i.e. accounting for very minor rounding differences
all.equal(invader_check, invader_no_mech)

resident_check <- Map('+', resident_eps_0, resident_eps_alpha)
resident_check <- Map('+', resident_check, resident_eps_lamb)
resident_check <- Map('+', resident_check, resident_eps_int)

# Check for approximate equality i.e. accounting for very minor rounding differences
all.equal(resident_check, resident_no_mech)
