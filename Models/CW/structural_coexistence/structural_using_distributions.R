## load structural coexistence functions from Saavedra 2017
source("models/CW/structural_coexistence/Saavedra_2017_code/toolbox_coexistence.R")
source("models/CW/structural_coexistence/Saavedra_2017_code/toolbox_figure.R")


## import Ricker model posteriors
source("models/CW/import_ricker_posteriors.R")

## try out structural coexistence when sampling from the posterior distributions rather than taking the mean


## test with 100 draws from the posterior distrib

# sample()

params1 <- posteriors2 %>%
  select(-(alpha_erbo:lp__)) %>%
  group_by(treatment, species) %>%
  summarise(across(lambda:alpha_avba, sample))
  
  
  
  #mutate(alpha_name2 = toupper(substr(alpha_name, start = 7, stop = 11))) %>%
  select(-lambda, -lp__, -alpha_other, -alpha_name) %>%
  pivot_wider(names_from = alpha_name2, values_from = alpha_value)
  
  
  
  mutate(alpha = sample(alpha_value, 100)) %>%
  

# test <- 


params1$ACAM[[1]][1]

ACAM.sample <- sample(posteriors_long[posteriors_long$species == "ACAM",]$alpha_value, 100)

## change column names to make the for loop easier to write
#colnames(mean_alphas) <- c("treatment", "species", "lambda", "ACAM", "AMME", "ANAR", "AVBA", "BRHO", "BRNI", "CESO", "CLPU", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL")








# Struct. Coexist Calc ####
## Prep ####
## make a vector of all unique species
species <- unique(posteriors_long$species)

## create empty dataframe
allcomm <- data.frame(ACAM = NA, AMME = NA, ANAR = NA, AVBA = NA, BRHO = NA, BRNI=NA, CESO=NA, GITR=NA, LENI = NA, LOMU= NA, MAEL= NA, MICA= NA, PLER= NA, PLNO= NA, TACA= NA, THIR= NA, TWIL = NA, feasibility=NA, rich = NA, treatment = NA, niche_diff = NA, fitness_diff = NA, community = NA)

## create comm richness level
richness <- 1:18

## create vector of treatments
treat <- unique(mean_alphas$treatment)

## Loop ####
## iterate calcs over every comm comp and treatment
for(k in 1:length(treat)){
  
  ## iterate at every richness level
  for(i in 1:length(richness)){
    
    ## create all possible combinations of composition at a given richness level
    comp <- data.frame(comboGeneral(species, m=richness[i], freqs = 1))
    
    ## iterate over each possible community composition
    for(j in 1:nrow(comp)){
      
      ## create a vector of community composition for each iteration
      cc <- as.character(comp[j,])
      
      trt <- treat[k]
      ## select precipitation treatment
      
      ## Randomly sample 100 times
      tmp_params <- posteriors2 %>%
        filter(treatment %in% trt) %>%
        filter(species %in% cc)

      ## select the matching columns
      sp <- unique(tmp_params$species)
      colnames(tmp_params)[grepl(sp,toupper(colnames(tmp_params)))]
      tmp_alphas <- tmp_params %>%
        ungroup() %>%
        select(all_of(sp)) %>%
        as.matrix()
      
      ## pull out intrinsic growth rate vector
      tmp_igr <- as.numeric(tmp_params$lambda)
      
      
      ## iterate over each random sample
      for(l in 1:100){
        
        ## test feasibility
        f <- test_feasibility(alpha = tmp_alphas, r = tmp_igr)
        
        ## calculate structural niche differences
        niche <- Omega(alpha=tmp_alphas)
        
        ## calculate structural fitness differences
        fitness <- theta(alpha = tmp_alphas, r=tmp_igr) 
        
        ## create dataframe
        temp <- data.frame(feasibility=NA)
        
        ## put back into dataframe
        temp$feasibility <- f
        temp$niche_diff <- niche
        temp$fitness_diff <- fitness
        
      }
      
      
      ## get comm composition back into dataframe
      temp <- temp %>%
        mutate(ACAM = ifelse("ACAM" %in% colnames(tmp_alphas), 1, 0), 
               AMME = ifelse("AMME" %in% colnames(tmp_alphas), 1, 0),
               ANAR = ifelse("ANAR" %in% colnames(tmp_alphas), 1, 0),
               AVBA = ifelse("AVBA" %in% colnames(tmp_alphas), 1, 0),
               BRHO = ifelse("BRHO" %in% colnames(tmp_alphas), 1, 0),
               CESO = ifelse("CESO" %in% colnames(tmp_alphas), 1, 0), 
               GITR = ifelse("GITR" %in% colnames(tmp_alphas), 1, 0), 
               LOMU = ifelse("LOMU" %in% colnames(tmp_alphas), 1, 0),
               MAEL = ifelse("MAEL" %in% colnames(tmp_alphas), 1, 0),
               MICA = ifelse("MICA" %in% colnames(tmp_alphas), 1, 0),
               PLER = ifelse("PLER" %in% colnames(tmp_alphas), 1, 0),
               PLNO = ifelse("PLNO" %in% colnames(tmp_alphas), 1, 0), 
               TACA = ifelse("TACA" %in% colnames(tmp_alphas), 1, 0),
               THIR = ifelse("THIR" %in% colnames(tmp_alphas), 1, 0),
               TWIL = ifelse("TWIL" %in% colnames(tmp_alphas), 1, 0), 
               rich = richness[i], ## add richness column
               treatment = trt, 
               community = paste(i,j, sep = "")) ## add treatment column
      
      ## join with original dataframe
      allcomm <- rbind(allcomm, temp)
      
    }
    
  }
  
}




