
library(RcppAlgos) ## for comboGeneral function

## load structural coexistence functions from Saavedra 2017
source("models/CW/structural_coexistence/Saavedra_2017_code/toolbox_coexistence.R")
source("models/CW/structural_coexistence/Saavedra_2017_code/toolbox_figure.R")


## import Ricker model posteriors
source("models/CW/import_ricker_posteriors.R")





## change column names to make the for loop easier to write
#posteriors3 <- posteriors2 %>% 
 # relocate(sort(colnames(posteriors2))) %>% ## alphabetize
  #select(-alpha_erbo, -alpha_figa, -alpha_gamu, -alpha_hygl, -alpha_other, -alpha_siga, -lp__) ## remove unnecessary cols

## rename
#colnames(posteriors3) <- c("ACAM", "AMME", "ANAR", "AVBA", "BRHO", "BRNI", "CESO", "CLPU", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL", "lambda", "species", "treatment")







# Struct. Coexist Calc ####
## Prep ####

r_samples <- 200 ## how many times to sample

## make a vector of all unique species
species <- unique(posteriors2$species)

combo <- c()

N <- array(NA, c(combo, species, runs))
## create an array where each of the rows is one of the composition combos 
## The columns should be: one for each species, plus feasibility, richness, treatment, niche_diff, and fitness_diff
## Each of the stacked matrices represents a run of the loop





## create empty dataframe
allcomm <- data.frame(ACAM = NA, AMME = NA, ANAR = NA, AVBA = NA, BRHO = NA, BRNI=NA, CESO=NA, GITR=NA, LENI = NA, LOMU= NA, MAEL= NA, MICA= NA, PLER= NA, PLNO= NA, TACA= NA, THIR= NA, TWIL = NA, feasibility=NA, rich = NA, treatment = NA, niche_diff = NA, fitness_diff = NA, community = NA)

## create comm richness level
richness <- 1:18

## create vector of treatments
treat <- unique(posteriors2$treatment)




## Loop ####
## iterate calcs over every comm comp and treatment
for(k in treat){
  
  ## iterate at every richness level
  for(i in richness){
    
    ## create all possible combinations of composition at a given richness level
    comp <- data.frame(comboGeneral(species, m=richness[i], freqs = 1))
    
    ## iterate over each possible community composition (iterate by row)
    for(j in 1:nrow(comp)){
      
      ## create a vector of community composition for each iteration
      cc <- as.character(comp[j,])
      
      
      ## create an empty list
      comp_list <- list()
      
      ## pull out each species in the comp
      for(l in cc) {
        
        comp_list[[l]] <- posteriors[[paste0(l, "_", k)]]
        
      }
      
      ## need to pull out particular columns of these species
      
      test <- list()
      
      for(l in cc) {
        
        for (m in cc)  {
          
          test[[l]][m] <- comp_list[[l]][paste0("alpha", "_", tolower(l))]
          ## ISSUE HERE: list names are not assoc with species yet
        }
        
      }
      
      ## get length of posterior distrib
      posts <- length(test[[l]][[l]])
      
      N <- array(NA, c(species, species, runs))
      ## randomly sample and put into an array?
      test2 <- list()
      for (l in cc) {
        
        for (m in cc)  {
          
        ## randomly sample indices
        sample_posts <- sample(posts, r_samples, replace = TRUE)
        test2 <- test[[l]][[l]][sample_posts]
        
        }
        
      }
      
      
      
      
      
      #test3 <- test[[l]][sample_posts]
      
      #test4 <- test[sample_posts]
      ## then select, randomize, shuffle
      
      
      
      
      ## create a vector of treatments
      trt <- treat[k]
      
      
      
      ## select a particular species treatment combo
      datset <- posteriors[[i]] 
      
      
      ## select precipitation treatment
      tmp_params <- posteriors3 %>%
        filter(treatment %in% trt) %>%
        filter(species %in% cc)

      ## select the matching columns
      sp <- unique(tmp_params$species)
      
      #colnames(tmp_params)[grepl(sp,toupper(colnames(tmp_params)))]
      
      tmp_alphas <- tmp_params %>%
        ungroup() %>%
        select(all_of(sp), species) %>%
        group_by(species) %>%
        mutate()
      
      random_vector <- round(runif(n=100, min = 1, max = length(tmp_alphas$species)/richness[i]))
      ## select 100 rows using the random vector
      test <- tmp_alphas[random_vector,]
      
      ## shuffle the rows
      dataframe[sample(1:nrow(dataframe)),]
      
      ## randonly select 100 rows using a random vector of vector indices that is unique to each vector
      ## shuffle each individual vetor
      
      
      
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




