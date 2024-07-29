## native species ONLY
## structural coexistence
## for 4 species communities

# Set up Env ####
set.seed(10)

library(RcppAlgos)
library(tidyverse)

code_loc <- "analyses/interactions_v_traits/structural_coexistence/Saavedra_2017_code/"

## load structural coexistence functions from Saavedra 2017
source(paste0(code_loc, "toolbox_coexistence.R"))
source(paste0(code_loc, "toolbox_figure.R"))

## load posteriors
source("analyses/interactions_v_traits/random_draws/clean_posteriors.R")

# Set up Loop ####
## create a vector of the number of posterior draws
posts <- 1:3750

## draw 200 posteriors
draws <- sample(posts, 200, replace = FALSE)

## species vector
all.sp <- unique(posts_clean$species) 

rm.sp = c("ANAR", "BRHO", "CESO", "TACA", "THIR", "LOMU", "BRNI", "MAEL")
## taking out invasive species and MAEL currently because it is missing a lot of the interaction data

sp = setdiff(all.sp, rm.sp)

## create all possible combinations of composition at a given richness level
comp4 <- data.frame(comboGeneral(sp, m=4, freqs = 1))

## create empty dataframe
natcommD <- data.frame(GITR = NA, LENI = NA, MICA=NA, PLER= NA, ACAM= NA, AMME= NA, PLNO = NA, TWIL = NA, feasibility=NA, niche_diff = NA, fitness_diff = NA, draw = NA)

## function for N-diff errors ####
#n_diff_error_catch <- function(alpha){
 # tryCatch(
    
  #  {## try the n-diff function from Saavedra 2017
   #   n <- Omega(alpha=alpha)
    #  return(n)
    #},
    #if an error occurs, tell me the error
    #error=function(e) {
     # message('An Error Occurred')
      #print(e)
      
      #return(NA)
      
    #}
  #)
#}

# Run Loop: 4 Sp Calcs ####
## iterate over each possible community composition
for(j in 1:nrow(comp4)){
  
  ## create a vector of community composition for each iteration
  cc <- as.character(comp4[j,])
  
  ## filter data
  prep <- posts_clean %>%
    filter(species %in% cc) %>%
    select(species, lambda_c, paste0("alpha_", tolower(cc[1]), "_d"), paste0("alpha_", tolower(cc[2]), "_d"), paste0("alpha_", tolower(cc[3]), "_d"), paste0("alpha_", tolower(cc[4]), "_d"))
  
  ## iterate over posterior draws
  for (i in 1:length(draws)) {
    
    ## select particular model draw
    d <- draws[i]
    
    ## select one row for every species
    prep2 <- prep[c(d, (d + 3750), (d+(2*3750)), (d+(3*3750)), (d+(4*3750))),] 
    
    ## rename
    names(prep2) <- c("sp", "lambda", cc[1], cc[2], cc[3], cc[4])
    
    ## remove row of NAs that keeps showing up
    prep2 <- prep2 %>%
      filter(!is.na(lambda))
    
    ## select the matching columns
    tmp_alphas <- prep2 %>%
      ungroup() %>%
      select(all_of(cc)) %>%
      as.matrix()
    
    ## pull out intrinsic growth rate vector
    tmp_igr <- as.numeric(prep2$lambda)
    
    
    if (any(is.na(tmp_alphas)) == FALSE) {
      
      ## test feasibility
      f <- test_feasibility(alpha = tmp_alphas, r = tmp_igr)
      
      ## calculate structural niche differences 
      # niche <- n_diff_error_catch(alpha = tmp_alphas)
      
      niche = Omega(alpha = tmp_alphas)
      
      ## the niche function returns a diff result every time... odd; how is it doing the calculation??
      
      ## calculate structural fitness differences
      fitness <- theta(alpha = tmp_alphas, r=tmp_igr) 
      
    } else {
      
      f <- NA
      niche <- NA
      fitness <- NA
      
    }
    
    ## create dataframe
    temp <- data.frame(feasibility=NA)
    
    ## put back into dataframe
    temp$feasibility <- f
    temp$niche_diff <- niche
    temp$fitness_diff <- fitness
    
    ## get comm composition back into dataframe
    temp <- temp %>%
      mutate(ACAM = ifelse("ACAM" %in% colnames(tmp_alphas), 1, 0), 
             AMME = ifelse("AMME" %in% colnames(tmp_alphas), 1, 0),
             GITR = ifelse("GITR" %in% colnames(tmp_alphas), 1, 0), 
             LENI = ifelse("LENI" %in% colnames(tmp_alphas), 1, 0),
             #MAEL = ifelse("MAEL" %in% colnames(tmp_alphas), 1, 0),
             MICA = ifelse("MICA" %in% colnames(tmp_alphas), 1, 0),
             PLER = ifelse("PLER" %in% colnames(tmp_alphas), 1, 0),
             PLNO = ifelse("PLNO" %in% colnames(tmp_alphas), 1, 0), 
             TWIL = ifelse("TWIL" %in% colnames(tmp_alphas), 1, 0), 
             draw = d)
    
    ## join with original dataframe
    natcommD <- rbind(natcommD, temp)
    
  }
}

write.csv(natcommD, "analyses/interactions_v_traits/structural_coexistence/nat_only_D_structural_results_20240729.csv")
