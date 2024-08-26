## structural coexistence
## for 14 species communities

# Set up ####
set.seed(10)

library(RcppAlgos)

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

## create all possible combinations of composition at a given richness level
## would be better over ACCESS or Talapas
comp4 <- data.frame(comboGeneral(all.sp, m=4, freqs = 1))
comp5 <- data.frame(comboGeneral(all.sp, m=5, freqs = 1))
comp6 <- data.frame(comboGeneral(all.sp, m=6, freqs = 1))
comp7 <- data.frame(comboGeneral(all.sp, m=7, freqs = 1))
comp8 <- data.frame(comboGeneral(all.sp, m=8, freqs = 1))
comp9 <- data.frame(comboGeneral(all.sp, m=9, freqs = 1)) 
comp10 <- data.frame(comboGeneral(all.sp, m=10, freqs = 1))
comp11 <- data.frame(comboGeneral(all.sp, m=11, freqs = 1))
comp12 <- data.frame(comboGeneral(all.sp, m=12, freqs = 1))

## possible to do on my computer
comp13 <- data.frame(comboGeneral(all.sp, m=13, freqs = 1))
comp14 <- data.frame(comboGeneral(all.sp, m=14, freqs = 1))
comp15 <- data.frame(comboGeneral(all.sp, m=15, freqs = 1))
comp16 <- data.frame(comboGeneral(all.sp, m=16, freqs = 1))


## make vector of rainfall treatments
rainfall = c("_c", "_d")

## create empty dataframe
natcomm <- data.frame(ACAM= NA, AMME = NA, GITR = NA, LENI = NA, MAEL = NA, MICA=NA, PLER= NA, PLNO = NA, TWIL = NA, feasibility=NA, niche_diff = NA, fitness_diff = NA, niche_diff_cpd = NA, omega_all = NA, comm_pair_overlap = NA, comm_pair_diff = NA, rainfall = NA, draw = NA)

# 4 Sp Calcs ####
## iterate over each possible community composition
for(j in 1:nrow(comp14)){
  
  ## select composition
  cc <- as.character(comp14[j,])
  
  ## iterate over each rainfall treatment
  for(r in 1:length(rainfall)){
    
    ## select rainfall treatment
    rain = rainfall[r]
    
    ## filter data
    prep <- posts_clean %>%
      filter(species %in% cc) %>%
      select(species, lambda_c, paste0("alpha_", tolower(cc[1]), rain), paste0("alpha_", tolower(cc[2]), rain), paste0("alpha_", tolower(cc[3]), rain), paste0("alpha_", tolower(cc[4]), rain))
    
    ## iterate over posterior draws
    for (i in 1:length(draws)) {
      
      ## select a model draw
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
      
      
      ## if matrix is complete, calculate structural metrics
      if (any(is.na(tmp_alphas)) == FALSE) {
        
        ## test feasibility
        f <- test_feasibility(alpha = tmp_alphas, r = tmp_igr)
        
        ## calculate structural niche differences
        niche = Omega(alpha = tmp_alphas)
        ## the niche function returns a diff result every time... odd; how is it doing the calculation??
        
        ## calculate structural fitness differences
        fitness <- theta(alpha = tmp_alphas, r=tmp_igr) 
        
        ## calculate fx of indirect interactions
        cpd = compute_overlap(tmp_alphas,10000)
        
        niche_cpd = cpd$Omega
        omega_all = cpd$Omega_all
        overlap_cpd = cpd$overlap
        
        ## if there are NAs in the alpha matrix, fill all structural metrics with NA
      } else {
        
        f <- NA
        niche <- NA
        fitness <- NA
        niche_cpd <- NA
        omega_all <- NA
        overlap_cpd <- NA
        
      }
      
      ## create dataframe
      temp <- data.frame(feasibility=NA)
      
      ## put back into dataframe
      temp$feasibility <- f
      temp$niche_diff <- niche
      temp$fitness_diff <- fitness
      
      temp$niche_diff_cpd = niche_cpd
      temp$omega_all = omega_all
      temp$comm_pair_overlap = overlap_cpd
      temp$comm_pair_diff = niche_cpd-omega_all
      
      ## get comm composition back into dataframe
      temp <- temp %>%
        mutate(ACAM = ifelse("ACAM" %in% colnames(tmp_alphas), 1, 0),
               AMME = ifelse("AMME" %in% colnames(tmp_alphas), 1, 0),
               GITR = ifelse("GITR" %in% colnames(tmp_alphas), 1, 0),
               LENI = ifelse("LENI" %in% colnames(tmp_alphas), 1, 0),
               MAEL = ifelse("MAEL" %in% colnames(tmp_alphas), 1, 0),
               MICA = ifelse("MICA" %in% colnames(tmp_alphas), 1, 0),
               PLER = ifelse("PLER" %in% colnames(tmp_alphas), 1, 0),
               PLNO = ifelse("PLNO" %in% colnames(tmp_alphas), 1, 0),
               TWIL = ifelse("TWIL" %in% colnames(tmp_alphas), 1, 0),
               rainfall = toupper(substr(rain, start = 2, stop = 2)),
               draw = d) ## add treatment column
      
      ## join with original dataframe
      natcomm <- rbind(natcomm, temp)
    }
    
  }
}

write.csv(natcomm, "analyses/interactions_v_traits/structural_coexistence/run_structural/nat_only_structural_results_20240823.csv")
