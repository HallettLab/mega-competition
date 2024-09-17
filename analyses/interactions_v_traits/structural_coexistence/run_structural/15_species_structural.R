## structural coexistence
## for 16 species community

# Set up Env ####
set.seed(10)

library(RcppAlgos)

code_loc <- "analyses/interactions_v_traits/structural_coexistence/Saavedra_2017_code/"

## load structural coexistence functions from Saavedra 2017
source(paste0(code_loc, "toolbox_coexistence.R"))

## load posteriors
source("analyses/interactions_v_traits/random_draws/clean_posteriors.R")

# Set up Loop ####
## create a vector of the number of posterior draws
posts <- 1:3750

## draw 100 posteriors
draws <- sample(posts, 100, replace = FALSE)

## species vector
all.sp <- unique(posts_clean$species) 

## create all possible combinations of composition at a given richness level
comp15 <- data.frame(comboGeneral(all.sp, m=15, freqs = 1))

## make vector of rainfall treatments
rainfall = c("_c", "_d")

## set index
write_cntr = 0

# 15 Sp Calcs ####
## iterate over each possible community composition
for(j in 1:nrow(comp15)){
  
  ## select composition
  cc <- as.character(comp15[j,])
  
  ## iterate over each rainfall treatment
  for(r in 1:length(rainfall)){
    
    ## select rainfall treatment
    rain = rainfall[r]
    
    ## filter data
    prep <- posts_clean %>%
      filter(species %in% cc) %>%
      select(species, lambda_c, paste0("alpha_", tolower(cc[1]), rain), paste0("alpha_", tolower(cc[2]), rain), paste0("alpha_", tolower(cc[3]), rain), paste0("alpha_", tolower(cc[4]), rain), paste0("alpha_", tolower(cc[5]), rain), paste0("alpha_", tolower(cc[6]), rain), paste0("alpha_", tolower(cc[7]), rain), paste0("alpha_", tolower(cc[8]), rain), paste0("alpha_", tolower(cc[9]), rain), paste0("alpha_", tolower(cc[10]), rain), paste0("alpha_", tolower(cc[11]), rain), paste0("alpha_", tolower(cc[12]), rain), paste0("alpha_", tolower(cc[13]), rain), paste0("alpha_", tolower(cc[14]), rain), paste0("alpha_", tolower(cc[15]), rain))
    
    ## iterate over posterior draws
    for (i in 1:length(draws)) {
      
      ## select a model draw
      d <- draws[i]
      
      ## select one row for every species
      prep2 <- prep[c(d, (d + 3750), (d+(2*3750)), (d+(3*3750)), (d+(4*3750)), (d+(5*3750)), (d+(6*3750)), (d+(7*3750)), (d+(8*3750)), (d+(9*3750)), (d+(10*3750)), (d+(11*3750)), (d+(12*3750)), (d+(13*3750)), (d+(14*3750))),] 
      
      ## rename
      names(prep2) <- c("sp", "lambda", cc[1], cc[2], cc[3], cc[4], cc[5], cc[6], cc[7], cc[8], cc[9], cc[10], cc[11], cc[12], cc[13], cc[14], cc[15])
      
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
      output_row <- data.frame(feasibility=NA)
      
      ## put back into dataframe
      output_row$feasibility <- f
      output_row$niche_diff <- niche
      output_row$fitness_diff <- fitness
      
      output_row$niche_diff_cpd = niche_cpd
      output_row$omega_all = omega_all
      output_row$comm_pair_overlap = overlap_cpd
      output_row$comm_pair_diff = niche_cpd-omega_all
      
      ## get comm composition back into dataframe
      output_row <- output_row %>%
        mutate(ACAM = ifelse("ACAM" %in% colnames(tmp_alphas), 1, 0),
               AMME = ifelse("AMME" %in% colnames(tmp_alphas), 1, 0),
               ANAR = ifelse("ANAR" %in% colnames(tmp_alphas), 1, 0),
               BRHO = ifelse("BRHO" %in% colnames(tmp_alphas), 1, 0),
               BRNI = ifelse("BRNI" %in% colnames(tmp_alphas), 1, 0),
               CESO = ifelse("CESO" %in% colnames(tmp_alphas), 1, 0),
               GITR = ifelse("GITR" %in% colnames(tmp_alphas), 1, 0),
               LENI = ifelse("LENI" %in% colnames(tmp_alphas), 1, 0),
               LOMU = ifelse("LOMU" %in% colnames(tmp_alphas), 1, 0),
               MAEL = ifelse("MAEL" %in% colnames(tmp_alphas), 1, 0),
               MICA = ifelse("MICA" %in% colnames(tmp_alphas), 1, 0),
               PLER = ifelse("PLER" %in% colnames(tmp_alphas), 1, 0),
               PLNO = ifelse("PLNO" %in% colnames(tmp_alphas), 1, 0),
               TACA = ifelse("TACA" %in% colnames(tmp_alphas), 1, 0),
               THIR = ifelse("THIR" %in% colnames(tmp_alphas), 1, 0),
               TWIL = ifelse("TWIL" %in% colnames(tmp_alphas), 1, 0),
               rainfall = toupper(substr(rain, start = 2, stop = 2)),
               draw = d,
               iteration_num = write_cntr) ## add treatment column
      
      print(write_cntr)
      
      write_cntr=write_cntr+1
      
      # Save Output ####
      ## need to do this one row at a time
      ## need to APPEND to the file, not write over
      write.table(output_row, "analyses/interactions_v_traits/structural_coexistence/run_structural/structural_results_files/15_sp_structural_results_20240917.csv", append = TRUE, row.names = FALSE, sep = ",", col.names = FALSE)
      
      
    }
    
  }
  
}

