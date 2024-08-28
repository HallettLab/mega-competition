## structural coexistence
## for 4 species communities

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
comp4 <- data.frame(comboGeneral(all.sp, m=4, freqs = 1))

## make vector of rainfall treatments
rainfall = c("_c", "_d")

## create empty dataframe
#allcomm <- data.frame(ACAM= NA, AMME = NA, ANAR = NA, BRHO = NA, BRNI = NA, CESO = NA, GITR = NA, LENI = NA, LOMU = NA, MAEL = NA, MICA=NA, PLER= NA, PLNO = NA, TACA = NA, THIR = NA, TWIL = NA, feasibility=NA, niche_diff = NA, fitness_diff = NA, niche_diff_cpd = NA, omega_all = NA, comm_pair_overlap = NA, comm_pair_diff = NA, rainfall = NA, draw = NA)

comp4 = comp4[1:2,]

## set index
write_cntr = 0

# 4 Sp Calcs ####
## iterate over each possible community composition
for(j in 1:nrow(comp4)){
  
  ## select composition
  cc <- as.character(comp4[j,])
  
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
      prep2 <- prep[c(d, (d + 3750), (d+(2*3750)), (d+(3*3750))),] 
    
      ## rename
      names(prep2) <- c("sp", "lambda", cc[1], cc[2], cc[3], cc[4])

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
      
      write_cntr=write_cntr+1
      
      # Save Output ####
      ## need to do this one row at a time
      ## need to APPEND to the file, not write over
      write.table(output_row, "analyses/interactions_v_traits/structural_coexistence/run_structural/4_sp_structural_results_20240828.csv", append = TRUE, row.names = FALSE, sep = ",", col.names = c("feasibility", "niche_diff", "fitness_diff", "niche_diff_cpd", "omega_all", "comm_pair_overlap", "comm_pair_diff", "ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL", "rainfall", "draw", "iteration_num"))
      
      
  }
  
  }
  
}

