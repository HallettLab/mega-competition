## network metrics
## for 4 species communities

# Set up Env ####
set.seed(10)

library(RcppAlgos)

## load posteriors
source("analyses/interactions_v_traits/random_draws/clean_posteriors.R")

## load network metric functions

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
      select(species, #lambda_c, 
             paste0("alpha_", tolower(cc[1]), rain), paste0("alpha_", tolower(cc[2]), rain), paste0("alpha_", tolower(cc[3]), rain), paste0("alpha_", tolower(cc[4]), rain))
    
    ## iterate over posterior draws
    for (i in 1:length(draws)) {
      
      ## select a model draw
      d <- draws[i]
      
      ## select one row for every species
      prep2 <- prep[c(d, (d + 3750), (d+(2*3750)), (d+(3*3750))),] 
      
      ## rename
      names(prep2) <- c("sp", #"lambda", 
                        cc[1], cc[2], cc[3], cc[4])
      
      ## select the matching columns
      tmp_alphas = prep2 %>%
        ungroup() %>%
        select(all_of(cc)) %>%
        as.matrix()
      
      ## set rownames
      rownames(tmp_alphas) = prep2$sp
      
      ## if matrix is complete, calculate network metrics
      if (any(is.na(tmp_alphas)) == FALSE) {
        
        ## calculate diagonal dominance
        dom = dominance(tmp_alphas)
        
        ## calculate competitive asymmetry
        asym = asymmetry(tmp_alphas, sp1 = cc[1], sp2 = cc[2], sp3 = cc[3], sp4 = cc[4], n=4)
        
        sk = skew(tmp_alphas)
  
        ## if there are NAs in the alpha matrix, fill all structural metrics with NA
      } else {
        
        dom = NA
        asym = NA
        sk = NA
        
      }
      
      ## create dataframe
      output_row = data.frame(dominance=NA)
      
      ## put back into dataframe
      output_row$dominance = dom
      output_row$asymmetry = asym
      output_row$skewness = sk
      
      ## get comm composition back into dataframe
      output_row = output_row %>%
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
      write.table(output_row, "analyses/interactions_v_traits/structural_coexistence/run_structural/4_sp_network_metrics_20240905.csv", append = TRUE, row.names = FALSE, sep = ",", col.names = FALSE)
      
      
    }
    
  }
  
}

