## Pairwise Niche and Fitness Differences
## for 6 species communities

# Set up Env ####
set.seed(10)

## load packages
library(RcppAlgos)

## load posteriors
#source("analyses/interactions_v_traits/random_draws/clean_posteriors.R")
posts_clean = read.csv("posteriors.csv") %>%
  select(-X.1, -X)

## load seed survival & germ data
source("data_cleaning/seed-survival_data-cleaning/seed-survival_data-cleaning.R")
source("data_cleaning/germination_data-cleaning/germination_data-cleaning.R")

## load composition data
comp = read.csv("comp6.csv") %>%
  select(-X)

# Set up Loop ####
## create a vector of the number of posterior draws
posts <- 1:3750

## draw 100 posteriors
draws <- sample(posts, 100, replace = FALSE)

## species vector
all.sp <- unique(posts_clean$species)

## make vector of rainfall treatments
rainfall = c("_c", "_d")

## set index
write_cntr = 1

## Need some variable with all combinations of TACA + other species
TC_pairs = data.frame(OS = all.sp, IS = "TACA")
LM_pairs = data.frame(OS = all.sp, IS = "LOMU")
CS_pairs = data.frame(OS = all.sp, IS = "CESO")
AA_pairs = data.frame(OS = all.sp, IS = "ANAR")
BH_pairs = data.frame(OS = all.sp, IS = "BRHO")
BN_pairs = data.frame(OS = all.sp, IS = "BRNI")

pairs = rbind(TC_pairs, LM_pairs, CS_pairs, AA_pairs, BH_pairs, BN_pairs) %>%
  filter(OS != IS)

## check out TACA - ANAR

# Run Loop ####
## iterate over each possible community composition
for(j in 1:nrow(pairs)){
  
  ## select composition
  cc <- as.character(pairs[j,])
  
  ## iterate over each rainfall treatment
  for(r in 1:length(rainfall)){
    
    ## select rainfall treatment
    rain = rainfall[r]
    
    ## filter & summarise data
    prep <- posts_clean %>%
      filter(species %in% cc) %>%
      select(species, paste0("lambda", rain), 
             paste0("alpha_", tolower(cc[1]), rain), paste0("alpha_", tolower(cc[2]), rain)) %>%
      group_by(species) %>%
      summarise_all(.funs = median) 
    
    rownames(prep) = prep$species
    
    ## filter seed survival & germination data
    seed = surv.sum %>%
      filter(species %in% cc)
    
    germ = mean.germ %>%
      filter(phyto %in% cc)
    
    ## select the matching columns
    tmp_alphas = prep %>%
      ungroup() %>%
      select(paste0("alpha_", tolower(cc[1]), rain), paste0("alpha_", tolower(cc[2]), rain)) %>%
      as.matrix()
    
    rownames(tmp_alphas) = prep$species
    colnames(tmp_alphas) = c(cc[1], cc[2])
    
    sp_i = cc[1]
    sp_j = cc[2]
    
    ## if matrix is complete, calculate pairwise niche & fitness diff metrics
    if (any(is.na(tmp_alphas)) == FALSE) {
      
      ## define alphas
      alpha_ii = tmp_alphas[sp_i, sp_i]
      alpha_ij = tmp_alphas[sp_i, sp_j]
      alpha_jj = round(tmp_alphas[sp_j, sp_j], digits = 3)
      alpha_ji = tmp_alphas[sp_j, sp_i]
      
      ## define lambdas
      lambda_i = as.numeric(prep[sp_i, paste0("lambda", rain)])
      lambda_j = as.numeric(prep[sp_j, paste0("lambda", rain)])
      
      ## seed survival
      s_i = as.numeric(seed[seed$species == sp_i, 2])
      s_j = as.numeric(seed[seed$species == sp_j, 2])
      
      ## germination
      g_i = as.numeric(germ[germ$phyto == sp_i & germ$treatment == toupper(substr(rain, start = 2, stop = 2)), 3])
      g_j = as.numeric(germ[germ$phyto == sp_j & germ$treatment == toupper(substr(rain, start = 2, stop = 2)), 3])
      
      
      ## calculate niche overlap
      n_overlap = sqrt( (alpha_ij*alpha_ji) / (alpha_ii*alpha_jj) )
           ## produces NaN when there is a negative alpha
           ## produced n_overlap > 1 when there is more INTERsp comp than INTRA comp; trouble seems to be from ANAR coeff
      
      ## subtract to get niche differences
      n_diffp = 1 - n_overlap
      
      ## αij describes the per capita effect of species j on species i.
    
      ## sqrt((alpha_ij/alpha_jj) * (alpha_ji/alpha_ii))
      
      ## fitness differences
        ## (nj - 1)/(ni - 1) * sqrt((alpha_ij * alpha_ii) / (alpha_jj * alpha_ji))
        ## nj = lambda_i * g_i / (1 - ((1-g_i)*s_i))
      
      n_i = (lambda_i * g_i) / (1 - ((1 - g_i) * s_i))
      n_j = (lambda_j * g_j) / (1 - ((1 - g_j) * s_j))
      
      f_diffp = (n_j - 1) / (n_i - 1) * sqrt((alpha_ij*alpha_ii) / (alpha_jj * alpha_ji))
  
      
      ## if there are NAs in the alpha matrix, fill all structural metrics with NA
    } else {
      
      n_diffp = NA
      f_diffp = NA
      n_overlap = NA
      
    }
    
    ## create dataframe
    output_row = data.frame(niche_diffp=NA)
    
    ## put back into dataframe
    output_row$niche_diffp = n_diffp
    output_row$fit_diffp = f_diffp
    output_row$n_overlap = n_overlap
    output_row$sp_i = sp_i
    output_row$sp_j = sp_j
    
    ## get comm composition back into dataframe
    output_row = output_row %>%
      mutate(ACAM = ifelse("ACAM" %in% cc, 1, 0),
             AMME = ifelse("AMME" %in% cc, 1, 0),
             ANAR = ifelse("ANAR" %in% cc, 1, 0),
             BRHO = ifelse("BRHO" %in% cc, 1, 0),
             BRNI = ifelse("BRNI" %in% cc, 1, 0),
             CESO = ifelse("CESO" %in% cc, 1, 0),
             GITR = ifelse("GITR" %in% cc, 1, 0),
             LENI = ifelse("LENI" %in% cc, 1, 0),
             LOMU = ifelse("LOMU" %in% cc, 1, 0),
             MAEL = ifelse("MAEL" %in% cc, 1, 0),
             MICA = ifelse("MICA" %in% cc, 1, 0),
             PLER = ifelse("PLER" %in% cc, 1, 0),
             PLNO = ifelse("PLNO" %in% cc, 1, 0),
             TACA = ifelse("TACA" %in% cc, 1, 0),
             THIR = ifelse("THIR" %in% cc, 1, 0),
             TWIL = ifelse("TWIL" %in% cc, 1, 0),
             rainfall = toupper(substr(rain, start = 2, stop = 2)))
    
    print(write_cntr)
    write_cntr=write_cntr+1
    
    write.table(output_row, "data/ndiff_fdiff_pairwise_test_20241116.csv", append = TRUE, row.names = FALSE, sep = ",", col.names = FALSE)
    
  }
}
