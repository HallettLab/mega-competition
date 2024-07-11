
set.seed(10)

library(RcppAlgos)

code_loc <- "analyses/interactions_v_traits/structural_coexistence/Saavedra_2017_code/"

## load structural coexistence functions from Saavedra 2017
source(paste0(code_loc, "toolbox_coexistence.R"))
source(paste0(code_loc, "toolbox_figure.R"))

## load posteriors
source("analyses/interactions_v_traits/random_draws/clean_posteriors.R")

# Prep DF ####

## Ideal data format for this... matrix of interaction coeff? from one model run
## need to select the same phyto species and background species 
    ## for instance, GITR, PLER, MICA and alpha_gitr, alpha_pler, alpha_mica

## need an if-else statement to handle NAs

# Set up Loop ####
## create a vector of the number of posterior draws
posts <- 1:3750

## draw 200 posteriors
draws <- sample(posts, 200, replace = FALSE)

## species vector
sp <- unique(posts_clean$species)

## create all possible combinations of composition at a given richness level
comp6 <- data.frame(comboGeneral(sp, m=6, freqs = 1))

## create empty dataframe
allcommC <- data.frame(ACAM = NA, AMME = NA, ANAR = NA, BRHO = NA, CESO=NA, GITR=NA, LOMU= NA, MAEL= NA, MICA= NA, PLER= NA, PLNO= NA, TACA= NA, THIR= NA, TWIL = NA, feasibility=NA, niche_diff = NA, fitness_diff = NA, draw = NA)

## function for N-diff errors ####
n_diff_error_catch <- function(alpha){
  tryCatch(
    
    {## try the n-diff function from Saavedra 2017
      n <- Omega(alpha=alpha)
      return(n)
    },
    #if an error occurs, tell me the error
    error=function(e) {
      message('An Error Occurred')
      print(e)
      
      return(NA)
      
    }
  )
}

# 6 Sp Calcs ####
## iterate over each possible community composition
for(j in 1:nrow(comp6)){
  
  ## create a vector of community composition for each iteration
  cc <- as.character(comp6[j,])
  
  ## filter data
  prep <- posts_clean %>%
    filter(species %in% cc) %>%
    select(species, lambda_c, paste0("alpha_", tolower(cc[1]), "_c"), paste0("alpha_", tolower(cc[2]), "_c"), paste0("alpha_", tolower(cc[3]), "_c"), paste0("alpha_", tolower(cc[4]), "_c"), paste0("alpha_", tolower(cc[5]), "_c"), paste0("alpha_", tolower(cc[6]), "_c"))

## iterate over posterior draws
for (i in 1:length(draws)) {
  
  ## select particular model draw
  d <- draws[i]
  
  ## select one row for every species
  prep2 <- prep[c(d, (d + 3750), (d+(2*3750)), (d+(3*3750)), (d+(4*3750)), (d+(5*3750)), (d+(6*3750))),] 
  
  ## rename
  names(prep2) <- c("sp", "lambda", cc[1], cc[2], cc[3], cc[4], cc[5], cc[6])
  
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
    niche <- n_diff_error_catch(alpha = tmp_alphas)
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
         ANAR = ifelse("ANAR" %in% colnames(tmp_alphas), 1, 0),
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
         draw = d) ## add treatment column

## join with original dataframe
allcommC <- rbind(allcommC, temp)
  
}
}

#allcomm <- allcomm[-1,]

ggplot(allcomm, aes(x=feasibility)) +
  geom_bar()

ggplot(allcomm, aes(x=niche_diff)) +
  geom_density()

## error notes:
## Error in solve.default(t(alpha) %*% alpha) :
  ## system is computationally singular: reciprocal condition number = 1.15444e-16

## d = 1254; i = 133; j = 557

## not sure if this error means it can't be calculated for this particular matrix?
## species in this matrix: ANAR, BRHO, LENI, PLER, TACA, MAEL
