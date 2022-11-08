## Merge allometric relationships

## the purpose of this script is to put all allometric relationships in one dataframe so that they can be easily sourced and used in prepping the phyto & background data for modeling. 

## Inputs
    ## one model output object from each allometry_testing script. 
    ## input should be named like: "BRHO.allo.output"

## Relevant Outputs
    ## allo.df -> one df that will contain the species name, the model intercept, the slope (coeff column) & coeff2 if it is a polynomial model


# Read in Data ####
## read in allometric relationships
source("allometry/gitr_allometry_testing.R")
source("allometry/brho_allometry_testing.R")
source("allometry/pler_allometry_testing.R")

# Modify Here w/New Sp ####
## make a vector of all completed relationship names
allo.outputs <- c("GITR.allo.output", "BRHO.allo.output", "PLER.allo.output")

# Merge Model Outputs ####
allo.df <- as.data.frame(matrix(ncol = 4))
colnames(allo.df) <- c("species", "intercept", "coeff", "coeff2")

for (i in 1:length(allo.outputs)) {
  
  sp <- get(allo.outputs[i]) ## fetch the correct model
  
  temp <- data.frame(species = NA, intercept = NA, coeff = NA, coeff2 = NA) ## set colnames in temp dataframe
  
  temp$species <- substr(allo.outputs[i], 1, 4) ## get sp name from first 4 letters of the model output object name
  
  ## add in model outputs
  temp$intercept <- sp[1] ## intercept
  temp$coeff <- sp[2] ## slope
  temp <- temp %>%
    mutate(coeff2 = ifelse(!is.na(sp[3]), sp[3], 0)) ## put in a 0 rather than NA if there is no 2nd model coeff
  
  ## append
  allo.df <- rbind(allo.df, temp)
  
}


allo.df <- allo.df %>%
  filter(!is.na(species))