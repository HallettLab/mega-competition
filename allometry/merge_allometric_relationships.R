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
source("allometry/mael_allometry_testing.R")
source("allometry/acam_allometry_testing.R")
source("allometry/anar_allometry_testing.R")
source("allometry/ceso_allometry_testing.R")

## merge all together
allo.df <- do.call("rbind", list(ACAM.allo.output, ANAR.allo.output, BRHO.allo.output, CESO.allo.output, GITR.allo.output, MAEL.allo.output, PLER.allo.output)) %>%
  mutate(poly = ifelse(is.na(poly), 0, poly)) ## change NAs to 0s, will work better in later calculations.


## clean up env
rm(list = c("ACAM.allo.output", "ANAR.allo.output", "BRHO.allo.output", "CESO.allo.output", "GITR.allo.output", "MAEL.allo.output", "PLER.allo.output"))
