## Merged BRHO QAQC ##
library(tidyverse)

# Read in Data ####
rm(list=ls())
source("data_cleaning/merge_processing_collections_data.R")

## actually might want to source from the allometry script?


## background data ####
# specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Background-Processing/Background-Processing_entered/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Background-Processing/Background-Processing_entered/"
  
} else {
  # Marina
  lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Processing/Background-Processing/Background-Processing_entered/"
} 


bg_indiv <- read.csv(paste0(lead, "bkgrd-processing_20221102.csv"))



brho  <- all_dat_final %>%
  filter(phyto == "BRHO")

# Filter for Phyto Change Notes ####
## keywords
## die, chang, ->

## create empty data frame
df <- data.frame()

for(i in colnames(brho)[45:48]) {
  tmp <- dplyr::filter(brho, grepl("die", brho[,i]))
  df <- rbind(df, tmp)
}

for(i in colnames(brho)[45:48]) {
  tmp <- dplyr::filter(brho, grepl("chang", brho[,i]))
  df <- rbind(df, tmp)
}

brho$intraphyto <- 0 



# Subset Columns ####
brho_sub <- brho %>%
  select(block, plot, sub, bkgrd, dens, phyto, phyto.unique,phyto.n.indiv, total.biomass.rounded.percap, seed.num.percap, inflor.g.rounded.percap, bkgrd.n.indiv, intraphyto, CRCO, ERBO, FIGA, GAMU, HYGL, SIGA, unique.ID)
  

# Add seeds in data ####


# Add background data ####




