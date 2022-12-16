# Load packages ####
library(tidyverse)

# Read in Data ####
## phyto-processing data
source("data_cleaning/phyto-processing_data-cleaning/basic-cleaning_all-phytos.R")

## allometry data
source("allometry/merge_allometric_relationships.R")

## uniqueID key
source("data_cleaning/unique_key.R")

# Final Cleaning ####
## Check Redo ####
## need to add in unique.IDs here
acam_int <- left_join(acamC, unique.key, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique")) %>%
  mutate(unique.ID = unique.ID.y)

## when I added unique.ID in, an extra row appeared. That's odd. Is there a duplicate combo somewhere?

## Make one ID column and filter for duplicates in this
temp_acam <- acam_int %>%
  mutate(sample.ID = paste(block, plot, sub, bkgrd, dens, phyto, phyto.unique, sep = "_"))

not_unique <- temp_acam %>%
  filter(duplicated(sample.ID))
## 7_34_11_PLNO_L_ACAM_NA
acam_not_unique <- acam_int %>%
  filter(block == 7, plot == 34, sub == 11)
## this one sample has 2 unique IDs attached to it. In October, CW found 2 processing rows associated with this sample, but only 1 physical sample. 

colnames(acam_int)


med_scales <- c("A", "E", "F", "G")  ## scales that need to be rounded

acam_final <- acam_int %>%
  filter(redo.complete == "Y") %>% ## remove incompletes
  
  mutate(total.biomass.g.rounded = ifelse(scale.ID %in% med_scales, round(total.biomass.g, digits = 3), total.biomass.g), 
         redo.total.biomass.g.rounded = ifelse(scale.ID %in% med_scales, round(total.biomass.g, digits = 3), total.biomass.g)) %>% ## round to 3 decimal places
  
  ## need to choose the correct weight from redo.total.biomass and total.biomass.g
  
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, redo.complete, total.biomass.g.rounded, flower.num, scale.ID, process.notes, census.notes, unique.ID) ## select only needed columns


# Check for Outliers ####
## look at inflor.g
ggplot(acam_final, aes(x=total.biomass.g.rounded)) +
  geom_histogram()
## no missing vals
ggplot(acam_final, aes(x=phyto.n.indiv)) +
  geom_histogram()

unique(acam_final$process.notes) ## okay




