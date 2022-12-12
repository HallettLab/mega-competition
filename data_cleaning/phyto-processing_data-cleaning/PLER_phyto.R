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
## need to add in unique.IDs here
pler_int <- left_join(plerC, unique.key, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique")) %>%
  mutate(unique.ID = unique.ID.y)


med_scales <- c("A", "E", "F", "G")  ## scales that need to be rounded

pler_final <- pler_int %>%
 # filter(complete.sample == "Y") %>% ## remove incompletes
  
  mutate(inflor.g.rounded = ifelse(scale.ID %in% med_scales, round(inflor.g, digits = 3), inflor.g)) %>% ## round to 3 decimal places
  
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, inflor.g.rounded, empty.flower.num, flower.num, seed.num, new.flower.num, scale.ID, process.notes, census.notes, unique.ID) ## select only needed columns



## check for outliers
ggplot(pler_final[pler_final$complete.sample == "Y",], aes(x=inflor.g.rounded)) +
  geom_histogram()

## check phyto.n.indiv
ggplot(pler_final, aes(x=phyto.n.indiv)) +
  geom_histogram()

## check for any samples that do not have a new.flower.num
ggplot(pler_final[pler_final$complete.sample == "N",], aes(x=seed.num/2, y=new.flower.num)) +
  geom_point() +
  geom_abline(slope = 1)
  ## 3 rows had missing values

# Check Notes ####
## this might eventually be best in the census script?
unique(pler_final$process.notes)
  ## some phyto changes here that need to be double checked
  ## a few samples will need to be removed - when parts of the inflorescence are missing

pler.notes <- pler_final %>%
  filter(!is.na(process.notes), process.notes!= "WEIGH TOTAL BIOMASS", process.notes != "Sample missing")

## unique.ID 9667 & 8432 need to be removed, missing parts of inflor



## create empty data frame
df <- data.frame()

## loop through all notes searching for "die" or "chang"
for(i in colnames(pler_final)[17:18]) {
  tmp <- dplyr::filter(pler_final, grepl("die", pler_final[,i]))
  df <- rbind(df, tmp)
}

for(i in colnames(pler_final)[17:18]) {
  tmp <- dplyr::filter(pler_final, grepl("chang", pler_final[,i]))
  df <- rbind(df, tmp)
}


