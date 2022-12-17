## removed the 5g sample from the allometric relationship as this was a big outlier and made the relatinoship fit worse for all other points
## when calculating seeds out, use the counted flowers for this particular sample

# Load packages ####
library(tidyverse)

# Read in Data ####
## phyto-processing data
source("data_cleaning/phyto-processing_data-cleaning/basic-cleaning_all-phytos.R")

## allometry data
source("allometry/merge_allometric_relationships.R")

## uniqueID key
source("data_cleaning/unique_key.R")


## need to add in unique.IDs here
anar_int <- left_join(anarC, unique.key, by = c("treatment", "block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique")) %>%
  mutate(unique.ID = unique.ID.y)

anar_final <- anar_int %>%
  filter(complete.sample == "Y") %>%
  mutate(census.notes = notes) %>%
  
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, total.biomass.g, flower.num, scale.ID, process.notes, census.notes, unique.ID)
## select only needed columns


## Check 
ggplot(anar_final, aes(x=total.biomass.g)) +
  geom_histogram()

ggplot(anar_final, aes(x=phyto.n.indiv)) +
  geom_histogram()

unique(anar_final$process.notes)
## seems okay


## create empty data frame
df <- data.frame()

## loop through all notes searching for "die" or "chang"
for(i in colnames(anar_final)[13:14]) {
  tmp <- dplyr::filter(anar_final, grepl("die", anar_final[,i]))
  df <- rbind(df, tmp)
}

for(i in colnames(anar_final)[13:14]) {
  tmp <- dplyr::filter(anar_final, grepl("chang", anar_final[,i]))
  df <- rbind(df, tmp)
}
unique(df$census.notes)

## 12-23-18 is showing up twice on the notes df. Not sure why.
## lots of notes but all thankfully show phyto # changes!! 
