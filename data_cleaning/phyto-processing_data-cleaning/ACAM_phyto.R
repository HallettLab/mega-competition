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
acam_int <- left_join(acamC, unique.key, by = c("treatment", "block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique")) %>%
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
  
  select(-complete.sample) %>% ## this is old data and best to remove BEFORE accidentally using it.
  
  filter(redo.complete == "Y") %>% ## remove incompletes
  ## make sure to use the redo.complete column, this one is most accurate
  
  mutate(final.total.biomass.g = ifelse(!is.na(redo.total.biomass), redo.total.biomass, total.biomass.g)) %>%
  ## when a sample was reweighed due to roots, use the new value from redo.total.biomass column
  
  mutate(total.biomass.g.rounded = ifelse(scale.ID %in% med_scales, round(final.total.biomass.g, digits = 3), final.total.biomass.g)) %>% ## round to 3 decimal places
  ## make sure to use the final.total.biomass.g as an input
  
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, redo.complete, total.biomass.g.rounded, flower.num, scale.ID, redo.notes, completion.notes, process.notes, census.notes, unique.ID) ## select only needed columns


# Check for Outliers ####
## look at inflor.g
ggplot(acam_final, aes(x=total.biomass.g.rounded)) +
  geom_histogram()
## no missing vals
ggplot(acam_final, aes(x=phyto.n.indiv)) +
  geom_histogram()

unique(acam_final$process.notes)
## probably not the most up to date column. Ignore all roots present notes. Redo.notes will be better for this.
## no current issues from these notes.

unique(acam_final$redo.notes)
## these all look good, notes were recorded when a sample was reweighed

## create empty data frame
df <- data.frame()

## loop through all notes searching for "die" or "chang"
for(i in colnames(acam_final)[14:17]) {
  tmp <- dplyr::filter(acam_final, grepl("die", acam_final[,i]))
  df <- rbind(df, tmp)
}

for(i in colnames(acam_final)[14:17]) {
  tmp <- dplyr::filter(acam_final, grepl("chang", acam_final[,i]))
  df <- rbind(df, tmp)
}
## no notes flagged! nice!

# Make Phyto DF ####
acam.phyto <- acam_final %>%
  mutate(ACAM.flowers.out = (allo.df[allo.df$Species == "ACAM",2] + ## intercept
                               (allo.df[allo.df$Species == "ACAM",5]*total.biomass.g.rounded)), ## slope
         ## use tot.bio to flower.num to get flowers out
         
         phyto.seed.out = ifelse(treatment == "D",  allo.df[allo.df$Species == "ACAM",13]*ACAM.flowers.out,  allo.df[allo.df$Species == "ACAM",11]*ACAM.flowers.out),
         ## use avg seed num per trt to calculate seeds out
         
         phyto.seed.in = ifelse(!is.na(phyto.unique), phyto.n.indiv, 3),
         ## for phyto uniques, use the # indiv as the seeds.in, otherwise put 3 as the default
         
         phyto.seed.in = ifelse(phyto.n.indiv > 3, phyto.n.indiv, phyto.seed.in)) %>%
  ## then, check for # indiv > 3, use # indiv as seeds.in here also
  select(unique.ID, phyto, phyto.n.indiv, phyto.seed.in, phyto.seed.out)

ggplot(acam.phyto, aes(x=phyto.seed.out)) +
  geom_histogram()


## clean up env
rm(list = c("acam_final", "acam_int", "acam_not_unique", "acamC"))
