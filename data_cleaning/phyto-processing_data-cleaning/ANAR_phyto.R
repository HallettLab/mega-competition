## removed the 5g sample from the allometric relationship as this was a big outlier and made the relatinoship fit worse for all other points
## when calculating seeds out, use the counted flowers for this particular sample

# Load packages ####
library(tidyverse)

# Read in Data ####
## phyto-processing data
## specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/"
  
} else {
  # Marina
  lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/"
} 

anar <- read.csv(paste0(lead, "ANAR_phyto-processing-redo_20221213.csv"))

## basic cleaning function
source("data_cleaning/phyto-processing_data-cleaning/basic_cleaning_function.R")

## allometry data
source("allometry/merge_allometric_relationships.R")

## uniqueID key
source("data_cleaning/unique_key.R")

# Clean Data ####
## Basic cleaning ####
anarC <- basic_cleaning_func(anar)

## need to add in unique.IDs here
anar_int <- left_join(anarC, unique.key, by = c("treatment", "block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique")) %>%
  mutate(unique.ID = unique.ID.y)

## Final mods ####
anar_final <- anar_int %>%
  filter(complete.sample == "Y") %>%
  mutate(census.notes = notes) %>%
  
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, total.biomass.g, flower.num, scale.ID, process.notes, census.notes, unique.ID)
## select only needed columns


# Check Outliers ####
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

## 12-23-18 is showing up twice on the notes df. Not sure why. Oh probably shows up in both for loops
## lots of notes but all thankfully show phyto # changes!! 

# Make Phyto DF ####
anar.phyto <- anar_final %>%
  mutate(ANAR.flowers.out = (allo.df[allo.df$Species == "ANAR",2] + ## intercept
                               (allo.df[allo.df$Species == "ANAR",5]*total.biomass.g)), ## slope
         ## use tot.bio to flower.num to get flowers out
         
         ANAR.flowers.out = ifelse(total.biomass.g > 5, 419, ANAR.flowers.out), ## for the largest sample, put in flower # manually as this didn't work well with the allometric relationship
         
         
         phyto.seed.out = ifelse(treatment == "D",  allo.df[allo.df$Species == "ANAR",13]*ANAR.flowers.out,  allo.df[allo.df$Species == "ANAR",11]*ANAR.flowers.out),
         ## use avg seed num per trt to calculate seeds out
         
         phyto.seed.in = ifelse(!is.na(phyto.unique), phyto.n.indiv, 3),
         ## for phyto uniques, use the # indiv as the seeds.in, otherwise put 3 as the default
         
         phyto.seed.in = ifelse(phyto.n.indiv > 3, phyto.n.indiv, phyto.seed.in)) %>%
  ## then, check for # indiv > 3, use # indiv as seeds.in here also
  select(unique.ID, phyto, phyto.n.indiv, phyto.seed.in, phyto.seed.out)

## still need lots of phyto.seed.in updates likely


ggplot(anar.phyto, aes(x=phyto.seed.out)) +
  geom_histogram()

rm(list = c("anar", "anar_final", "anar_int", "anarC", "df", "tmp"))
