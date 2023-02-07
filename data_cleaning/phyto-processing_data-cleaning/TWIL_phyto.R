# Load packages ####
library(tidyverse)
theme_set(theme_bw())

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

twil <- read.csv(paste0(lead, "TWIL_phyto-processing-redo_20230206.csv"))

## basic cleaning function
source("data_cleaning/phyto-processing_data-cleaning/basic_cleaning_function.R")

## allometry data
source("allometry/merge_allometric_relationships.R")

## uniqueID key
source("data_cleaning/unique_key.R")



# Data Cleaning ####
twilC <- basic_cleaning_func(twil)


## Final Mods ####
med_scales <- c("A", "E", "F", "G")  ## scales that need to be rounded

twil_final <- twilC %>%
  
  ## add unique IDs in
  left_join(unique.key, by = c("treatment", "block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique")) %>% 
  
  mutate(phyto = ifelse(phyto == "TWIL-I", "TWIL", phyto)) %>%
  ## get rid of the 'I'
  
  filter(redo.complete == "Y") %>% ## remove incompletes
  ## important to use redo.complete here as we reassessed and it is likely different than the first time which appeared inconsistent
  
  mutate(final.total.biomass.g = ifelse(!is.na(redo.total.biomass.g), redo.total.biomass.g, total.biomass.g)) %>%
  ## when a sample was reweighed, use the new value from redo.total.biomass column
  
  mutate(total.biomass.g.rounded = ifelse(scale.ID %in% med_scales, round(final.total.biomass.g, digits = 3), final.total.biomass.g)) %>% ## round to 3 decimal places
  ## make sure to use the final.total.biomass.g as an input
  
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, redo.complete, majority.seeds.present, total.biomass.g.rounded, flower.num, scale.ID, redo.scale.ID, redo.notes, process.notes, census.notes, unique.ID) ## select only needed columns



# Check Data ####
## look at total biomass
ggplot(twil_final, aes(x=total.biomass.g.rounded)) +
  geom_histogram()
## no missing values

## look at phyto.n.indiv
ggplot(twil_final, aes(x=phyto.n.indiv)) +
  geom_histogram()

## check flower.num (calc for indiv w/o seeds present)
ggplot(twil_final, aes(x=flower.num)) +
  geom_histogram()

## lots of samples without many seeds, decent amount of vegetative samples also.
ggplot(twil_final, aes(x=majority.seeds.present)) +
  geom_bar()


# Check Notes ####
unique(twil_final$process.notes)
## looks okay

unique(twil_final$redo.notes)
## 0.037 present in one... was this meant to be here?

note_check <- twilC %>%
  filter(redo.notes == "0.037")
## I'm not sure why this is in the notes, but since the majority of seeds are not present and there is a flower count, we can just ignore it.

unique(twil_final$census.notes)
## no notes


# Make Phyto Dataframe ####
twil.phyto <- twil_final %>%
  mutate(TWIL.flowers.out = ifelse(majority.seeds.present == "Y" | majority.seeds.present == "veg", 
                                   allo.df[allo.df$Species == "TWIL",2] + ## intercept
                                     (allo.df[allo.df$Species == "TWIL",5]*total.biomass.g.rounded) + ## slope 
                                   (allo.df[allo.df$Species == "TWIL", 8]*(total.biomass.g.rounded^2)), flower.num),
         ## if most seeds present, use biomass-flower rel. otherwise use flower.num count
         
         #TWIL.flowers.out = ifelse(majority.seeds.present == "veg", ),
         
         
         viable.flowers.out = ifelse(treatment == "D",  allo.df[allo.df$Species == "TWIL",17]*TWIL.flowers.out,  allo.df[allo.df$Species == "TWIL",15]*TWIL.flowers.out),
         ## use viability to calculate the viable flowers out
         ## viability col is proportion of viable flowers, so multiplying total flowers by the proportion viable should give us the viable flowers
         
         phyto.seed.out = ifelse(treatment == "D",  allo.df[allo.df$Species == "TWIL",13]*viable.flowers.out,  allo.df[allo.df$Species == "TWIL",11]*viable.flowers.out),
         
         phyto.seed.in = ifelse(!is.na(phyto.unique), phyto.n.indiv, 3),
         ## for phyto uniques, use the # indiv as the seeds.in, otherwise put 3 as the default
         
         phyto.seed.in = ifelse(phyto.n.indiv > 3, phyto.n.indiv, phyto.seed.in)) %>%
  ## then, check for # indiv > 3, use # indiv as seeds.in here also
  select(unique.ID, phyto, phyto.n.indiv, phyto.seed.in, phyto.seed.out)


ggplot(twil.phyto, aes(x=phyto.seed.out)) +
  geom_histogram()


## clean up env
rm(list = c("twil", "twil_final", "twilC", "med_scales", "note_check"))
