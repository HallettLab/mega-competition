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

amme <- read.csv(paste0(lead, "AMME_phyto-processing_20230131.csv"))
  

## basic cleaning function
source("data_cleaning/phyto-processing_data-cleaning/basic_cleaning_function.R")

## allometry data
source("allometry/merge_allometric_relationships.R")

## uniqueID key
source("data_cleaning/unique_key.R")



# Clean Data ####
ammeC <- basic_cleaning_func(amme)


amme_final <- ammeC %>%
  filter(complete.sample == "Y") %>% ## remove incompletes
  
  left_join(unique.key, by = c("treatment", "block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique")) %>% 
  ## add in unique.ID nums
  
  #mutate(total.biomass.g.rounded = ifelse(scale.ID %in% med_scales, round(total.biomass.g, digits = 3), total.biomass.g)) %>% ## round to 3 decimal places
  ## these were not done on scale A; don't need to round
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, total.biomass.g, scale.ID, process.notes, collect.notes, unique.ID) 


# Check Data ####
ggplot(amme_final, aes(x=total.biomass.g)) +
  geom_histogram()
## no missing values

ggplot(amme_final, aes(x=phyto.n.indiv)) +
  geom_histogram()


# Check Notes ####
unique(amme_final$process.notes)
## looks good


## create empty data frame
df <- data.frame()

## loop through all notes searching for "die" or "chang"
for(i in colnames(amme_final)[13:14]) {
  tmp <- dplyr::filter(amme_final, grepl("die", amme_final[,i]))
  df <- rbind(df, tmp)
}

for(i in colnames(amme_final)[13:14]) {
  tmp <- dplyr::filter(amme_final, grepl("chang", amme_final[,i]))
  df <- rbind(df, tmp)
}
## all good


# Make Phyto DF ####