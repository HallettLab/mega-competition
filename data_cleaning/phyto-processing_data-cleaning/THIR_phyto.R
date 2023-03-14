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

thir <- read.csv(paste0(lead, "THIR_phyto-processing-redo_20230124.csv"))

## basic cleaning function
source("data_cleaning/phyto-processing_data-cleaning/basic_cleaning_function.R")

## allometry data
source("allometry/merge_allometric_relationships.R")

## uniqueID key
source("data_cleaning/unique_key.R")


# Data Cleaning ####
thirC <- basic_cleaning_func(thir)

## Check Samples ####
#thir_final[thir_final$unique.ID == 9790,]
#thir_final[thir_final$unique.ID == 6337,]
#thir_final[thir_final$unique.ID == 7439,]
#thir_final[thir_final$unique.ID == 9415,]
#thir_final[thir_final$unique.ID == 10498,]
#thir_final[thir_final$unique.ID == 6337,]

## Check Spot Checks ####
#thir_final[thir_final$unique.ID == 1356,] ## good
#thir_final[thir_final$unique.ID == 1877,] ## good
#thir_final[thir_final$unique.ID == 1927,] ## good
#thir_final[thir_final$unique.ID == 5242,] ## good
#thir_final[thir_final$unique.ID == 5443,] ## good
#thir_final[thir_final$unique.ID == 5567,] ## good
#thir_final[thir_final$unique.ID == 5793,] ## good
#thir_final[thir_final$unique.ID == 5968,] ## good
#thir_final[thir_final$unique.ID == 6337,] ## good
#thir_final[thir_final$unique.ID == 8651,] ## good
#thir_final[thir_final$unique.ID == 11589,] ## good

## Check Redo ####
## make sure columns missed by the basic cleaning function are okay!
#str(thirC)
#ggplot(thirC, aes(x=redo.total.biomass.g)) +
 # geom_histogram()
## there's a 17g redo weight? This seems odd...
## there's also several at 10.
## these are not making it through the final modifications

#thirC[!is.na(thirC$redo.total.biomass.g) & thirC$redo.total.biomass.g > 9,]
## okay, they are all incomplete.


## Final Mods ####
med_scales <- c("A", "E", "F", "G")  ## scales that need to be rounded

thir_final <- thirC %>%
  
  ## add unique IDs in
  left_join(unique.key, by = c("treatment", "block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique")) %>% 
  
  #mutate(phyto = ifelse(phyto == "THIR-I", "THIR", phyto)) %>%
  ## get rid of the 'I'
  
  filter(complete.sample == "Y") %>% ## remove incompletes

  mutate(final.total.biomass.g = ifelse(!is.na(redo.total.biomass.g), redo.total.biomass.g, total.biomass.g)) %>%
  ## when a sample was reweighed, use the new value from redo.total.biomass column
  
  mutate(total.biomass.g.rounded = ifelse(scale.ID %in% med_scales, round(final.total.biomass.g, digits = 3), final.total.biomass.g)) %>% ## round to 3 decimal places
  ## make sure to use the final.total.biomass.g as an input
  
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g.rounded, scale.ID, redo.scale.ID, process.notes, census.notes, unique.ID) ## select only needed columns

# Check Data ####
## look at total biomass
ggplot(thir_final, aes(x=total.biomass.g.rounded)) +
  geom_histogram()
## no missing values

## look at phyto.n.indiv
ggplot(thir_final, aes(x=phyto.n.indiv)) +
  geom_histogram()
## some 4 phyto samples

# Check Notes ####
unique(thir_final$process.notes)
# NOTE: 
    ## still need to check samples with TINC and make sure this is added as a weed, NOT used as a phyto.
    ## Done now!



## create empty data frame
df <- data.frame()

## loop through all notes searching for "die" or "chang"
for(i in colnames(thir_final)[14:15]) {
  tmp <- dplyr::filter(thir_final, grepl("die", thir_final[,i]))
  df <- rbind(df, tmp)
}

for(i in colnames(thir_final)[14:15]) {
  tmp <- dplyr::filter(thir_final, grepl("chang", thir_final[,i]))
  df <- rbind(df, tmp)
}
## one phyto change shows up here; changed from 1->2; CW made the change during processing. Otherwise, all good.


# Make Phyto Dataframe ####
thir.phyto <- thir_final %>%
  mutate(THIR.flowers.out = (allo.df[allo.df$Species == "THIR",2] + ## intercept
                               (allo.df[allo.df$Species == "THIR",5]*total.biomass.g.rounded)), ## slope
         ## use tot.bio to flower.num to get flowers out
         
         phyto.seed.out = ifelse(treatment == "D",  allo.df[allo.df$Species == "THIR",17]*THIR.flowers.out,  allo.df[allo.df$Species == "THIR",15]*THIR.flowers.out),
         ## use viability to calculate the total seeds out
         ## viability col is proportion of viable flowers, so multiplying total flowers by the proportion viable should give us the viable flowers which is equivalent to viable seed # as there is always 1 seed/flower.
         
         phyto.seed.in = ifelse(!is.na(phyto.unique), phyto.n.indiv, 3),
         ## for phyto uniques, use the # indiv as the seeds.in, otherwise put 3 as the default
         
         phyto.seed.in = ifelse(phyto.n.indiv > 3, phyto.n.indiv, phyto.seed.in)) %>%
  ## then, check for # indiv > 3, use # indiv as seeds.in here also
  select(unique.ID, phyto, phyto.n.indiv, phyto.seed.in, phyto.seed.out)

# NOTE: ####
    ## there are probably several instances where the phyto collected was a recruit. We should check all collection notes for this possibility.

ggplot(thir.phyto, aes(x=phyto.seed.out)) +
  geom_histogram()


## clean up env
rm(list = c("thir", "thir_final", "thirC", "df", "tmp"))
