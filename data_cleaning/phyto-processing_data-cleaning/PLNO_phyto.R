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

plno <- read.csv(paste0(lead, "PLNO_phyto-processing_20230125.csv")) %>%
  mutate(scale.ID = NA)

## basic cleaning function
source("data_cleaning/phyto-processing_data-cleaning/basic_cleaning_function.R")

## allometry data
source("allometry/merge_allometric_relationships.R")

## uniqueID key
source("data_cleaning/unique_key.R")



# Clean Data ####
plnoC <- basic_cleaning_func(plno)


plno_final <- plnoC %>%
  filter(complete.sample == "Y") %>% ## remove incompletes
  
  left_join(unique.key, by = c("treatment", "block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique")) %>% 
  ## add in unique.ID nums
  
  #mutate(total.biomass.g.rounded = ifelse(scale.ID %in% med_scales, round(total.biomass.g, digits = 3), total.biomass.g)) %>% ## round to 3 decimal places
  ## these were not done on scale A; don't need to round
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, total.biomass.g, scale.ID, process.notes, pheno.notes, collect.notes, unique.ID) 


# Check Data ####
ggplot(plno_final, aes(x=total.biomass.g)) +
  geom_histogram()
## no missing values

ggplot(plno_final, aes(x=phyto.n.indiv)) +
  geom_histogram()


# Check Notes ####
unique(plno_final$process.notes)
## looks good


## create empty data frame
df <- data.frame()

## loop through all notes searching for "die" or "chang"
for(i in colnames(plno_final)[13:15]) {
  tmp <- dplyr::filter(plno_final, grepl("die", plno_final[,i]))
  df <- rbind(df, tmp)
}

for(i in colnames(plno_final)[13:15]) {
  tmp <- dplyr::filter(plno_final, grepl("chang", plno_final[,i]))
  df <- rbind(df, tmp)
}
## nothing here


# Make Phyto DF ####
plno.phyto <- plno_final %>%
  mutate(plno.flowers.out = (allo.df[allo.df$Species == "PLNO",2] + ## intercept
                             (allo.df[allo.df$Species == "PLNO",5]*total.biomass.g) + ## slope
                             (allo.df[allo.df$Species == "PLNO",8]*(total.biomass.g^2))), ## poly
         ## calc seed out from biomass weight & allo relationship
         
         phyto.seed.out = ifelse(treatment == "D",  
                                 allo.df[allo.df$Species == "PLNO",13]*plno.flowers.out,  ## drought seeds
                                 allo.df[allo.df$Species == "PLNO",11]*plno.flowers.out), ## control seeds
         
         phyto.seed.in = ifelse(!is.na(phyto.unique), phyto.n.indiv, 3),
         ## for phyto uniques, use the # indiv as the seeds.in, otherwise put 3 as the default
         
         phyto.seed.in = ifelse(phyto.n.indiv > 3, phyto.n.indiv, phyto.seed.in)) %>%
  ## then, check for # indiv > 3, use # indiv as seeds.in here also
  
  select(unique.ID, phyto, phyto.n.indiv, phyto.seed.in, phyto.seed.out)


## check the seed.in numbers
ggplot(plno.phyto, aes(x=phyto.n.indiv, y=phyto.seed.in)) +
  geom_point()

## clean up env
rm(list = c("plno", "plno_final", "plnoC", "df", "tmp"))