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

## Fix Subplots ####
## these were changed during processing and this messed up joining with unique.IDs
ammeC[ammeC$block == 7 & ammeC$plot == 28 & ammeC$sub == 18,]$sub <- 17
ammeC[ammeC$block == 16 & ammeC$plot == 42 & ammeC$sub == 22,]$sub <- 19

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

ggplot(amme_final, aes(x=unique.ID)) +
  geom_histogram()
## 2 missing unique.IDs
    ## fixed this now.

#amme.na.check <- amme_final %>%
 # filter(is.na(unique.ID))
## subplots were changed and this is why the unique.IDs are missing



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
amme.phyto <- amme_final %>%
  mutate(amme.flowers.out = (allo.df[allo.df$Species == "AMME",5]*total.biomass.g), ## slope
         ## calc seed out from biomass weight & allo relationship
         
         phyto.seed.out = ifelse(treatment == "D",  
                                 allo.df[allo.df$Species == "AMME",10]*amme.flowers.out,  ## drought seeds
                                 allo.df[allo.df$Species == "AMME",8]*amme.flowers.out), ## control seeds
         
         phyto.seed.in = ifelse(!is.na(phyto.unique), phyto.n.indiv, 3),
         ## for phyto uniques, use the # indiv as the seeds.in, otherwise put 3 as the default
         
         phyto.seed.in = ifelse(phyto.n.indiv > 3, phyto.n.indiv, phyto.seed.in)) %>%
  ## then, check for # indiv > 3, use # indiv as seeds.in here also
  
  select(unique.ID, treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.seed.in, phyto.seed.out)


## check the seed.in numbers
ggplot(amme.phyto, aes(x=phyto.n.indiv, y=phyto.seed.in)) +
  geom_point()

## clean up env
rm(list = c("amme", "amme_final", "ammeC", "df", "tmp"))
