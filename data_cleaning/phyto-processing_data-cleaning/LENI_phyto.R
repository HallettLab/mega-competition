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

leni <- read.csv(paste0(lead, "LENI_phyto-processing-redo_20230315.csv"))

## basic cleaning function
source("data_cleaning/phyto-processing_data-cleaning/basic_cleaning_function.R")

## allometry data
source("allometry/merge_allometric_relationships.R")

## uniqueID key
source("data_cleaning/unique_key.R")

# Clean Data ####
leniC <- basic_cleaning_func(leni)
## leniC loses a few samples that were in VIVI backgrounds (these are filtered out by the basic cleaning function)


leni_final <- leniC %>%
  filter(complete.sample == "Y") %>% ## remove incompletes
  
  left_join(unique.key, by = c("treatment", "block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique")) %>% 
  ## add in unique.ID nums
  
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, pod.num, seed.num, scale.ID, process.notes, census.notes, unique.ID) 



# Check Data ####
## look at total biomass
ggplot(leni_final, aes(x=total.biomass.g)) +
  geom_histogram()
## 1 missing value

## remove non-LENI phyto ####
leni_final[is.na(leni_final$total.biomass.g),] ## ahh ok this was the sample where something other than LENI was collected as the phyto

leni_final <- leni_final %>%
  filter(unique.ID != 518)

## look at phyto.n.indiv
ggplot(leni_final, aes(x=phyto.n.indiv)) +
  geom_histogram()

# Check Notes ####
unique(leni_final$process.notes)
## several phyto number changes occurred at the processing stage
## OK several here with notes that seem like they should be incomplete samples, but since they are still here they probably are still marked complete
## double checked and updated in the processing data file on 3/16. Should be good to go from this point on!

leni.notes <- leni_final %>%
  filter(!is.na(process.notes), process.notes != "WEIGH TOTAL BIOMASS")

## create empty data frame
df <- data.frame()

## loop through all notes searching for "die" or "chang"
for(i in colnames(leni_final)[15:16]) {
  tmp <- dplyr::filter(leni_final, grepl("die", leni_final[,i]))
  df <- rbind(df, tmp)
}

for(i in colnames(leni_final)[15:16]) {
  tmp <- dplyr::filter(leni_final, grepl("chang", leni_final[,i]))
  df <- rbind(df, tmp)
}
## no notes flagged here except 1 processing note that was caught earlier

# Make Phyto DF ####
leni.phyto <- leni_final %>%
  mutate(pods.out = (allo.df[allo.df$Species == "LENI",5]*total.biomass.g), ## slope
         ## calc pods out from biomass weight & allo relationship
         
         phyto.seed.out = pods.out*2,
         
         phyto.seed.in = ifelse(!is.na(phyto.unique), phyto.n.indiv, 3),
         ## for phyto uniques, use the # indiv as the seeds.in, otherwise put 3 as the default
         
         phyto.seed.in = ifelse(phyto.n.indiv > 3, phyto.n.indiv, phyto.seed.in)) %>%
  ## then, check for # indiv > 3, use # indiv as seeds.in here also
  
  select(unique.ID, phyto, phyto.n.indiv, phyto.seed.in, phyto.seed.out)


## clean up env
rm(list = c("leni", "leni_final", "leniC", "leni.notes", "df", "tmp"))
