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

clpu <- read.csv(paste0(lead, "CLPU_phyto-processing_20230206.csv"))

## basic cleaning function
source("data_cleaning/phyto-processing_data-cleaning/basic_cleaning_function.R")

## allometry data
source("allometry/merge_allometric_relationships.R")

## uniqueID key
source("data_cleaning/unique_key.R")


# Data Cleaning ####
clpuC <- basic_cleaning_func(clpu)


## Final Mods ####
clpu_final <- clpuC %>%
  
  ## add unique IDs in
  left_join(unique.key, by = c("treatment", "block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique")) %>% 
  
  filter(complete.sample == "Y") %>% ## remove incompletes
  
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, scale.ID, process.notes, census.notes, unique.ID) ## select only needed columns



# Check Data ####
## look at total biomass
ggplot(clpu_final, aes(x=total.biomass.g)) +
  geom_histogram()
## no missing values

## look at phyto.n.indiv
ggplot(clpu_final, aes(x=phyto.n.indiv)) +
  geom_histogram()
## some 4 phyto samples

# Check Notes ####
unique(clpu_final$process.notes)
## all seem good. There are likely phyto # changes that have not carried over to the phyto collections data sheet and will need to be made there.


## create empty data frame
df <- data.frame()

## loop through all notes searching for "die" or "chang"
for(i in colnames(lomu_final)[14:15]) {
  tmp <- dplyr::filter(lomu_final, grepl("die", lomu_final[,i]))
  df <- rbind(df, tmp)
}

for(i in colnames(lomu_final)[14:15]) {
  tmp <- dplyr::filter(lomu_final, grepl("chang", lomu_final[,i]))
  df <- rbind(df, tmp)
}



# Make Phyto Dataframe ####
lomu.phyto <- lomu_final %>%
  mutate(phyto.seed.out = (allo.df[allo.df$Species == "LOMU",2] + ## intercept
                             (allo.df[allo.df$Species == "LOMU",5]*total.biomass.g.rounded) ## slope
                           (allo.df[allo.df$Species == "LOMU",8]*(total.biomass.g.rounded^2))), ## poly
         ## use tot.bio to seed.num
         
         phyto.seed.in = ifelse(!is.na(phyto.unique), phyto.n.indiv, 3),
         ## for phyto uniques, use the # indiv as the seeds.in, otherwise put 3 as the default
         
         phyto.seed.in = ifelse(phyto.n.indiv > 3, phyto.n.indiv, phyto.seed.in)) %>%
  ## then, check for # indiv > 3, use # indiv as seeds.in here also
  select(unique.ID, phyto, phyto.n.indiv, phyto.seed.in, phyto.seed.out)

# NOTE: ####
## there are probably several instances where the phyto collected was a recruit. We should check all collection notes for this possibility.

ggplot(lomu.phyto, aes(x=phyto.seed.out)) +
  geom_histogram()


## clean up env
rm(list = c("lomu", "lomu_final", "lomuC", "df", "tmp"))
