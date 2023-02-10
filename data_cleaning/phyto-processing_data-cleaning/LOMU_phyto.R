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

lomu <- read.csv(paste0(lead, "LOMU_phyto-processing-redo_20230206.csv")) 

## basic cleaning function
source("data_cleaning/phyto-processing_data-cleaning/basic_cleaning_function.R")

## allometry data
source("allometry/merge_allometric_relationships.R")

## uniqueID key
source("data_cleaning/unique_key.R")

# Data Cleaning ####
## Check Redo ####
#colnames(lomu)
#str(lomu)
#unique(lomu$redo.total.biomass) ## someone wrote missing in this column

redone <- lomu %>%
  filter(redo.total.biomass != "")

#lomu.redo <- lomu %>%
 # filter(redo.total.biomass != "", redo.total.biomass != "missing")

#lomu.redo$redo.total.biomass <- as.numeric(lomu.redo$redo.total.biomass)
  
#lomu.redo <- lomu.redo %>%
 # mutate(redo.diff = redo.total.biomass - total.biomass.g)

#ggplot(lomu.redo, aes(x=redo.diff)) +
 # geom_histogram() +
  #geom_vline(xintercept = 0)

#concerns <- lomu.redo %>%
 # filter(redo.diff > 0)

#MWblocks <- c(6, 7, 12, 14, 15)

#lomu.needs.redo <- lomu %>%
 # filter(block %in% MWblocks, redo.total.biomass == "")

## Final Cleaning
lomuC <- basic_cleaning_func(lomu)

## someone wrote "missing in the redo.total.biomass column
lomuC[lomuC$block == 14 & lomuC$plot == 21 & lomuC$sub == 2, ]$redo.total.biomass <- NA
lomuC[lomuC$block == 14 & lomuC$plot == 21 & lomuC$sub == 2, ]$redo.notes <- "sample missing"

lomuC$redo.total.biomass <- as.numeric(lomuC$redo.total.biomass)

## Check Spot Check ####
## made all updates to the raw data sheet, that's why all of these are commented out now (CW 2/9/23)
#lomuC[lomuC$unique == 7778,] ## good
#lomuC[lomuC$unique == 2604,]$redo.total.biomass <- 0.503
#lomuC[lomuC$unique == 11971,]$redo.total.biomass <- 0.454
#lomuC[lomuC$unique == 2429,]$redo.total.biomass <- 0.500
#lomuC[lomuC$unique == 7478,] ## good
#lomuC[lomuC$unique == 8863,] ## good
#lomuC[lomuC$unique == 11967,]$redo.total.biomass <- 0.840 ## seems like it could have lost bio b/w spot check and redo, so using the spot check value here.
#lomuC[lomuC$unique == 4335,]$redo.total.biomass <- 2.537 ## seems like it could have lost bio b/w spot check and redo, so using the spot check value here.
#lomuC[lomuC$unique == 8104,] ## good
#lomuC[lomuC$unique == 8229,] ## good
lomuC[lomuC$unique == 1016,] ## off by a lot, no notes though; look into this further
#lomuC[lomuC$unique == 8388,]$redo.total.biomass <- 1.248 ## seems like it could have lost bio b/w spot check and redo, so using the spot check value here.
#lomuC[lomuC$unique == 9038,] ## good
#lomuC[lomuC$unique == 5086,] ## good
#lomuC[lomuC$unique == 8838,] ## missing in both places.

## Final Mods ####
#med_scales <- c("A", "E", "F", "G")  ## scales that need to be rounded

lomu_final <- lomuC %>%
  
  ## add unique IDs in
  left_join(unique.key, by = c("treatment", "block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique")) %>% 
  
  filter(complete.sample == "Y") %>% ## remove incompletes
  
  mutate(final.total.biomass.g = ifelse(!is.na(redo.total.biomass), redo.total.biomass, total.biomass.g)) %>%
  ## when a sample was reweighed, use the new value from redo.total.biomass column
  
  #mutate(total.biomass.g.rounded = ifelse(scale.ID %in% med_scales, round(final.total.biomass.g, digits = 3), final.total.biomass.g)) %>% ## round to 3 decimal places
  ## make sure to use the final.total.biomass.g as an input
  ## don't need to round, no scale A here
  
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, final.total.biomass.g, scale.ID, redo.scale.ID, process.notes, census.notes, redo.notes, unique.ID) ## select only needed columns



# Check Data ####
## look at total biomass
ggplot(lomu_final, aes(x=final.total.biomass.g)) +
  geom_histogram()
## no missing values

## look at phyto.n.indiv
ggplot(lomu_final, aes(x=phyto.n.indiv)) +
  geom_histogram()
## some 4 phyto samples

# Check Notes ####
unique(lomu_final$process.notes)
## all seem good. There are likely phyto # changes that have not carried over to the phyto collections data sheet and will need to be made there.

unique(lomu_final$redo.notes)
## "2 copies of this sample, pulled aside"                                           
## "2 copies of this sample, pulled aside, removed the roots from the small envelope"
## "sample missing"

## create empty data frame
df <- data.frame()

## loop through all notes searching for "die" or "chang"
for(i in colnames(lomu_final)[14:16]) {
  tmp <- dplyr::filter(lomu_final, grepl("die", lomu_final[,i]))
  df <- rbind(df, tmp)
}

for(i in colnames(lomu_final)[14:16]) {
  tmp <- dplyr::filter(lomu_final, grepl("chang", lomu_final[,i]))
  df <- rbind(df, tmp)
}

# NOTE ####
## LOTS of change notes to follow up on. phew! look thru later.

# Make Phyto Dataframe ####
lomu.phyto <- lomu_final %>%
  mutate(phyto.seed.out = (allo.df[allo.df$Species == "LOMU",2] + ## intercept
                             (allo.df[allo.df$Species == "LOMU",5]*final.total.biomass.g) + ## slope
                             (allo.df[allo.df$Species == "LOMU",8]*(final.total.biomass.g^2))), ## poly
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
rm(list = c("lomu", "lomuC", "df", "tmp", "redone"))
