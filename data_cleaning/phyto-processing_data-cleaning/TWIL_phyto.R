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

## Check Redo Cols ####
## these columns would not be error checked by the basic cleaning function - follow up and make sure that all looks okay here.
#str(twilC)
#unique(twilC$majority.seeds.present) ## looks good
#twilC[is.na(twilC$majority.seeds.present),]
## 3 samples are missing, but there are 2 others that are unclear.
    ## missing: 10102,  968, and one that doesn't have a unique.ID sample 6-5-8 (odd) - okay after looking into it the unique.ID is 4276. It must have just gotten deleted? Joining it with the unique key fixes this issue.
    ## samples 5365 and 5991 were just marked incomplete and majority.seeds.present wasn't filled out
    ## these should all get excluded when we filter for redo.complete == "Y

#unique(twilC$redo.complete)
## one value was entered as missing here
#twilC[is.na(twilC$redo.complete),]
## 2 missing samples show up here.

## Follow up- Collections Notes ####
#twilC[twilC$unique == 6091,]
#twilC[twilC$unique == 6166,] ## this is a veg sample that was collected too early by mistake. Would have gone on to produce flowers likely. 
#twilC[twilC$unique == 6402,] 
#twilC[twilC$unique == 7103,] 
#twilC[twilC$unique == 8347,] ## this sample may have been double planted with 6 TWIL rather than 3. 
    ## since the actual phyto # is 2, leave seeds.in as is (3)
#twilC[twilC$unique == 8547,] ## these phytometers might be spillover from adjacent TWIL background
    ## need to adjust this seeds.in value to # phyto.n.indiv (2)
    ## change made below 2/9/23


## Final Mods ####
med_scales <- c("A", "E", "F", "G")  ## scales that need to be rounded

twil_final <- twilC %>%
  
  ## add unique IDs in
  left_join(unique.key, by = c("treatment", "block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique")) %>% 
  
  #mutate(phyto = ifelse(phyto == "TWIL-I", "TWIL", phyto)) %>%
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
## 97 rows missing values - so there should be at least 97 samples with seeds present

nrow(twil_final[twil_final$majority.seeds.present == "Y",]) ## 100
nrow(twil_final[twil_final$majority.seeds.present == "N",]) ## 63
nrow(twil_final[twil_final$majority.seeds.present == "veg",]) ## 12


## lots of samples without many seeds, decent amount of vegetative samples also.
ggplot(twil_final, aes(x=majority.seeds.present)) +
  geom_bar()

ggplot(twil_final[twil_final$majority.seeds.present == "N",], aes(x=flower.num)) +
  geom_histogram()
## no missing values, meaning everything that didn't have the majority of seeds had flowers counted. Good.

ggplot(twil_final[twil_final$majority.seeds.present == "Y",], aes(x=flower.num)) +
  geom_histogram()
## several with seeds present had flower.num counted, but that's okay

ggplot(twil_final[twil_final$majority.seeds.present == "veg",], aes(x=flower.num)) +
  geom_histogram()
ggplot(twil_final[twil_final$majority.seeds.present == "veg",], aes(x=total.biomass.g.rounded)) +
  geom_histogram()
## no missing values, everything veg has a weight at least. that's good.
ggplot(twil_final, aes(x=majority.seeds.present)) +
  geom_bar()

## Make Changes ####
## on double check, this sample was vegetative - didn't have seeds
twil_final[twil_final$unique == 6091,]$majority.seeds.present <- "veg"

# Check Notes ####
unique(twil_final$process.notes)
## looks okay

unique(twil_final$redo.notes)
## 0.037 present in one... was this meant to be here?
## a few of the notes were about reweighing because the orig biomass did not reflect sample size. I believe that in the original data, some of the block 14 plots (20's - 30's) got scrambled. The redo weights should have fixed this problem, CW went back and reweighed a few more on 2/8/2023 and feel good about things now.

note_check <- twilC %>%
  filter(redo.notes == "0.037")
## I'm not sure why this is in the notes, but since the majority of seeds are not present and there is a flower count, we can just ignore it.

unique(twil_final$census.notes)
## no notes


# Make Phyto Dataframe ####
twil.phyto <- twil_final %>%
  mutate(TWIL.flowers.out = ifelse(majority.seeds.present == "Y" | majority.seeds.present == "veg", 
                                   allo.df[allo.df$Species == "TWIL",5]*total.biomass.g.rounded, ## slope
                                   flower.num),
         ## if most seeds present, use biomass-flower rel. otherwise use flower.num count; need to make a decision still on how to treat vegetative samples, currently we are treating them as if they would potentially produce seeds at some point.
         
         
         viable.flowers.out = ifelse(treatment == "D",  
                                     allo.df[allo.df$Species == "TWIL",14]*TWIL.flowers.out,  
                                     allo.df[allo.df$Species == "TWIL",12]*TWIL.flowers.out),
         ## use viability to calculate the viable flowers out
         ## viability col is proportion of viable flowers, so multiplying total flowers by the proportion viable should give us the viable flowers
         
         phyto.seed.out = ifelse(treatment == "D",  
                                 allo.df[allo.df$Species == "TWIL",10]*viable.flowers.out,  
                                 allo.df[allo.df$Species == "TWIL",8]*viable.flowers.out),
         
         phyto.seed.in = ifelse(!is.na(phyto.unique), phyto.n.indiv, 3),
         ## for phyto uniques, use the # indiv as the seeds.in, otherwise put 3 as the default
         
         phyto.seed.in = ifelse(phyto.n.indiv > 3, phyto.n.indiv, phyto.seed.in),
         ## then, check for # indiv > 3, use # indiv as seeds.in here also
         phyto.seed.in = ifelse(unique.ID == 8547, phyto.n.indiv, phyto.seed.in)
         ## this phyto was a recruit, use phyto.n.indiv as seeds.in
         
         ) %>%
  
  select(unique.ID, phyto, phyto.n.indiv, phyto.seed.in, phyto.seed.out)


ggplot(twil.phyto, aes(x=phyto.seed.out)) +
  geom_histogram()


## clean up env
rm(list = c("twil", "twilC", "med_scales", "note_check"))
