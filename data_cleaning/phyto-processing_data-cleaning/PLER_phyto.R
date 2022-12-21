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

pler <- read.csv(paste0(lead, "PLER_phyto-processing-redo_20221209.csv"))

## basic cleaning function
source("data_cleaning/phyto-processing_data-cleaning/basic_cleaning_function.R")

## allometry data
source("allometry/merge_allometric_relationships.R")

## uniqueID key
source("data_cleaning/unique_key.R")

# Basic Cleaning
plerC <- basic_cleaning_func(pler)

## need to add in unique.IDs here
pler_int <- left_join(plerC, unique.key, by = c("treatment", "block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique")) %>%
  mutate(unique.ID = unique.ID.y)

# Final Cleaning ####
med_scales <- c("A", "E", "F", "G")  ## scales that need to be rounded

pler_final <- pler_int %>%
  mutate(inflor.g.rounded = ifelse(scale.ID %in% med_scales, round(inflor.g, digits = 3), inflor.g)) %>% ## round to 3 decimal places
  
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, inflor.g.rounded, empty.flower.num, flower.num, seed.num, new.flower.num, scale.ID, process.notes, census.notes, unique.ID) ## select only needed columns



# Check Outliers/Missing Vals ####

## separate by completion first
pler_complete <- pler_final %>%
  filter(complete.sample == "Y")

pler_incomplete <- pler_final %>%
  filter(complete.sample == "N")

## visualize
ggplot(pler_complete, aes(x=inflor.g.rounded)) +
  geom_histogram()
ggplot(pler_incomplete, aes(x=new.flower.num)) +
  geom_histogram()
## missing 1 value.

pler_incomplete[is.na(pler_incomplete$new.flower.num),]
## looks like this one was already processed correctly, so it was not redone. For this, need to add empty.flower.num and flower.num together and put in new.flower.num column
##unique.ID 11705

## change 11705 here ####
pler_final[pler_final$unique.ID == 11705,]$new.flower.num <- pler_final[pler_final$unique.ID == 11705,]$empty.flower.num + pler_final[pler_final$unique.ID == 11705,]$flower.num
## should be good now.

## Check phyto num 
ggplot(pler_final, aes(x=phyto.n.indiv)) +
  geom_histogram()

## check for any samples that do not have a new.flower.num
ggplot(pler_final[pler_final$complete.sample == "N",], aes(x=seed.num/2, y=new.flower.num)) +
  geom_point() +
  geom_abline(slope = 1)
  ## 2 rows had missing values
## I think that this has to do with the way that R subsetting the data, it's adding in rows with nothing but NAs. I think all incompletes have a new.flower.num now.


# Check Notes ####
## this might eventually be best in the census script?
unique(pler_final$process.notes)
  ## some phyto changes here that need to be double checked
    ## 4230 & 4430 dealt with on 12/14
    ## should be good now!

  ## a few samples will need to be removed - when parts of the inflorescence are missing - DONE!

pler.notes <- pler_final %>%
  filter(!is.na(process.notes), process.notes!= "WEIGH TOTAL BIOMASS", process.notes != "Sample missing")

## remove 9667 & 8432 ####
## unique.ID 9667 & 8432 need to be removed, missing parts of inflor
pler_final2 <- pler_final %>%
  filter(unique.ID != 9667, unique.ID != 8432)


## create empty data frame
df <- data.frame()

## loop through all notes searching for "die" or "chang"
for(i in colnames(pler_final)[17:18]) {
  tmp <- dplyr::filter(pler_final, grepl("die", pler_final[,i]))
  df <- rbind(df, tmp)
}

for(i in colnames(pler_final)[17:18]) {
  tmp <- dplyr::filter(pler_final, grepl("chang", pler_final[,i]))
  df <- rbind(df, tmp)
}

## all notes are descriptive enough that we know the correct phyto num.


# Make Phyto DF ####
pler.phyto <- pler_final2 %>%
  mutate(phyto.seed.out = ifelse(complete.sample == "N", 
                                 (new.flower.num*2), ## if incomplete, flowersx2 = seeds out
                                 (allo.df[allo.df$Species == "PLER",2] + ## intercept
                                    (allo.df[allo.df$Species == "PLER",5]*inflor.g.rounded))), ## slope
         ## if complete, use the allo relationship
         
         phyto.seed.in = ifelse(!is.na(phyto.unique), phyto.n.indiv, 3),
         ## for phyto uniques, use the # indiv as the seeds.in, otherwise put 3 as the default
         
         phyto.seed.in = ifelse(phyto.n.indiv > 3, phyto.n.indiv, phyto.seed.in)) %>%
  ## then, check for # indiv > 3, use # indiv as seeds.in here also
  
  filter(!is.na(phyto.seed.out)) %>%
  ## remove 2 missing samples
  
  select(unique.ID, phyto, phyto.n.indiv, phyto.seed.in, phyto.seed.out)


ggplot(pler.phyto, aes(x=phyto.seed.out)) +
  geom_histogram()
## 2 rows w/NAs these were missing samples, removed now.

#pler_phyto_check <- pler.phyto %>%
 # filter(is.na(phyto.seed.out))
## 2 missing samples

## clean up env
rm(list = c("pler", "pler.notes", "pler_complete", "pler_final", "pler_final2", "pler_incomplete", "pler_int", "plerC", "tmp", "df"))
