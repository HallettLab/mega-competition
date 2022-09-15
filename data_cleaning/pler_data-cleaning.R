# PLER Data Cleaning 
## script current as of 09/14/2022
## data current as of 08/25/2022, will need to be replaced as I think some changes were made.

## load packages
library(googlesheets4)
library(plyr)
library(tidyverse)
library(googledrive)
library(openxlsx)


# Read in Data ####
lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/" # Carmen's file path
date <- 20220825

## Processing data
pler <- read.xlsx(paste0(lead, "Phytometer-Processing/", date, "_Phyto-Processing.xlsx"), sheet = 14)
## Collections data
collections <- read.xlsx(paste0(lead, "Collections/Collections_merged/", date, "_MASTER_Collections_2-in-progress.xlsx"), sheet = 2)

## make sure the dates read in correctly
collections$phyto.date.collect <- as.Date(collections$phyto.date.collect, origin = "1899-12-30")
collections$phyto.date.census <- as.Date(collections$phyto.date.census, origin = "1899-12-30")
collections$bg.date.census <- as.Date(collections$bg.date.census, origin = "1899-12-30")
collections$phyto.unique <- as.character(collections$phyto.unique)



# Data Cleaning ####
theme_set(theme_bw())
## Processing Data ####
### Check ID info ####
ggplot(pler, aes(x=bkgrd)) +
  geom_bar() ## TWIL-U and THIR-U still in this, need to be removed
unique(pler$bkgrd) ## will need to remove TWIL-U & THIR-U backgrounds

unique(pler$dens)
pler_dens_na_check <- pler %>% ## all dens NAs are controls. Good.
  filter(is.na(dens))

unique(pler$phyto) ## all PLER, good
unique(pler$phyto.unique) ## need to standardize capitalization

### Check Sample Completion ####
ggplot(pler, aes(x=`complete?`)) +
  geom_bar()
unique(pler$`complete?`) ## need to remove a space from one of the N values; 
    ## also there is an "A"... not sure what this means here?
    ## also need to change the column name
pler[pler$block == 8 & pler$plot == 25 & pler$sub == 8 & pler$phyto.unique == "A",]$`complete?` <- "N"
## pler[pler$block == 15 & pler$plot == 4 & pler$sub == 15 & pler$phyto.unique == "A", ] ##15-4-15
    ## eventually use the above line to fix the "A" value in the complete? column after checking sample.

## also check the NA and N values in the complete column!!!
pler_complete_nas_check <- pler %>%
  filter(is.na(`complete?`)) %>%
  filter(plot < 43)
    ## 4-27-11; 7-29-12
    ## two samples are not processed at all- yep these are both marked as missing. 
    ## These should be removed

pler_complete_no_check <- pler %>%
  filter(`complete?` == "N") %>%
  filter(plot<43)
    ## all samples except 1 are processed correctly for not being complete
    ## some of these samples still have inflor.g filled in, but this number should not be used!
    ## 8-22-8 -> marked as incomplete but no other processing data present. This sample is not marked as missing
    ## Based on notes, we might not be able to use the following samples: 14-4-21, 15-11-15
    ## both have notes suggesting that parts of the inflorescences are missing. 
    ## Will remove these for now and add back in later after checking the samples to determine usability.


### Check Measured Info ####
ggplot(pler, aes(x=phyto.n.indiv)) +
  geom_bar()
## 1-3 phytos

ggplot(pler, aes(x=inflor.g)) +
  geom_histogram()
## 165 NAs removed...

pler_inflor_na_check <- pler %>%
  filter(is.na(inflor.g)) %>%
  filter(plot < 43) %>%
  filter(`complete?` == "Y")
    ## all inflor.g values that were marked NA were incomplete samples.
    ## the two missing samples also show up here again.


unique(pler$total.biomass.g)
sort(unique(pler$total.biomass.g)) ## varies from 0.0276 - 0.7537
    ## this looks good
    ## also need to check if all of the BRHO background phytos have total biomass weighed.

pler_tot.bio_check <- pler %>%
  filter(bkgrd == "BRHO") ## yep, this looks good!

unique(pler$scale.ID)
pler_scaleA <- pler %>%   ## 4 decimal places
  filter(scale.ID == "A")
pler_scaleE <- pler %>%   ## 3 decimal places
  filter(scale.ID == "E")


unique(pler$empty.flower.num)
sort(unique(pler$empty.flower.num)) ## ranges from 0-130; includes NAs but this makes sense as not all samples were processed this way.
## I'm not sure why there are numbers with decimals here? 

pler$empty.flower.num <- as.character(pler$empty.flower.num)

pler_empty.flower.num_decimal_check <- pler %>%
  filter(str_detect(empty.flower.num, ".5")) ## hard to get the filtering to work correctly
    ## so far it looks like there are 4 instances where the empty.flower.num ends with .5
    ## samples are: 1-1-10; 1-2-3; 1-10-3; 1-14-12

pler$empty.flower.num <- as.numeric(pler$empty.flower.num) ## change back to numeric

unique(pler$flower.num)
sort(unique(pler$flower.num))
  ## this is a character currently as one of the values is a space...
  ## again, not sure why there are numbers with decimals
pler[pler$flower.num == " ",] ## looks like 1-30-23
pler[pler$block == 1 & pler$plot == 30 & pler$sub == 23,]$flower.num <- NA ## change the space to an NA


pler_flower.num_decimal_check <- pler %>%
  filter(str_detect(flower.num, ".5$"))
## there are 4 samples that have a flower.num ending in .5 
## I guess check through these samples? I'm not really sure why they have the decimals, or whether it's possible to recount these samples if necessary?
## samples are: 1-1-10; 1-2-3; 1-10-3; 1-14-12

pler$flower.num <- as.numeric(pler$flower.num) ## change flower.num to a number rather than a character

unique(pler$seed.num)
sort(unique(pler$seed.num)) ## varies from 6-906 

unique(pler$process.notes) ## two notes re: broken inflorescences already recorded on data-cleaning spreadsheet

unique(pler$unique)
## no unique values. We will need to add these back in, probably from the census data.
## also still need to change the column names


## Make all Mods ####
pler_temp <- pler %>%
  filter(plot < 43) %>% ## remove trifolium sub-experiment phytos
  mutate(phyto.unique = ifelse(phyto.unique == "a", "A", ## change all phyto.unique values to caps
                               ifelse(phyto.unique == "b", "B", 
                                      ifelse(phyto.unique == "c", "C", phyto.unique)))) %>%
  filter(!is.na(`complete?`)) %>% ## remove the 2 missing samples (which are the only ones that have NAs in the complete column)
  mutate(complete.sample = `complete?`, unique.ID = unique)
  filter(!(block == 14 & plot == 4 & sub == 21), !(block == 15 & plot == 11 & sub == 15)) %>% ## removing two samples that seem like they are missing part of the inflorescence. Only add back in after physically checking sample to determine suitability.
    select()
  
## Make final Dataframe ####
drought <- c(1, 3, 4, 6, 12, 14) ## create treatment vectors

pler_proc_clean <- pler_temp %>% ## NOT COMPLETE YET, SEVERAL ISSUES TO RESOLVE!!!
  mutate(treatment = ifelse(block %in% drought, "D", "C"))
