# PLER Data Cleaning 

library(googlesheets4)
library(plyr)
library(tidyverse)
library(googledrive)
library(openxlsx)

## QUESTION: How will we account for using scales with different specificity levels? I believe scales A & E have diff signif digits
    ## yep, scale A has 4 decimal places and scale E has 3 decimal places.

lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/" # Carmen's file path
date <- 20220825

pler <- read.xlsx(paste0(lead, "Phytometer-Processing/", date, "_Phyto-Processing.xlsx"), sheet = 14)


# Processing Data ####

str(pler)

## Check all Columns ####
### ID info ####
unique(pler$block) ## looks good
unique(pler$plot)
sort(unique(pler$plot)) ## still contains phytos from the trifolium subexperiment
unique(pler$sub)
sort(unique(pler$sub))
unique(pler$bkgrd) ## will need to remove TWIL-U & THIR-U backgrounds
unique(pler$dens)
pler_dens_na_check <- pler %>% ## all dens NAs are controls. Good.
  filter(is.na(dens))
unique(pler$phyto) ## all PLER, good
unique(pler$phyto.unique) ## need to standardize capitalization

### Collected/Measured Info ####
unique(pler$phyto.n.indiv) ## 1-3 phytos

unique(pler$`complete?`) ## need to remove a space from one of the N values; 
    ## also there is an "A"... not sure what this means here?
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



unique(pler$inflor.g) ## some NAs present, which makes sense for samples that were incomplete. Still check which ones are NAs.
sort(unique(pler$inflor.g)) ## varies from 0.0010 - 2.4628g
pler_inflor_na_check <- pler %>%
  filter(is.na(inflor.g)) %>%
  filter(plot < 43) %>%
  filter(`complete?` == "Y")
    ## all inflor.g values that were marked NA were not complete samples.
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
  filter(!(block == 14 & plot == 4 & sub == 21), !(block == 15 & plot == 11 & sub == 15)) ## removing two samples that seem like they are missing part of the inflorescence. Only add back in after physically checking sample to determine suitability.
  
## Make final Dataframe ####
drought <- c(1, 3, 4, 6, 12, 14) ## create treatment vectors

pler_proc_clean <- pler_temp %>% ## NOT COMPLETE YET, SEVERAL ISSUES TO RESOLVE!!!
  mutate(treatment = ifelse(block %in% drought, "D", "C"))
