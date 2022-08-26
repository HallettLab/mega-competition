
library(googlesheets4)
library(plyr)
library(tidyverse)
library(googledrive)
library(openxlsx)

lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/" # Carmen's file path
date <- 20220825

# Upload data from dropbox ####
gitr <- read.xlsx(paste0(lead, "Phytometer-Processing/", date, "_Phyto-Processing.xlsx"), sheet = 9)


# Data Cleaning ####
## Processing Data ####

str(gitr)

### Check all Columns ####
unique(gitr$block) ## looks good
sort(unique(gitr$plot))
sort(unique(gitr$sub)) ## no sub 8. Looks like a fluke, no sub 8 in processing or collections data either
unique(gitr$bkgrd) ## one of the TWIL-I backgrounds has a space in the title
gitr_bg_space_check <- gitr %>%
  filter(bkgrd == "TWIL-I ")
  ## 4-42-21 has the space in its bkgrd column (unique 11595). Need to change this manually.

gitr[gitr$unique == 11595,]$bkgrd <- "TWIL-I" ## manually remove space from bkgrd


unique(gitr$dens)
gitr_dens_check <- gitr %>%
  filter(is.na(dens)) ## looks good, these are all controls

unique(gitr$phyto) ## all GITR, looks good
unique(gitr$phyto.n.indiv) ## 1-3 phytos, looks good

unique(gitr$phyto.unique) ## will need to standardize capitalization!

unique(gitr$`complete?`) ## will need to standardize capitalization! 
  ## Also, need to explore the NAs and Ns
  ## also need to change the column name
  ## Will need to remove the space before one of the Y values

gitr_complete_check_no <- gitr %>%
  filter(`complete?` == "N") 
## 20 are not complete

gitr_incompletes <- gitr_complete_check_no$unique ## make a vector of incompletes
## will remove all incompletes for the moment and add them back in if appropriate after checking samples

gitr_complete_check_nas <- gitr %>%
  filter(is.na(`complete?`))
## 1 NA, this sample is missing (5-23-11)

gitr_missing_samples <- gitr_complete_check_nas$unique ## make a vector of missing samples

unique(gitr$total.biomass.g) ## there are some NAs present, double check what these are
sort(unique(gitr$total.biomass.g)) ## ranges from 0.0022 - 5.9307; all seem like reasonable values

gitr_biomass_nas <- gitr %>%
  filter(is.na(total.biomass.g))
## cool, only the 1 missing sample has an NA for biomass

unique(gitr$flower.num)
sort(unique(gitr$flower.num)) ## ranges from 5-132
as.data.frame(table(gitr$flower.num))
gitr_flower_nas <- gitr %>% ## lots of NAs, but this is okay because there are biomass measurements for all of them
  filter(is.na(flower.num))

unique(gitr$scale.ID) ## will need capitalization standardization if we retain this column
unique(gitr$process.notes)
## filter for observations containing processing notes
gitr_process_notes_check <- gitr %>%
  filter(!is.na(process.notes))
    ## there are several veg phytos that we need to determine how to handle.
    ## 4-1-21; 4-2-21; 4-18-21; 12-23-24

unique(gitr$unique) ## need to change this column name so that it doesn't match the function
length(unique(gitr$unique)) == nrow(gitr) ## these are the same length, so we can infer that none of them are missing a unique number
## double check this to be sure
gitr_unique_check <- gitr %>%
  filter(is.na(unique)) ## yep no NAs in this column!


### Make Mods ####
gitr_temp <- gitr %>%
  mutate(phyto.unique = ifelse(phyto.unique == "a", "A", ## change all values to caps
                                    ifelse(phyto.unique == "b","B", phyto.unique))) %>%
  mutate(complete.sample = ifelse(`complete?` == "y", "Y", ## change all values to caps
                                    ifelse(`complete?` == " Y","Y", `complete?`))) %>% ## remove the space
  ## also change the column name for ease
  filter(complete.sample == "N", !is.na(complete.sample)) %>% ## filter out incompletes, missing phytos
  mutate(scale.ID = ifelse(scale.ID == "e", "E", scale.ID)) %>% ## change all values to caps
  mutate(unique.id = unique) %>% ## change column name
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, flower.num, scale.ID, process.notes, census.notes, unique.id) ## choose which columns to retain and which order
                          
### Double Checks ####
#gitr_phyto_unique_caps_check <- gitr_temp %>% ## looks good
 # select(phyto.unique, phyto.unique.caps)

#gitr_complete_caps_check <- gitr_temp %>% ## looks good
 # select(complete, `complete?`)

### Make final Dataframe ####
drought <- c(1, 3, 4, 6, 12, 14) ## create treatment vectors

gitr_proc_clean <- gitr_temp %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C"))



