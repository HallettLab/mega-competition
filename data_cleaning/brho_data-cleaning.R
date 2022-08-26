
library(googlesheets4)
library(plyr)
library(tidyverse)
library(googledrive)
library(openxlsx)

lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/" # Carmen's file path
date <- 20220825

# Download processing data from google drive ####
drive_download(
  file = "https://docs.google.com/spreadsheets/d/1-Rov2Her2obv7pRGuF-RP5ZbyxIL26okHbRmJAB614Q/edit?usp=sharing",
  path = paste0(lead, "Phytometer-Processing/", date, "_Phyto-Processing"),
  type = "xlsx",
  overwrite = FALSE
)


# Upload data from dropbox ####
## BRHO
brho <- read.xlsx(paste0(lead, "Phytometer-Processing/", date, "_Phyto-Processing.xlsx"), sheet = 5)


# Data Cleaning ####
## Processing Data ####

str(brho)
brho$inflor.g <- as.numeric(brho$inflor.g)

### Check all Columns ####
unique(brho$block) ## correct blocks present, no NAs
sort(unique(brho$plot))
sort(unique(brho$sub))
unique(brho$bkgrd)
unique(brho$dens) ## some NAs here, check if they are controls
brho_dens_nas <- brho %>%
  filter(is.na(dens)) ## yep all are controls
unique(brho$phyto)
unique(brho$phyto.n.indiv) ## 0-4 phytos. Shouldn't be 0 phytos in this data sheet though.
brho_phyto_zero <- brho %>%
  filter(phyto.n.indiv == 0) ## looks like two of the phytos that were buried by gophers were still included in the processing datasheet. Remove these from the data frame
unique(brho$phyto.unique) ## need to standardize caps on this.
unique(brho$`complete?`) ## should explore NA & N values more closely to see if they are good to use; probs also change the column name as the question mark is throwing it off.

brho_complete_check_no <- brho %>%
  filter(`complete?` != "Y") 
## there are 7 phytos that aren't marked complete. 
    ## 2 are vegetative (I think we should use these and it will just predict a small amount of seeds that they might have eventually produced seed?)
    ## 4 are missing some part of an inflorescence
    ## 1 without any notes that we should look further into to determine if it should be used (12-8-11)

brho_complete_check_nas <- brho %>%
  filter(is.na(`complete?`)) %>% ## a lot of these are the trifolium sub-exp 
  filter(plot < 43) ## fixed that
## 6 phytos have nothing in the complete column
    ## 1 phyto is missing and has not been processed (14-18-20)
    ## 2 are the gopher phytos that were never collected (7-25-8 a & b)
        ## these are already removed with phytos that are 0
    ## need to check completeness on: 8-25-6, 1-22-18, 3-42-4

brho_NC <- c(11698, 4794, 9935, 10360, 8781) ## make a vector of the 4 incomplete samples and the 1 missin sample


sort(unique(brho$total.biomass.g)) ## varies from 0.0229 - 5.0660; should be an okay range. The 5g value is quite a bit larger than the next largest (3.7690) but still probably reasonable
unique(brho$biomass.no.seed.g) ## all NAs this is good. Probably can remove this column.
sort(unique(brho$inflor.g)) ## ranges from 0.004 to 3.051; these all seem like reasonable values.
  ## there are some NAs that should be double checked
brho_inflor_na <- brho %>%
  filter(is.na(inflor.g), plot < 43)
  ## All except 3 have an actual seed number count plus total biomass (9 in total). 
  ## It is probably okay to leave as is because these ones have their seeds counted already.
  ## The other 3 are the 2 gopher casualties + the missing sample.

sort(unique(brho$seed.num)) ## varies from 9-1729; lot of NAs but this makes sense
unique(brho$scale.ID) ## using scales A and E; some do not have scale filled in on this.
unique(brho$process.notes) ## looks like processing notes issues have come up in other columns (i.e. veg phytos, missing inflorescences)
unique(brho$census.notes) ## comb through these when cleaning census data.
unique(brho$unique)
length(unique(brho$unique)) ## length 480, while the length of brho dataframe is 483
unique.check <- as.data.frame(table(brho$unique)) %>%
  filter(Freq != 1)
length(unique(brho$unique)) == nrow(brho)
unique.check.nas <- brho %>%
  filter(is.na(unique))
## 4 observations are missing a unique number. 
## all 4 have unique numbers in the master collections data. These need to be added back in.
## 4-15-13 B = 11795; 4-16-13 B = 11796; 14-11-7 = 8593; 16-9-5 = 10649


### Make Mods ####
## create treatment vectors
drought <- c(1, 3, 4, 6, 12, 14)

brho_temp <- brho %>%
  filter(plot < 43, phyto.n.indiv > 0) %>% 
  ## remove trifolium sub experiment phytos; 
  ## remove 2 phytos that were buried by gophers
  mutate(treatment = ifelse(block %in% drought, "D", "C")) %>% ## add a treatment column
  mutate(phyto.unique.caps = ifelse(phyto.unique == "a", "A", ## change all values to caps
                                    ifelse(phyto.unique == "b","B", phyto.unique))) %>%
  mutate(complete = `complete?`, unique.id = unique) %>% 
  ## change the name of the complete column to be easier to use
  ## change the name of the unique column so that it doesn't match a function
  select(-`complete?`, -unique, -biomass.no.seed.g) %>%
  filter(!unique.id %in% brho_NC) ## remove the incomplete phytos

## add unique.id numbers for 4 samples missing it.
brho_temp[brho_temp$block == 4 & brho_temp$plot == 15 & brho_temp$sub == 13 & brho_temp$phyto.unique.caps == "B",]$unique.id <- 11795

brho_temp[brho_temp$block == 4 & brho_temp$plot == 16 & brho_temp$sub == 13 & brho_temp$phyto.unique.caps == "B",]$unique.id <- 11796

brho_temp[brho_temp$block == 14 & brho_temp$plot == 11 & brho_temp$sub == 7,]$unique.id <- 8593

brho_temp[brho_temp$block == 16 & brho_temp$plot == 9 & brho_temp$sub == 5,]$unique.id <- 10649

### Double Checks ####
## double check that the rewrite of phyto.unique worked w/o errors
unique(brho_temp$phyto.unique.caps)
phyto.unique.checks <- brho_temp %>%
  select(phyto.unique, phyto.unique.caps)

unique.id.final.checks <- brho_temp %>%
  filter(is.na(unique.id)) ## looks good

### Make final dataframe ####
brho_proc_clean <- brho_temp

## clean up environment
rm(list = c("unique.id.final.checks"))

## Census Data ####