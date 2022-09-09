## GITR Data Cleaning
## script current as of 09/01/2022
## data current as of 08/25/2022

library(googlesheets4)
library(plyr)
library(tidyverse)
library(googledrive)
library(openxlsx)

# Read in Data ####
lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/" # Carmen's file path
date <- 20220825

## Upload data from dropbox ####
## Processing data
gitr <- read.xlsx(paste0(lead, "Phytometer-Processing/", date, "_Phyto-Processing.xlsx"), sheet = 9)
## Collections data
collections <- read.xlsx(paste0(lead, "Collections/Collections_merged/", date, "_MASTER_Collections_2-in-progress.xlsx"), sheet = 2)

## make sure the dates read in correctly
collections$phyto.date.collect <- as.Date(collections$phyto.date.collect, origin = "1899-12-30")
collections$phyto.date.census <- as.Date(collections$phyto.date.census, origin = "1899-12-30")
collections$bg.date.census <- as.Date(collections$bg.date.census, origin = "1899-12-30")
collections$phyto.unique <- as.character(collections$phyto.unique)



# Data Cleaning ####
## Processing Data ####
theme_set(theme_bw())
str(gitr)

### Check ID Info ####
unique(gitr$bkgrd) ## one of the TWIL-I backgrounds has a space in the title
gitr[gitr$bkgrd == "TWIL-I ",]
  ## 4-42-21 has the space in its bkgrd column (unique 11595). Need to change this manually.
gitr[gitr$unique == 11595,]$bkgrd <- "TWIL-I" ## manually remove space from bkgrd

unique(gitr$dens)
gitr_dens_check <- gitr %>%
  filter(is.na(dens)) ## looks good, these are all controls

unique(gitr$phyto) ## all GITR, looks good
unique(gitr$phyto.n.indiv) ## 1-3 phytos, looks good

unique(gitr$phyto.unique) ## will need to standardize capitalization!

### Check Sample Completion ####
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



### Check Measured Info ####
ggplot(gitr, aes(x=total.biomass.g)) +
  geom_histogram()
## a few outliers

gitr_biomass_nas <- gitr %>%
  filter(is.na(total.biomass.g))
## cool, only the 1 missing sample has an NA for biomass

## flower number
ggplot(gitr, aes(x=flower.num)) +
  geom_histogram()
ggplot(gitr, aes(x=total.biomass.g, y=flower.num)) +
  geom_point()

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
  filter(complete.sample != "N", !is.na(complete.sample)) %>% ## filter out incompletes, missing phytos
  mutate(scale.ID = ifelse(scale.ID == "e", "E", scale.ID)) %>% ## change all values to caps
  mutate(unique.ID = unique) %>% ## change column name
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, flower.num, scale.ID, process.notes, census.notes, unique.ID) ## choose which columns to retain and which order
                          
### Double Checks ####
#gitr_phyto_unique_caps_check <- gitr_temp %>% ## looks good
 # select(phyto.unique, phyto.unique.caps)

#gitr_complete_caps_check <- gitr_temp %>% ## looks good
 # select(complete, `complete?`)

## Round to 3 digits
gitr_temp2 <- gitr_temp %>%
  mutate(total.biomass.g.rounded = round(total.biomass.g, digits = 3))

round_check <- gitr_temp2 %>%
  select(total.biomass.g, total.biomass.g.rounded)
  

### Make final Dataframe ####
drought <- c(1, 3, 4, 6, 12, 14) ## create treatment vectors

gitr_proc_clean <- gitr_temp2 %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C"))

## Collections Data ####
### Needed tweaks ####
## filter to only GITR phytos, get rid of unused backgrounds and phytos that did not survive
gitr_cen <- collections %>%
  filter(phyto == "GITR", bkgrd != "VIVI", bkgrd != "ERBO", phyto.n.indiv > 0) %>%
  mutate(phyto.unique = ifelse(phyto.unique == "a", "A", ## change all values to caps
                               ifelse(phyto.unique == "b","B", phyto.unique))) %>%
  mutate(unique.ID = unique) %>%
  mutate(Nbrhood.size = 10)

unique(gitr_cen$dens)
unique(gitr_cen$phyto)
unique(gitr_cen$phyto.date.census) 
unique(gitr_cen$phyto.n.indiv)
unique(gitr_cen$Nbrhood.size)
## should finish filling in the neighborhood size column
## DONE


unique(gitr_cen$bkgrd) ## one of the TWIL-I backgrounds has a space in the title
gitr_cen[gitr_cen$bkgrd == "TWIL-I ",]
## 4-42-21 has the space in its bkgrd column (unique 11595). Need to change this manually.
gitr_cen[gitr_cen$unique == 11595,]$bkgrd <- "TWIL-I" ## manually remove space from bkgrd



ggplot(gitr_cen, aes(x=bkgrd.n.indiv)) +
  geom_histogram(bins = 50) +
  facet_wrap(~bkgrd)

## Set all control background # of individuals to NA
gitr_cen[gitr_cen$bkgrd == "Control",]$bkgrd.n.indiv <- NA

### Check weeds ####
ggplot(gitr_cen, aes(x=CRCO)) +
  geom_histogram(bins = 50) +
  facet_wrap(~block)

ggplot(gitr_cen, aes(x=ERBO)) +
  geom_histogram(bins = 50, binwidth = 1) +
  facet_wrap(~block)
## There's one 10cm subplot with 8 ERBO? That seems hard to believe that 8 would fit in that area

ggplot(gitr_cen, aes(x=FIGA)) +
  geom_histogram(bins = 50) +
  facet_wrap(~block)

ggplot(gitr_cen, aes(x=SIGA)) +
  geom_histogram(bins = 50) +
  facet_wrap(~block)

ggplot(gitr_cen, aes(x=GAMU)) +
  geom_histogram(bins = 50) +
  facet_wrap(~block)

ggplot(gitr_cen, aes(x=HYGL)) +
  geom_histogram(bins = 50) +
  facet_wrap(~block)

ggplot(gitr_cen, aes(x = other)) +
  geom_histogram(bins = 50) +
  facet_wrap(~block)

# Merge the Dataframes ####
gitr_all <- left_join(gitr_proc_clean, gitr_cen, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique", "unique.ID"))

ggplot(gitr_all, aes(x = phyto.n.indiv.x, y = phyto.n.indiv.y)) +
  geom_point()
## this looks good

#phyto.indiv.na <- gitr_all %>%
 # filter(is.na(phyto.n.indiv.x) | is.na(phyto.n.indiv.y))
## looks like 4-42-21 didn't have a phyto number in the collections data frame. Since there is one in the processing datasheet this is not an issue.

ggplot(gitr_all, aes(x=unique.ID, y=unique)) +
  geom_point()
#unique.na <- gitr_all %>%
 # filter(is.na(unique) | is.na(unique.ID))
## 4-42-21-B doesn't have any data from the collections sheet, which is odd since it is filled in on this sheet.
#gitr_cen[gitr_cen$unique == "11595",]
## ohhh this was the one that had a space in the background "TWIL-I ". Will need to fix this before merging the dataframes.
    ## FIXED


gitr_clean <- gitr_all %>%
  mutate(phyto.n.indiv = phyto.n.indiv.x) %>%
  select(-phyto.n.indiv.y, -unique, -phyto.n.indiv.x)



ggplot(gitr_clean, aes(y=total.biomass.g.rounded, x = treatment)) +
  geom_boxplot() +
  facet_wrap(~bkgrd)

ggplot(gitr_clean, aes(y=total.biomass.g.rounded, x = bkgrd.n.indiv)) +
  geom_point() +
  facet_wrap(~bkgrd)


