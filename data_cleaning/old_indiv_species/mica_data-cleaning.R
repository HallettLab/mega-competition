# MICA Data Cleaning 
## script current as of 09/15/2022
## processing data current as of 09/14/2022
## collections data current as of 08/25/2022 -- this should be updated to the most current version

library(googlesheets4)
library(plyr)
library(tidyverse)
library(googledrive)
library(openxlsx)


# Read in Data ####
lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/" # Carmen's file path
date <- 20220914
date_collections <- 20220825

## Processing data
mica <- read.xlsx(paste0(lead, "Processing/Phytometer-Processing/Phytometer-Processing_entered/", date, "_Phyto-Processing.xlsx"), sheet = 13)

## Collections data
collections <- read.xlsx(paste0(lead, "Collections/Collections_merged/", date_collections, "_MASTER_Collections_2-in-progress.xlsx"), sheet = 2)

## make sure the dates read in correctly
collections$phyto.date.collect <- as.Date(collections$phyto.date.collect, origin = "1899-12-30")
collections$phyto.date.census <- as.Date(collections$phyto.date.census, origin = "1899-12-30")
collections$bg.date.census <- as.Date(collections$bg.date.census, origin = "1899-12-30")
collections$phyto.unique <- as.character(collections$phyto.unique)



# Data Cleaning ####
## Processing Data ####
theme_set(theme_bw())

### Check ID info ####
ggplot(mica, aes(x=bkgrd)) +
  geom_bar() ## there are some here in an ERBO background.
mica_erbo_bg_check <- mica %>%
  filter(bkgrd == "ERBO") ## there are 5-8-10 A & B here. 
    ## Check if these were used in place of a control that didn't survive
    ## (this is the most likely option I can foresee)
    ## yep, notes in the census data say to use this as a control!

ggplot(mica, aes(x=dens)) +
  geom_bar() 
mica_dens_na_check <- mica %>% ## all dens NAs are controls. Good.
  filter(is.na(dens))
unique(mica$phyto) ## MICA, good.

unique(mica$phyto.unique) ## need to standardize capitalization
 
ggplot(mica, aes(x=phyto.n.indiv)) +
  geom_bar() ## 1-3, good

### Check Sample Completion ####
ggplot(mica, aes(x=`complete?`)) + 
  geom_bar()
## need to standardize capitalization

## check the NAs
mica_complete_nas_check <- mica %>%
  filter(is.na(`complete?`))
    ## 3 samples have no data at all and are marked missing in weird things
          ## 1-12-17; 1-17-23; 6-21-16
          ## missing samples should be removed.
    ## update, 1 more sample is missing but it is marked as 'Y' in the complete column so it doesn't show up in this check
    

mica_complete_no_check <- mica %>%
  filter(`complete?` == "n" | `complete?` == "N")
  ## 1 sample marked not complete. 
    ## Exclude this sample until checking it
    ## 12-9-8
    ## the note makes it sound like we should check the sample to see if it is usable, it might just have a senesced top rather than be incomplete
## 5 samples were also marked incomplete in the final check on 9/1
    ## 8-6-14; 16-4-3; 16-7-3; 16-11-3; 16-13-3
    ## all of these samples should be removed 


### Check Measured Info ####
ggplot(mica, aes(x=total.biomass.g)) + 
  geom_histogram(bins = 50)

sort(unique(mica$total.biomass.g)) ## ranges from 0.001 - 3.555
    ## the numbers include 5 decimal places. Looks like there is one value that has 5 decimal places, the others have 3-4. Isolate this value (0.15496)
mica[mica$total.biomass.g == 0.15496,]
    ## this one should perhaps be reweighed, because it looks like there is an extra digit in here
    ## it is sample 16-12-3

mica_totbio_na_check <- mica %>% ## these are the 4 missing samples, nothing else.
  filter(is.na(total.biomass.g))

ggplot(mica, aes(x=seed.num)) +
  geom_histogram(bins = 50)

unique(mica$biomass.no.seed.g) ## this column is empty and should be removed

unique(mica$scale.ID) ## standardize capitalization
    ## ALSO: need to decide how to deal with diff #'s of decimal places from the 2 scales.

unique(mica$process.notes)
unique(mica$unique) ## change column name
    ## All NAs, make sure to add these data in from the census data.


### Make Mods ####
mica_temp <- mica %>%
  mutate(phyto.unique = ifelse(phyto.unique == "a", "A", ## change all phyto.unique values to caps
                               ifelse(phyto.unique == "b", "B", phyto.unique))) %>%
  mutate(complete.sample = ifelse(`complete?` == "y", "Y", ## change all values to caps
                                  ifelse(`complete?` == "n","N", `complete?`))) %>%
  mutate(unique.ID = unique) %>% ## change column name
  mutate(scale.ID = ifelse(scale.ID == "e", "E", scale.ID)) %>% ## change all values to caps
  filter(!is.na(total.biomass.g)) %>% ## remove the 4 missing samples; went from 382 - 378 samples
  filter(complete.sample == "Y") %>% ## remove the incomplete/unknown completeness samples
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, seed.num, scale.ID, process.notes, census.notes, unique.ID) ## pare down and reorder columns

## Round to 3 digits
mica_temp2 <- mica_temp %>%
  mutate(total.biomass.g.rounded = round(total.biomass.g, digits = 3))

### Make final dataframe ####
drought <- c(1, 3, 4, 6, 12, 14) ## create treatment vector

mica_proc_clean <- mica_temp2 %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C")) %>% ## add a treatment column
  select(-total.biomass.g)


## Census Data ####

### Needed tweaks ####
## check for mis-spellings or spaces in phyto names
unique(collections$phyto) ## all good

## filter to only MICA phytos, get rid of unused backgrounds and phytos that did not survive
    ## since there was an ERBO bg used as a control, the ERBO backgrounds should not be removed. 
mica_cen <- collections %>%
  filter(phyto == "MICA", bkgrd != "VIVI", phyto.n.indiv > 0) %>%
  mutate(phyto.unique = ifelse(phyto.unique == "a", "A", ## change all values to caps
                               ifelse(phyto.unique == "b","B", phyto.unique))) %>%
  mutate(unique.ID = unique) %>%
  mutate(Nbrhood.size = 10)

unique(mica_cen$dens)
unique(mica_cen$phyto)
unique(mica_cen$phyto.date.census) 
unique(mica_cen$phyto.n.indiv)
unique(mica_cen$Nbrhood.size)
## should finish filling in the neighborhood size column
## DONE

ggplot(mica_cen, aes(x=bkgrd)) +
  geom_bar()

ggplot(mica_cen, aes(x=bkgrd.n.indiv)) +
  geom_histogram(bins = 50) +
  facet_wrap(~bkgrd)
## some of the backgrounds are NAs, these should be controls. All controls should have NA backgrounds really

bg.na.indiv <- mica_cen %>%
  filter(is.na(bkgrd.n.indiv)) ## yep all controls

## Set all control background # of individuals to NA
mica_cen[mica_cen$bkgrd == "Control",]$bkgrd.n.indiv <- NA

### Check weeds ####
ggplot(mica_cen, aes(x=CRCO)) +
  geom_histogram(bins = 50) +
  facet_wrap(~block)

ggplot(mica_cen, aes(x=ERBO)) +
  geom_histogram(bins = 50, binwidth = 1) +
  facet_wrap(~block)

ggplot(mica_cen, aes(x=FIGA)) +
  geom_histogram(bins = 50) +
  facet_wrap(~block)

ggplot(mica_cen, aes(x=SIGA)) +
  geom_histogram(bins = 50) +
  facet_wrap(~block)

ggplot(mica_cen, aes(x=GAMU)) +
  geom_histogram(bins = 50) +
  facet_wrap(~block)

ggplot(mica_cen, aes(x=HYGL)) +
  geom_histogram(bins = 50) +
  facet_wrap(~block)

ggplot(mica_cen, aes(x = other)) +
  geom_histogram(bins = 50) +
  facet_wrap(~block)

# Merge the two dataframes ####
nrow(mica_proc_clean) ## 372 rows
nrow(mica_cen) ## 382 rows
## There are 10 samples that need to be accounted for

mica_unmatched <- anti_join(mica_cen, mica_proc_clean, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))
## all of these are missing or incomplete.

mica_all <- left_join(mica_proc_clean, mica_cen, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))
    ## can't merge with unique.ID, the processing data never had unique numbers in it!

ggplot(mica_all, aes(x = phyto.n.indiv.x, y = phyto.n.indiv.y)) +
  geom_point()
## this looks good

mica_clean <- mica_all %>%
  mutate(phyto.n.indiv = phyto.n.indiv.x,
         unique.ID = unique.ID.y) %>%
  select(-phyto.n.indiv.y, -unique, -phyto.n.indiv.x, -unique.ID.y, -unique.ID.x)



## Clean up environment
rm(list = c("bg.na.indiv", "collections", "date", "date_collections", "drought", "lead", "mica", "mica_all", "mica_cen", "mica_complete_nas_check", "mica_complete_no_check", "mica_dens_na_check", "mica_erbo_bg_check", "mica_proc_clean", "mica_temp", "mica_temp2", "mica_totbio_na_check", "mica_unmatched"))
