## BRHO Data Cleaning
## script current as of 09/15/2022
## processing data current as of 09/14/2022
## collections data current as of 08/25/2022 -- this should be updated to the most current version

## load packages
library(googlesheets4)
library(plyr)
library(tidyverse)
library(googledrive)
library(openxlsx)

# Read in Data ####
lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/" # Carmen's file path
date <- 20220914
date_collections <- 20220825
## Download processing data from google drive ####
#drive_download(
#  file = "https://docs.google.com/spreadsheets/d/1-Rov2Her2obv7pRGuF-RP5ZbyxIL26okHbRmJAB614Q/edit?usp=sharing",
#  path = paste0(lead, "Processing/Phytometer-Processing/Phytometer-Processing_entered/", date, "_Phyto-Processing"),
#  type = "xlsx",
 # overwrite = FALSE
#)


## Upload data from dropbox ####
## Processing data
brho <- read.xlsx(paste0(lead, "Processing/Phytometer-Processing/Phytometer-Processing_entered/", date, "_Phyto-Processing.xlsx"), sheet = 5)
## Collections data
collections <- read.xlsx(paste0(lead, "Collections/Collections_merged/", date_collections, "_MASTER_Collections_2-in-progress.xlsx"), sheet = 2)

## make sure the dates read in correctly
collections$phyto.date.collect <- as.Date(collections$phyto.date.collect, origin = "1899-12-30")
collections$phyto.date.census <- as.Date(collections$phyto.date.census, origin = "1899-12-30")
collections$bg.date.census <- as.Date(collections$bg.date.census, origin = "1899-12-30")
collections$phyto.unique <- as.character(collections$phyto.unique)


# Data Cleaning ####
theme_set(theme_bw())

## Processing Data ####
### Check ID info ####
ggplot(brho, aes(x=bkgrd)) +
  geom_bar() ## TWIL-U and THIR-U still in this, need to be removed
    ## DONE

ggplot(brho, aes(x=dens)) +
  geom_bar() 
    ## some NAs here, check if they are controls
brho_dens_nas <- brho %>%
  filter(is.na(dens)) ## yep all are controls

unique(brho$phyto) ## all good

ggplot(brho, aes(x=phyto.n.indiv)) +
  geom_bar() 
    ## 0-4 phytos. 
    ## Shouldn't be 0 phytos in this data sheet though.
brho[brho$phyto.n.indiv==0,] ## looks like two of the phytos that were buried by gophers were still included in the processing datasheet. Remove these from the data frame
    ## DONE

unique(brho$phyto.unique) ## need to standardize caps on this.
    ## DONE


### Check sample completion ####
ggplot(brho[brho$plot < 43,], aes(x=`complete?`)) + 
  geom_bar()
    ## should explore NA & N values more closely to see if they are good to use;
    ##Also, change the column name as the question mark is throwing it off.

brho_complete_check_no <- brho %>%
  filter(`complete?` == "N") 
## there are 10 phytos that are marked not complete. 
    ## 2 are vegetative
    ## 4 are missing some part of an inflorescence
    ## 1 without any notes that we should look further into to determine if it should be used
## for now, remove all incompletes, but check the vegetative & no notes samples

brho_complete_check_nas <- brho %>%
  filter(is.na(`complete?`)) %>% ## a lot of these are the trifolium sub-exp 
  filter(plot < 43) ## fixed that
    ## 1 phyto is missing and has not been processed (14-18-20)
    ## 2 are the gopher phytos that were never collected (7-25-8 a & b)
    ## need to check completeness on 3 samples: 8-25-6, 1-22-18, 3-42-4

brho_NoC <- unique(brho_complete_check_no$unique) ## make a vector of the incomplete samples
brho_NAC <- unique(brho_complete_check_nas$unique) ## make a vector of the NA complete samples
    ## REMOVED THESE.

### Check Measured Info ####
ggplot(brho, aes(x=total.biomass.g)) + ## 387 of these are NAs which makes sense - this was not the primary measurement.
  geom_histogram(bins = 50)
## there's one 5g sample - this is fairly large - but it could also be multiple phytos, so probably nothing to worry about 

unique(brho$biomass.no.seed.g) ## all NAs this is good. Remove this column.

ggplot(brho, aes(x=inflor.g)) +
  geom_histogram()
str(brho) ## inflor.g is a character

sort(unique(brho$inflor.g))
  ## one value contains g, that's why this is a character, not a number.
  ## "1.6847g"
brho[brho$inflor.g == "1.6847g",]
## unique ID 5568; 7-15-8
## manually fix that here
brho[brho$block == 7 & brho$plot == 15 & brho$sub == 8,]$inflor.g <- "1.6847" 
brho$inflor.g <- as.numeric(brho$inflor.g) ## change inflor.g to a numeric variable
ggplot(brho[brho$plot < 43,], aes(x=inflor.g)) +
  geom_histogram()

## explore the NAs
brho_inflor_na <- brho %>%
  filter(is.na(inflor.g), plot < 43)
  ## All except 3 have an actual seed number count plus total biomass (9 in total). 
  ## We may need to go back to measure inflor biomass on these??
  ## The other 3 are the 2 gopher casualties + the missing sample.

ggplot(brho[brho$plot < 43,], aes(x=inflor.g)) +
  geom_histogram(bins = 50)
## one outlier of 3g, but again this could be multiple phytos

ggplot(brho[brho$plot < 43,], aes(x=seed.num)) +
  geom_histogram(bins = 50)

unique(brho$scale.ID) ## using scales A and E; some do not have scale filled in on this.
unique(brho$process.notes) ## looks like processing notes issues have come up in other columns (i.e. veg phytos, missing inflorescences)
unique(brho$unique)
length(unique(brho$unique)) == nrow(brho)
## these are not the same length, indicating that some phytos do not have a unique number
unique.check.nas <- brho %>%
  filter(is.na(unique))
## 4 observations are missing a unique number. 
## all 4 have unique numbers in the master collections data. These need to be added back in.
## 4-15-13 B = 11795; 4-16-13 B = 11796; 14-11-7 = 8593; 16-9-5 = 10649

## add unique.id numbers for 4 samples missing it.
brho[brho$block == 4 & brho$plot == 15 & brho$sub == 13 & brho$phyto.unique == "B",]$unique <- 11795
brho[brho$block == 4 & brho$plot == 16 & brho$sub == 13 & brho$phyto.unique == "B",]$unique <- 11796
brho[brho$block == 14 & brho$plot == 11 & brho$sub == 7,]$unique <- 8593
brho[brho$block == 16 & brho$plot == 9 & brho$sub == 5,]$unique <- 10649



### Make Mods ####
brho_temp <- brho %>%
  filter(plot < 43, phyto.n.indiv > 0) %>% 
  ## remove trifolium sub experiment phytos; 
  ## remove 2 phytos that were buried by gophers
  mutate(phyto.unique = ifelse(phyto.unique == "a", "A", ## change all values to caps
                                    ifelse(phyto.unique == "b","B", phyto.unique))) %>%
  mutate(complete.sample = `complete?`, unique.ID = unique) %>% 
  ## change the name of the complete column to be easier to use
  ## change the name of the unique column so that it doesn't match a function
  filter(!unique.ID %in% brho_NoC, !unique.ID %in% brho_NAC) %>% ## remove incomplete & NA complete phytos
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, inflor.g, seed.num, scale.ID, process.notes, census.notes, unique.ID) ## select the correct columns and make sure order is correct

## Round to 3 digits
brho_temp2 <- brho_temp %>%
  mutate(inflor.g.rounded = round(inflor.g, digits = 3),
         total.biomass.g.rounded = round(total.biomass.g, digits = 3))

round_check <- brho_temp2 %>%
  select(inflor.g, inflor.g.rounded)
## I'm not quite sure how this is rounding, but when a number ends in 5 sometimes it rounds up and sometimes it rounds down. That's odd.
## this function goes to the even digit when rounding from a 5. Not sure if this will be a problem or just a quirk but worth noting

### Make final dataframe ####
drought <- c(1, 3, 4, 6, 12, 14) ## create treatment vector

brho_proc_clean <- brho_temp2 %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C")) %>% ## add a treatment column
  select(-inflor.g, -total.biomass.g)

## Census Data ####
### Needed Tweaks ####
## check for mis-spellings or spaces in phyto names
unique(collections$phyto) ## all good for BRHO; there is a THIR-I with a space

## filter to only BRHO phytos, remove trifolium sub experiment from this, get rid of unused backgrounds and phytos that did not survive
brho_cen <- collections %>%
  filter(phyto == "BRHO", plot < 43, bkgrd != "VIVI", phyto.n.indiv > 0) %>%
  mutate(phyto.unique = ifelse(phyto.unique == "a", "A", ## change all values to caps
                               ifelse(phyto.unique == "b","B", phyto.unique))) %>%
  mutate(unique.ID = unique) %>%
  mutate(Nbrhood.size = 10)

ggplot(brho_cen, aes(x=bkgrd)) +
  geom_bar() ## looks like ERBO is not in this and was filtered out by doing phyto.n.indiv > 0

unique(brho_cen$dens)
unique(brho_cen$phyto)
unique(brho_cen$phyto.date.census) 
unique(brho_cen$phyto.n.indiv)
unique(brho_cen$Nbrhood.size)
## should finish filling in the neighborhood size column
    ## DONE

ggplot(brho_cen, aes(x=bkgrd.n.indiv)) +
  geom_histogram(bins = 50) +
  facet_wrap(~bkgrd)
## some of the backgrounds are NAs, these should be controls. All controls should have NA backgrounds really

bg.na.indiv <- brho_cen %>%
  filter(is.na(bkgrd.n.indiv)) ## yep all controls

## Set all control background # of individuals to NA
brho_cen[brho_cen$bkgrd == "Control",]$bkgrd.n.indiv <- NA

### Check Weeds ####
ggplot(brho_cen, aes(x=CRCO)) +
  geom_histogram(bins = 50) +
  facet_wrap(~block)
  
ggplot(brho_cen, aes(x=ERBO)) +
  geom_histogram(bins = 50, binwidth = 1) +
  facet_wrap(~block)
## There's one 10cm subplot with 8 ERBO? That seems hard to believe that 8 would fit in that area

ggplot(brho_cen, aes(x=FIGA)) +
  geom_histogram(bins = 50) +
  facet_wrap(~block)

ggplot(brho_cen, aes(x=SIGA)) +
  geom_histogram(bins = 50) +
  facet_wrap(~block)

ggplot(brho_cen, aes(x=GAMU)) +
  geom_histogram(bins = 50) +
  facet_wrap(~block)


ggplot(brho_cen, aes(x=HYGL)) +
  geom_histogram(bins = 50) +
  facet_wrap(~block)

ggplot(brho_cen, aes(x = other)) +
  geom_histogram(bins = 50) +
  facet_wrap(~block)



# Merge the two dataframes ####
nrow(brho_proc_clean) ## 427 rows
nrow(brho_cen) ## 438 rows
    ## There are 11 samples that need to be accounted for

brho_unmatched <- anti_join(brho_cen, brho_proc_clean, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique", "unique.ID"))
## double checked this with the data cleaning checks spreadsheet and these are all samples that are incomplete, missing, or the complete column is empty

brho_all <- left_join(brho_proc_clean, brho_cen, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique", "unique.ID"))

ggplot(brho_all, aes(x = phyto.n.indiv.x, y = phyto.n.indiv.y)) +
  geom_point()
## this looks good

brho_clean <- brho_all %>%
  mutate(phyto.n.indiv = phyto.n.indiv.x) %>%
  select(-phyto.n.indiv.y, -unique, -phyto.n.indiv.x)

ggplot(brho_clean, aes(y=inflor.g.rounded, x = treatment)) +
  geom_boxplot() +
  facet_wrap(~bkgrd)

ggplot(brho_clean, aes(y=inflor.g.rounded, x = bkgrd.n.indiv)) +
  geom_point() +
  facet_wrap(~bkgrd)

## clean up environment
rm(list = c("bg.na.indiv", "brho", "brho_all", "brho_cen", "brho_inflor_na", "brho_NAC", "brho_NoC", "brho_proc_clean", "brho_temp2", "collections", "date", "drought", "lead", "unique.check.nas", "brho_complete_check_nas", "brho_complete_check_no", "brho_dens_nas", "brho_temp", "round_check", "brho_unmatched", "date_collections"))
