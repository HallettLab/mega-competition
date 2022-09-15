# LENI Data Cleaning 

library(googlesheets4)
library(plyr)
library(tidyverse)
library(googledrive)
library(openxlsx)

# Read in Data ####
lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/" # Carmen's file path
date <- 20220825


leni <- read.xlsx(paste0(lead, "Phytometer-Processing/", date, "_Phyto-Processing.xlsx"), sheet = 10)

## Collections data
collections <- read.xlsx(paste0(lead, "Collections/Collections_merged/", date, "_MASTER_Collections_2-in-progress.xlsx"), sheet = 2)

## make sure the dates read in correctly
collections$phyto.date.collect <- as.Date(collections$phyto.date.collect, origin = "1899-12-30")
collections$phyto.date.census <- as.Date(collections$phyto.date.census, origin = "1899-12-30")
collections$bg.date.census <- as.Date(collections$bg.date.census, origin = "1899-12-30")
collections$phyto.unique <- as.character(collections$phyto.unique)

# Data Cleaning ####
## Processing Data ####
### Check ID Info ####
colnames(leni)
## block column doesn't have a name
colnames(leni)[colnames(leni) == ""] <- "block" ## fixed that

## unique column is not filled
## pod.num should maybe be changed to flower.num

unique(leni$bkgrd)
## ERBO, VIVI backgrounds in this still
## Some ERBOs might be used as control backgrounds, so remove carefully

unique(leni$dens) ## some NAs here, check if they are controls

leni_dens_nas <- leni %>%
  filter(is.na(dens)) 
## yep, all controls

unique(leni$phyto) ## all LENI, good.
unique(leni$phyto.unique) ## need to standardize capitalization!

### Check Sample Completion ####
ggplot(leni, aes(x=`complete?`)) +
  geom_bar()

unique(leni$`complete?`) ## capitalization is good, need to look into NAs & nos

## check the NAs
leni_complete_nas_check <- leni %>%
  filter(is.na(`complete?`))
    ## one NA and that is the missing sample 5-1-25

leni_complete_no_check <- leni %>%
  filter(`complete?` == "N")
    ## two samples marked incomplete, should be removed and eventually double checked


### Check Measured Info ####

unique(leni$phyto.n.indiv) ## 1-3, good

unique(leni$total.stem.length.mm)

ggplot(leni, aes(x=total.stem.length.mm)) +
  geom_histogram(bins = 200)
ggplot(leni, aes(y=total.stem.length.mm)) +
  geom_boxplot()

ggplot(leni, aes(x=total.biomass)) + ## this column should be renamed
  geom_histogram()

ggplot(leni, aes(x=seed.num)) +
  geom_histogram()
ggplot(leni, aes(x=pod.num)) + ## change this column name
  geom_histogram()


unique(leni$scale.ID) ## will need to round weights

unique(leni$process.notes)
unique(leni$unique) ## change column name
## All NAs, make sure to add these data in from the census data.


## Make all Mods ####
leni_temp <- leni %>%
  mutate(phyto.unique = ifelse(phyto.unique == "a", "A", ## change all phyto.unique values to caps
                             ifelse(phyto.unique == "b", "B", phyto.unique))) %>%
  mutate(total.biomass.g = total.biomass, unique.ID = unique, complete.sample = `complete?`) %>% ## rename columns to standardize
  filter(complete.sample == "Y") %>% ## remove the incomplete/unknown completeness samples
  filter(bkgrd != "VIVI") %>% ## remove VIVI backgrounds, keep ERBO as these can be used as controls
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.stem.length.mm, total.biomass.g, pod.num, seed.num, scale.ID, process.notes, census.notes, unique.ID)

## Round to 3 digits
leni_temp2 <- leni_temp %>%
  mutate(total.biomass.g.rounded = round(total.biomass.g, digits = 3))
 
### Make final dataframe ####
drought <- c(1, 3, 4, 6, 12, 14) ## create treatment vector

leni_proc_clean <- leni_temp2 %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C")) %>% ## add a treatment column
  select(-total.biomass.g)


## Census Data ####

## check for mis-spellings or spaces in phyto names
unique(collections$phyto) ## all good

## filter to only LENI phytos, get rid of unused backgrounds and phytos that did not survive
leni_cen <- collections %>%
  filter(phyto == "LENI", bkgrd != "VIVI", phyto.n.indiv > 0) %>%
  mutate(phyto.unique = ifelse(phyto.unique == "a", "A", ## change all values to caps
                               ifelse(phyto.unique == "b","B", phyto.unique))) %>%
  mutate(unique.ID = unique) %>%
  mutate(Nbrhood.size = 18)

unique(leni_cen$dens)
unique(leni_cen$phyto)
unique(leni_cen$phyto.date.census) 
unique(leni_cen$phyto.n.indiv)
unique(leni_cen$Nbrhood.size)
## should finish filling in the neighborhood size column
## DONE

ggplot(leni_cen, aes(x=bkgrd.n.indiv)) +
  geom_histogram(bins = 50) +
  facet_wrap(~bkgrd)
## some of the backgrounds are NAs, these should be controls. All controls should have NA backgrounds really

bg.na.indiv <- leni_cen %>%
  filter(is.na(bkgrd.n.indiv)) ## yep all controls

## Set all control background # of individuals to NA
leni_cen[leni_cen$bkgrd == "Control",]$bkgrd.n.indiv <- NA

ggplot(leni_cen, aes(x=CRCO)) +
  geom_histogram(bins = 50) +
  facet_wrap(~block)

ggplot(leni_cen, aes(x=ERBO)) +
  geom_histogram(bins = 50, binwidth = 1) +
  facet_wrap(~block)

ggplot(leni_cen, aes(x=FIGA)) +
  geom_histogram(bins = 50) +
  facet_wrap(~block)

ggplot(leni_cen, aes(x=SIGA)) +
  geom_histogram(bins = 50) +
  facet_wrap(~block)

ggplot(leni_cen, aes(x=GAMU)) +
  geom_histogram(bins = 50) +
  facet_wrap(~block)
## there's one very high GAMU census in block 7 (50 individuals) - this block was very dense with GAMU, but that number is quite high compared with the rest

ggplot(leni_cen, aes(x=HYGL)) +
  geom_histogram(bins = 50) +
  facet_wrap(~block)

ggplot(leni_cen, aes(x = other)) +
  geom_histogram(bins = 50) +
  facet_wrap(~block)
## one with 30 others... quite a lot but I guess not totally out of the question?


# Merge the two dataframes ####
leni_all <- left_join(leni_proc_clean, leni_cen, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))

ggplot(leni_all, aes(x = phyto.n.indiv.x, y = phyto.n.indiv.y)) +
  geom_point()
## this looks good

leni_clean <- leni_all %>%
  mutate(phyto.n.indiv = phyto.n.indiv.x,
         unique.ID = unique.ID.y) %>%
  select(-phyto.n.indiv.y, -unique, -phyto.n.indiv.x, -unique.ID.y, -unique.ID.x)

ggplot(leni_clean, aes(y=total.stem.length.mm, x = treatment)) +
  geom_boxplot() +
  facet_wrap(~bkgrd)

ggplot(leni_clean, aes(y=total.stem.length.mm, x = bkgrd.n.indiv)) +
  geom_point() +
  facet_wrap(~bkgrd)

## clean up environment
rm(list = c("bg.na.indiv", "collections", "date", "drought", "lead", "leni", "leni_all", "leni_cen", "leni_complete_nas_check", "leni_complete_no_check", "leni_controls", "leni_dens_nas", "leni_proc_clean", "leni_temp", "leni_temp2"))


