## Merged GITR QAQC ##

## the purpose of this script is:
## 1. to do species specific QAQC
## 2. clean the census data (clean here rather than earlier so that only clean the necessary rows not everything)


# set up env
library(tidyverse)
theme_set(theme_bw())

# Read in Data ####
source("data_cleaning/initial_data_prep/merge_processing_collections_data.R")

gitr  <- all_dat_final %>%
  filter(phyto == "GITR")


# Clean Census Data ####
## Phyto Change Notes ####
    ## keywords
        ## die, chang, ->

## create empty data frame
df <- data.frame()

## search for "die"
for(i in colnames(gitr)[45:48]) {
  tmp <- dplyr::filter(gitr, grepl("die", gitr[,i]))
  df <- rbind(df, tmp)
}

## search for "chang"
for(i in colnames(gitr)[45:48]) {
  tmp <- dplyr::filter(gitr, grepl("chang", gitr[,i]))
  df <- rbind(df, tmp)
}

df$unique.ID

gitr$intraphyto <- 0 

## add in intraphytos that died and subtract these from the other column where they were originally located.

## unique ID 5115
gitr[gitr$unique.ID == 5115,]$intraphyto <- 1
gitr[gitr$unique.ID == 5115,]$other <- 0

## unique ID 5740
gitr[gitr$unique.ID == 5740,]$intraphyto <- 1
gitr[gitr$unique.ID == 5740,]$other <- 0

## unique ID 11219
gitr[gitr$unique.ID == 11219,]$intraphyto <- 1
gitr[gitr$unique.ID == 11219,]$other <- 1

## unique ID 2605
gitr[gitr$unique.ID == 2605,]$intraphyto <- 1
gitr[gitr$unique.ID == 2605,]$other <- 0


## still need investigation
    ## gitr[gitr$unique.ID == 5015,]
    #gitr[gitr$unique.ID == 4940,]
    #gitr[gitr$unique.ID == 11444,]


# Check Cols for Oddballs ####
## look at total.biomass
ggplot(gitr, aes(x=total.biomass.rounded.percap)) +
  geom_histogram()
## no missing vals

ggplot(gitr, aes(x=bkgrd.n.indiv)) +
  geom_histogram() +
  facet_wrap(~bkgrd)
## 12 missing vals that are likely controls
ggplot(gitr, aes(x=phyto.n.indiv)) +
  geom_histogram()

ggplot(gitr, aes(x=CRCO)) +
  geom_histogram()
ggplot(gitr, aes(x=ERBO)) + ## there's one with 8 ERBO?
  geom_histogram()
ggplot(gitr, aes(x=FIGA)) +
  geom_histogram()
ggplot(gitr, aes(x=GAMU)) +
  geom_histogram()
ggplot(gitr, aes(x=HYGL)) +
  geom_histogram()
ggplot(gitr, aes(x=SIGA)) +
  geom_histogram()
ggplot(gitr, aes(x=other)) +
  geom_histogram()

