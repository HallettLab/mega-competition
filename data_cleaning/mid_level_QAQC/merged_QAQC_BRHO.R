## Merged BRHO QAQC ##

## the purpose of this script is:
    ## 1. to do species specific QAQC
    ## 2. clean the census data (clean here rather than earlier so that only clean the necessary rows not everything)


# set up env
library(tidyverse)
theme_set(theme_bw())

# Read in Data ####
source("data_cleaning/initial_data_prep/merge_processing_collections_data.R")

brho  <- all_dat_final %>%
  filter(phyto == "BRHO")

# Clean Census Data ####
## Phyto Change Notes ####
    ## keywords
        ## die, chang

## create empty data frame
df <- data.frame()

## loop through all notes searching for "die" or "chang"
for(i in colnames(brho)[45:48]) {
  tmp <- dplyr::filter(brho, grepl("die", brho[,i]))
  df <- rbind(df, tmp)
}

for(i in colnames(brho)[45:48]) {
  tmp <- dplyr::filter(brho, grepl("chang", brho[,i]))
  df <- rbind(df, tmp)
}

## add an empty intraphyto column to  brho df
    ## if there were intraphytos that died before collection they would go in this column
brho$intraphyto <- 0 

## Check Cols for Oddballs ####
## look at infor.g
ggplot(brho, aes(x=inflor.g)) +
  geom_histogram()
## no missing vals

ggplot(brho, aes(x=bkgrd.n.indiv)) +
  geom_histogram() +
  facet_wrap(~bkgrd)
## 12 missing vals that are likely controls
ggplot(brho, aes(x=phyto.n.indiv)) +
  geom_histogram()

ggplot(brho, aes(x=CRCO)) +
  geom_histogram()
ggplot(brho, aes(x=ERBO)) + ## there's one with 8 ERBO?
  geom_histogram()
ggplot(brho, aes(x=FIGA)) +
  geom_histogram()
ggplot(brho, aes(x=GAMU)) +
  geom_histogram()
ggplot(brho, aes(x=HYGL)) +
  geom_histogram()
ggplot(brho, aes(x=SIGA)) +
  geom_histogram()
ggplot(brho, aes(x=other)) +
  geom_histogram()

# Q here ####
## are we modeling by per capita or not?
## are we doing anything to the other column?