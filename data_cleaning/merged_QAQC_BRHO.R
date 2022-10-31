## Merged BRHO QAQC ##
library(tidyverse)

# Read in Data ####
rm(list=ls())
source("data_cleaning/merge_processing_collections_data.R")

brho  <- all_dat_final %>%
  filter(phyto == "BRHO")

# Filter for Phyto Change Notes ####
## keywords
## die, chang, ->

## create empty data frame
df <- data.frame()

## search for "die"
df <- data.frame()

for(i in colnames(brho)[45:48]) {
  tmp <- dplyr::filter(brho, grepl("die", brho[,i]))
  df <- rbind(df, tmp)
}

for(i in colnames(brho)[45:48]) {
  tmp <- dplyr::filter(brho, grepl("chang", brho[,i]))
  df <- rbind(df, tmp)
}

brho$intraphyto <- 0 

