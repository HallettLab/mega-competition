## Merged GITR QAQC ##
library(tidyverse)

# Read in Data ####
rm(list=ls())
source("data_cleaning/merge_processing_collections_data.R")

gitr  <- all_dat_final %>%
  filter(phyto == "GITR")

# Filter for Phyto Change Notes ####
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


