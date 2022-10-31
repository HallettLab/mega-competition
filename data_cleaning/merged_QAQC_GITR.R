library(tidyverse)

source("data_cleaning/merge_processing_collections_data.R")

gitr  <- all_dat_final %>%
  filter(phyto == "GITR")

## need to look through notes.... 

colnames(gitr)

## can we filter by something that contains text vs. is blank?
## phyto died before collection that's not yet added to other

unique(gitr$pheno.notes)
unique(gitr$collect.notes)
unique(gitr$background.notes)
unique(gitr$notes)

## keywords
    ## die, chang*, ->

gitr_temp <- gitr %>%
  filter(grepl("WUE",collect.notes))


filter(gitr, grepl("change",notes))
## unique IDs that def need a change: 2605
## unique IDs that maybe need a change: 5015, 5115, 4940, 11444
filter(gitr, grepl("die",notes))

filter(gitr, grepl("die", background.notes))
## phytos that have the change in other: 5740, 5115

df <- data.frame()
for(i in colnames(gitr)[45:48]) {
  tmp <- dplyr::filter(gitr, grepl("die", gitr[,i]))
  df <- rbind(df, tmp)
}

#df.change <- data.frame()
for(i in colnames(gitr)[45:48]) {
  tmp <- dplyr::filter(gitr, grepl("chang", gitr[,i]))
  df <- rbind(df, tmp)
}

df$unique.ID



gitr2 <- gitr %>%
  mutate(intra.phyto = NA)








