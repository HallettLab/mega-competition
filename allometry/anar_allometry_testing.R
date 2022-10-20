library(tidyverse)
library(ggpubr)

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}


# Read in Data ####
## Processing data
source("data_cleaning/merge_processing_collections_data.R")

anar_dat <- all_dat_final %>%
  filter(phyto == "ANAR")

## Allometry data
## none yet


# Flower Dat Range ####
ggplot(anar_dat, aes(x=total.biomass.rounded.percap)) +
  geom_histogram() +
  facet_wrap(~treatment)

ggsave("anar_dat_range.png", width = 5, height = 3)
