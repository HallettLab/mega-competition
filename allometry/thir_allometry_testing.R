library(tidyverse)
library(ggpubr)

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}


# Read in Data ####
## Processing data
source("data_cleaning/phyto-processing_data-cleaning/basic-cleaning_all-phytos.R")

thir_dat <- all_dat_final %>%
  filter(phyto == "THIR-I")

## Allometry data
## none yet


# Flower Dat Range ####
ggplot(thir_dat, aes(x=total.biomass.rounded.percap)) +
  geom_histogram() +
  facet_wrap(~treatment)

ggsave("thir_dat_range.png", width = 5, height = 3)
