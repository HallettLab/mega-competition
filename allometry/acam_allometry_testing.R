library(tidyverse)
library(ggpubr)
library(openxlsx)

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}


# Read in Data ####
## Processing data ####
source("data_cleaning/initial_data_prep/merge_processing_collections_data.R")

acam_dat <- all_dat_final %>%
  filter(phyto == "ACAM")

## Allometry data ####

# specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Allometry/Allometry_entered/")){
  # Carmen
  allo_lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Allometry/Allometry_entered/"
  
} else {
  # Marina
  allo_lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Allometry/Allometry_entered/"
} 

acam_allo <- read.xlsx(paste0(allo_lead, "20221113_Allometry-Processing.xlsx"), sheet = 1) %>%
  filter(!is.na(flower.num))


# Flower Dat Range ####
theme_set(theme_bw())

phyto<-ggplot(acam_dat, aes(x=total.biomass.rounded.percap)) +
  geom_histogram() +
  facet_wrap(~treatment) +
  coord_cartesian(xlim = c(0,20))

allo<-ggplot(acam_allo, aes(x=total.biomass.g)) +
  geom_histogram() +
  facet_wrap(~treatment) +
  coord_cartesian(xlim = c(0,20))


ggarrange(phyto, allo, ncol = 1, nrow=2)

ggsave("allometry/preliminary_figs/allometric_relationship_fits/acam_allometry_check.png", height = 4, width = 6)
