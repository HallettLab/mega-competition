library(tidyverse)
library(ggpubr)
library(openxlsx)


## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}


# Read in Data ####
## Processing data
source("data_cleaning/initial_data_prep/merge_processing_collections_data.R")

anar_dat <- all_dat_final %>%
  filter(phyto == "ANAR")

## Allometry data
# specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Allometry/Allometry_entered/")){
  # Carmen
  allo_lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Allometry/Allometry_entered/"
  
} else {
  # Marina
  allo_lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Allometry/Allometry_entered/"
} 


anar_allo <- read.xlsx(paste0(allo_lead, "20221113_Allometry-Processing.xlsx"), sheet = 5) %>%
  filter(!is.na(flower.num))

anar_seeds <- read.xlsx(paste0(allo_lead, "20221113_Allometry-Processing.xlsx"), sheet = 6)

# Flower Dat Range ####
theme_set(theme_bw())

phyto<-ggplot(anar_dat, aes(x=total.biomass.rounded.percap)) +
  geom_histogram() +
  facet_wrap(~treatment) +
  coord_cartesian(xlim = c(0,6))

allo<-ggplot(anar_allo, aes(x=total.biomass.g)) +
  geom_histogram() +
  facet_wrap(~treatment) +
  coord_cartesian(xlim = c(0,6))


ggarrange(phyto, allo, ncol = 1, nrow=2)

ggsave("allometry/preliminary_figs/allometric_relationship_fits/anar_allometry_check.png", height = 4, width = 6)
