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
source("data_cleaning/phyto-processing_data-cleaning/basic-cleaning_all-phytos.R")

#acam_dat <- all_dat_final %>%
 # filter(phyto == "ACAM")

## Allometry data ####

# specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Allometry/Allometry_entered/")){
  # Carmen
  allo_lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Allometry/Allometry_entered/"
  
} else {
  # Marina
  allo_lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Allometry/Allometry_entered/"
} 

acam_allo <- read.csv(paste0(allo_lead, "Allometry-Processing_ACAM-flowers_20221202.csv")) %>%
  filter(!is.na(flower.num))

acamC2 <- acamC %>%
  mutate(total.biomass.g.percap = total.biomass.g/phyto.n.indiv)


# Flower Dat Range ####
theme_set(theme_bw())

phyto<-ggplot(acamC2, aes(x=total.biomass.g.percap)) +
  geom_histogram() +
  facet_wrap(~treatment) +
  coord_cartesian(xlim = c(0,20))

allo<-ggplot(acam_allo, aes(x=total.biomass.g)) +
  geom_histogram() +
  facet_wrap(~treatment) +
  coord_cartesian(xlim = c(0,20))


ggarrange(phyto, allo, ncol = 1, nrow=2)

ggsave("allometry/preliminary_figs/allometric_relationship_fits/acam_allometry_check.png", height = 4, width = 6)


ggplot(acam_allo, aes(x=total.biomass.g, y=flower.num, color = treatment)) +
  geom_point() +
  geom_smooth(method = lm)





