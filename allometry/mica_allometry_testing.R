library(googlesheets4)
library(plyr)
library(tidyverse)
library(googledrive)
library(openxlsx)
library(ggpubr)

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

# Read in Data ####
## Processing data
source("data_cleaning/merge_processing_collections_data.R")

allo_lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Allometry/Allometry_entered/" # Carmen's file path

## Allometry data
drought <- c(1, 3, 4, 6, 12, 14) ## create treatment vector
mica_allo <- read.xlsx(paste0(allo_lead, "20220901_Allometry.xlsx"), sheet = 17) %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C"),
         seed.num = seeds.num)


# Dat Range ####
theme_set(theme_bw())

mica_dat <- all_dat_final %>%
  filter(phyto == "MICA")

phyto<-ggplot(mica_dat, aes(x=total.biomass.rounded.percap)) +
  geom_histogram() +
  facet_wrap(~treatment)

allo<-ggplot(mica_allo, aes(x=total.biomass.g)) +
  geom_histogram() +
  facet_wrap(~treatment) +
  coord_cartesian(xlim = c(0,2))


ggarrange(phyto, allo, ncol = 1, nrow=2)

ggsave("allometry/preliminary_figs/allometric_relationship_fits/mica_allometry_check.png", height = 4, width = 6)









