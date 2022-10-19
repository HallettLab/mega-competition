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
source("data_cleaning/merge_processing_collections_data.R")

allo_lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Allometry/Allometry_entered/" # Carmen's file path
#date <- 20221019

## Allometry data
#leni_allo <- read.csv(paste0(allo_lead, "LENI_allometry-processing_", date, ".csv")) ## use when relationship is more finalized

drought <- c(1, 3, 4, 6, 12, 14) ## create treatment vector
leni_allo <- read.xlsx(paste0(allo_lead, "20220901_Allometry.xlsx"), sheet = 14) %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C"))


# Dat Range ####
theme_set(theme_bw())

leni_dat <- all_dat_final %>%
  filter(phyto == "LENI")

phyto<-ggplot(leni_dat, aes(x=total.stem.length.mm.percap)) +
  geom_histogram() +
  facet_wrap(~treatment)

allo<-ggplot(leni_allo, aes(x=total.stem.length.mm)) +
  geom_histogram() +
  facet_wrap(~treatment) +
  coord_cartesian(xlim = c(0,6000))


ggarrange(phyto, allo, ncol = 1, nrow=2)

ggsave("allometry/preliminary_figs/allometric_relationship_fits/leni_allometry_check.png", height = 4, width = 6)

## To-Do ####
    ## LENI relationship needs filling out everywhere, missing med-high values particularly














