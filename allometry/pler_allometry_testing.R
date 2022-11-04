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
#pler_allo <- read.xlsx(paste0(allo_lead, "20220901_Allometry.xlsx"), sheet = 18) %>%
#  mutate(treatment = ifelse(block %in% drought, "D", "C"))

pler_allo <- read.csv(paste0(allo_lead, "PLER_allometry-processing_20221101.csv"))
  

# Dat Range ####
theme_set(theme_bw())

pler_dat <- all_dat_final %>%
  filter(phyto == "PLER")

phyto<-ggplot(pler_dat, aes(x=inflor.g.rounded.percap)) +
  geom_histogram() +
  facet_wrap(~treatment) #+
  #scale_x_continuous(breaks=seq(0,2.5,by=0.25))

allo<-ggplot(pler_allo, aes(x=inflor.g)) +
  geom_histogram() +
  facet_wrap(~treatment) +
  coord_cartesian(xlim = c(0,2.5)) #+
  #scale_x_continuous(breaks=seq(0,2.5,by=0.25))


ggarrange(phyto, allo, ncol = 1, nrow=2)

ggsave("allometry/preliminary_figs/allometric_relationship_fits/pler_allometry_check.png", height = 4, width = 6)

