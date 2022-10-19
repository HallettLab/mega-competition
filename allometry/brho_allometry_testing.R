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

allo_lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Allometry/Allometry_entered/" # Carmen's file path
date <- 20221019

## Allometry data
brho_allo <- read.csv(paste0(allo_lead, "BRHO_allometry-processing_", date, ".csv")) %>%
  mutate(inflor.g = inflorescence.weight.g, seed.num = seeds.num) %>% ## standardize column names
  mutate(treatment = ifelse(treatment == "ambient", "C", "D")) ## standardize treatment values


# Dat Range ####
theme_set(theme_bw())

brho_dat <- all_dat_final %>%
  filter(phyto == "BRHO")

phyto <- ggplot(brho_dat, aes(x=inflor.g.rounded.percap)) +
  geom_histogram() +
  facet_wrap(~treatment)+
  coord_cartesian(xlim = c(0,2))

allo <- ggplot(brho_allo, aes(x=inflor.g)) +
  geom_histogram() +
  facet_wrap(~treatment) +
  coord_cartesian(xlim = c(0,2))

ggarrange(phyto, allo, ncol = 1, nrow=2)

#ggsave("allometry/preliminary_figs/allometric_relationship_fits/brho_allometry_check.png", height = 4, width = 6)


## To-Do ####
  ## Looks like BRHO control could use a few more samples in the lower range.


# Inflor-Seed Rel. ####
ggplot(brho_allo, aes(x=inflor.g, y=seed.num, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm")
## the error bars on the lines overlap for most of it - so probably not separate relationships here

ggplot(brho_allo, aes(x=inflor.g, y=seed.num)) +
  geom_point() +
  geom_smooth(method = "lm")

inflorseeds <- lm(seed.num~inflor.g, data = brho_allo)
summary(inflorseeds)
## slope = 951.7297

## Q here ####
    ## how do we test other models?



# Predict Seed Num ####







