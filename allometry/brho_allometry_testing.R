## BRHO Allometry Testing
## updated 9/9/2022

## load packages
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
## read in cleaned processing/censusing data
source("data_cleaning/clean_collections_merge_processing.R")

colnames(mc_dat)
  

## file paths for allometry data
lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/" # Carmen's file path
date <- 20220901

# Download allometry data from google drive ####
#drive_download(
 #file = "https://docs.google.com/spreadsheets/d/12Dq75VjwsawdOVdz7ReDnezylefZMXsW164MzJoV3zg/edit#gid=282097641",
  #path = paste0(lead, "Allometry/Allometry_entered/", date, "_Allometry"),
# type = "xlsx",
# overwrite = FALSE
#)

## Upload allometry data from dropbox 
brho_allo <- read.xlsx(paste0(lead, "Allometry/Allometry_entered/", date, "_Allometry.xlsx"), sheet = 8) %>%
  mutate(inflor.g = inflorescence.weight.g, seed.num = seeds.num) %>% ## standardize column names
  mutate(treatment = ifelse(treatment == "ambient", "C", "D")) ## standardize treatment values


# Visually Explore data ####
theme_set(theme_bw())

## Allometric Relationship ####
ggplot(brho_allo, aes(x=inflor.g, y=seed.num, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(brho_allo, aes(x=inflor.g, y=seed.num, color = treatment)) +
  geom_point()


## Compare Distribs ####
brhodphytos <- ggplot(brho_clean[brho_clean$treatment == "D",], aes(x=percap.inflor.g.rounded)) +
  geom_histogram() +
  coord_cartesian(xlim = c(0,2), ylim = c(0,75)) +
  ggtitle("BRHO All Samples - Drought") +
  xlab("BRHO per-cap inflor biomass (g)")

brhodallo <- ggplot(brho_allo[brho_allo$treatment == "D",], aes(x=inflor.g)) +
  geom_histogram() +
  coord_cartesian(xlim = c(0,2), ylim = c(0,7)) +
  ggtitle("BRHO Allometry Samples - Drought") +
  xlab("BRHO per-cap inflor biomass (g)")

brhocphytos <- ggplot(brho_clean[brho_clean$treatment == "C",], aes(x=percap.inflor.g.rounded)) +
  geom_histogram() +
  coord_cartesian(xlim = c(0,2), ylim = c(0,75)) +
  ggtitle("BRHO All Samples - Control") +
  xlab("BRHO per-cap inflor biomass (g)")

brhocallo <- ggplot(brho_allo[brho_allo$treatment == "C",], aes(x=inflor.g)) +
  geom_histogram() +
  coord_cartesian(xlim = c(0,2), ylim = c(0,7)) +
  ggtitle("BRHO Allometry Samples - Control") +
  xlab("BRHO per-cap inflor biomass (g)")

ggarrange(brhocphytos, brhodphytos, brhocallo, brhodallo, 
          nrow = 2, ncol = 2)
ggsave("allometry/preliminary_figs/allometric_relationship_fits/brho_allo_distributions.png", height = 5, width = 7)


# Predict Seed Num ####
## Make separate allometric relationships for drought and control

## Drought
brho_allo_D <- lm(seed.num ~ inflor.g, data = brho_allo[brho_allo$treatment == "D",])
summary(brho_allo_D)
## slope = 1014.04
## intercept = -9.88; does it make sense to force the intercept to 0? This might make the model fit worse even if it is biologically correct. Although since we are not predicting the seed output on something that is 0, we shouldn't have any issues in using a model w/ an intercept of -9

## Control
brho_allo_C <- lm(seed.num ~ inflor.g, data = brho_allo[brho_allo$treatment == "C",])
summary(brho_allo_C)
## slope = 922.44

## These slopes are pretty different, so for the time being use drought and ambient values separately

brho_seeds <- brho_clean %>%
  mutate(percap.seed.num.predicted = ifelse(treatment == "C", percap.inflor.g.rounded*922.14, percap.inflor.g.rounded*1014.04))


## Explore Seed Output data ####
brho_seeds_summary <- brho_seeds %>%
  group_by(treatment) %>%
  summarise(mean.percap.seeds = mean(percap.pred.seed.num, na.rm = T),
            se.percap.seeds = calcSE(percap.pred.seed.num))

ggplot(brho_seeds_summary, aes(x=treatment, y=mean.percap.seeds, color = treatment)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#699FA1", "#DD8627")) +
  geom_errorbar(aes(ymin = mean.percap.seeds - se.percap.seeds, ymax = mean.percap.seeds + se.percap.seeds), width = 0.25) +
  xlab("Treatment") +
  ylab("BRHO mean per capita seed #")

ggsave("brho_seeds_tx.png", height = 3.5, width = 4)

brho_seeds_bgs <- brho_seeds %>%
  group_by(treatment, bkgrd) %>%
  summarise(mean.percap.seeds = mean(percap.pred.seed.num, na.rm = T),
            se.percap.seeds = calcSE(percap.pred.seed.num))

ggplot(brho_seeds_bgs, aes(x=treatment, y=mean.percap.seeds, color = treatment)) +
  geom_point(size = 1.5) +
  scale_color_manual(values = c("#699FA1", "#DD8627")) +
  geom_errorbar(aes(ymin = mean.percap.seeds - se.percap.seeds, ymax = mean.percap.seeds + se.percap.seeds), width = 0.15) +
  xlab("Treatment") +
  ylab("BRHO mean per capita seed #") +
  facet_wrap(~bkgrd)


ggplot(brho_seeds_bgs, aes(x=bkgrd, y=mean.percap.seeds, color = treatment)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("#008083", "#FAAB36")) +
  geom_errorbar(aes(ymin = mean.percap.seeds - se.percap.seeds, ymax = mean.percap.seeds + se.percap.seeds), width = 0.25) +
  xlab("Background Species") +
  ylab("BRHO mean per capita seed #") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("brho_seeds_bg.png", height = 5, width = 7)

ggplot(brho_seeds, aes(x=bkgrd.n.indiv, y=percap.pred.seed.num, color = treatment)) +
  geom_point() +
  facet_wrap(~bkgrd)

ggplot(brho_seeds, aes(x=seed.num, y= seed.num.predicted, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25, size = 0.75) +
  geom_abline(slope = 1, intercept = 0)


# Test Allo Relationship ####

## Look at the test data ####
ggplot(brho_seeds, aes(x=percap.seed.num, y=percap.seed.num.predicted, color = treatment)) +
  scale_color_manual(values = c("#008083", "#FAAB36")) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0)
## visually, the drought points seem to fit closer to the 1:1 line than the control points


