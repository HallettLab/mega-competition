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
source("data_cleaning/mica_data-cleaning.R")

## put variables in terms of per-capita
mica_clean <- mica_clean %>%
  mutate(percap.total.biomass.g.rounded = total.biomass.g.rounded/phyto.n.indiv,
         percap.seed.num = seed.num/phyto.n.indiv)

lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/" # Carmen's file path
date <- 20220901

# Upload data from dropbox ####
## Allometry data
drought <- c(1, 3, 4, 6, 12, 14) ## create treatment vector

mica_allo <- read.xlsx(paste0(lead, "Allometry/Allometry_entered/", date, "_Allometry.xlsx"), sheet = 17) %>%
  mutate(seed.num = seeds.num,
         treatment = ifelse(block %in% drought, "D", "C"))


# Explore data ####
theme_set(theme_bw())

## Vis Allo Relationship ####
ggplot(mica_allo, aes(x=seed.num, y=total.biomass.g, color = treatment)) +
  geom_point() +
  scale_color_manual(values = c("#699FA1", "#DD8627")) +
  geom_smooth(method = "lm")
## relationship looks okay to use preliminarily
## keep drought and ambient separate for the time being

## Compare Distribs ####
micadphytos <- ggplot(mica_clean[mica_clean$treatment == "D",], aes(x=percap.total.biomass.g.rounded)) +
  geom_histogram() +
  coord_cartesian(xlim = c(0,2), ylim = c(0,60)) +
  ggtitle("MICA All Samples - Drought") +
  xlab("MICA per cap total biomass (g)")

micadallo <- ggplot(mica_allo[mica_allo$treatment == "D",], aes(x=total.biomass.g)) +
  geom_histogram() +
  coord_cartesian(xlim = c(0,2), ylim = c(0,5)) +
  ggtitle("MICA Allometry Samples - Drought") +
  xlab("MICA per cap total biomass (g)")

micacphytos <- ggplot(mica_clean[mica_clean$treatment == "C",], aes(x=percap.total.biomass.g.rounded)) +
  geom_histogram() +
  coord_cartesian(xlim = c(0,2), ylim = c(0,60)) +
  ggtitle("MICA All Samples - Control") +
  xlab("MICA per cap total biomass (g)")

micacallo <- ggplot(mica_allo[mica_allo$treatment == "C",], aes(x=total.biomass.g)) +
  geom_histogram() +
  coord_cartesian(xlim = c(0,2), ylim = c(0,5)) +
  ggtitle("MICA Allometry Samples - Control") +
  xlab("MICA per cap total biomass (g)")

ggarrange(micacphytos, micadphytos, micacallo, micadallo, 
          nrow = 2, ncol = 2)
ggsave("allometry/preliminary_figs/allometric_relationship_fits/mica_allo_distributions.png", height = 5, width = 7)




# Predict Seed Num ####
mica_allo_D <- lm(seed.num ~ total.biomass.g, data = mica_allo[mica_allo$treatment == "D",])
summary(mica_allo_D)
## slope = 791.41

mica_allo_C <- lm(seed.num ~ total.biomass.g, data = mica_allo[mica_allo$treatment == "C",])
summary(mica_allo_C)
## slope = 699.68

mica_seeds <- mica_clean %>%
  mutate(seed.num.predicted = ifelse(treatment == "C", total.biomass.g.rounded*699.68, total.biomass.g.rounded*791.41)) %>%
  mutate(percap.pred.seed.num = seed.num.predicted/phyto.n.indiv)

ggplot(mica_seeds, aes(x=seed.num.predicted, y=seed.num)) +
  geom_point()
## there is one weird outlier here... mostly okay other than this.

ggplot(mica_seeds, aes(x = percap.pred.seed.num)) +
  geom_histogram()

mica_seeds_summary <- mica_seeds %>%
  group_by(treatment) %>%
  summarise(mean.percap.seeds = mean(percap.pred.seed.num, na.rm = T),
            se.percap.seeds = calcSE(percap.pred.seed.num))

ggplot(mica_seeds_summary, aes(x=treatment, y=mean.percap.seeds, color = treatment)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#699FA1", "#DD8627")) +
  geom_errorbar(aes(ymin = mean.percap.seeds - se.percap.seeds, ymax = mean.percap.seeds + se.percap.seeds), width = 0.25) +
  xlab("Treatment") +
  ylab("MICA mean per capita seed #")


mica_seeds_bgs <- mica_seeds %>%
  group_by(treatment, bkgrd) %>%
  summarise(mean.percap.seeds = mean(percap.pred.seed.num, na.rm = T),
            se.percap.seeds = calcSE(percap.pred.seed.num))

ggplot(mica_seeds_bgs, aes(x=bkgrd, y=mean.percap.seeds, color = treatment)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("#699FA1", "#DD8627")) +
  geom_errorbar(aes(ymin = mean.percap.seeds - se.percap.seeds, ymax = mean.percap.seeds + se.percap.seeds), width = 0.25) +
  xlab("Background Species") +
  ylab("MICA mean per capita seed #") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


# Check the allo relationship ####
range(mica_clean$total.biomass.g.rounded)
    ## 0.001 - 3.555
range(mica_allo$total.biomass.g)
    ## 0.0169 - 0.6701
## These ranges are not well aligned at all, need to ensure allo samples cover a better range


mean(mica_clean[mica_clean$treatment == "C",]$total.biomass.g.rounded)
mean(mica_clean[mica_clean$treatment == "D",]$total.biomass.g.rounded)
mean(mica_allo[mica_allo$treatment == "C",]$total.biomass.g)
mean(mica_allo[mica_allo$treatment == "D",]$total.biomass.g)
## the mean is quite a bit smaller in the allo samples than in the actual data. Particularly in drought samples. Odd


calcSE(mica_clean[mica_clean$treatment == "C",]$total.biomass.g.rounded)
calcSE(mica_allo[mica_allo$treatment == "C",]$total.biomass.g)

calcSE(mica_clean[mica_clean$treatment == "D",]$total.biomass.g.rounded)
calcSE(mica_allo[mica_allo$treatment == "D",]$total.biomass.g)



var(mica_clean[mica_clean$treatment == "C",]$total.biomass.g.rounded)
var(mica_allo[mica_allo$treatment == "C",]$total.biomass.g)

var(mica_clean[mica_clean$treatment == "D",]$total.biomass.g.rounded)
var(mica_allo[mica_allo$treatment == "D",]$total.biomass.g)



