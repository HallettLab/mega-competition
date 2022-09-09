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
source("data_cleaning/gitr_data-cleaning.R")

lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/" # Carmen's file path
date <- 20220901

# Upload data from dropbox ####
## Allometry data
gitr_flower_allo <- read.xlsx(paste0(lead, "Allometry/Allometry_entered/", date, "_Allometry.xlsx"), sheet = 12)
gitr_seeds_allo <- read.xlsx(paste0(lead, "Allometry/Allometry_entered/", date, "_Allometry.xlsx"), sheet = 13)

str(gitr_flower_allo)
str(gitr_seeds_allo)

# Calc avg seeds/flower ####
colnames(gitr_seeds_allo)
gitr_seeds_allo <- gitr_seeds_allo %>%
  select(1:7) %>%
  mutate(block = Block, species = Species, rep = Rep, notes = Notes) %>%
  select(rep, block, species, pod.num, seed.num, seed.per.pod, notes)

ggplot(gitr_seeds_allo, aes(x=seed.per.pod)) +
  geom_histogram()

gitr_mean_seeds <- mean(gitr_seeds_allo$seed.per.pod, na.rm = T)
gitr_mean_seeds ## 10.55 


## should do this for drought and ambient

drought <- c(1, 3, 4, 6, 12, 14) ## create treatment vectors

gitr_sallo <- gitr_seeds_allo %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C"))

ggplot(gitr_sallo, aes(x=seed.per.pod, fill = treatment)) +
  geom_histogram()

nrow(gitr_sallo[gitr_sallo$treatment == "D",]) ## 19 samples of drought seed pods
nrow(gitr_sallo[gitr_sallo$treatment == "C",]) ## 33 samples of ambient seed pods

## these look different enough that they should be considered separately though!!

gitr_seed_means <- gitr_sallo %>%
  group_by(treatment) %>%
  summarise(mean_seeds = mean(seed.per.pod, na.rm = T))


## should we be doing these relationships based on per-capita biomass and flower numbers?


# Calc Flower # from TotBio ####
gitr_flower_allo <- gitr_flower_allo %>%
  filter(!is.na(total.biomass.g)) %>% ## seems like several blank rows at the end got added on, removing these here
  mutate(treatment = ifelse(block %in% drought, "D", "C"))

ggplot(gitr_flower_allo, aes(x=total.biomass.g, y=flower.num, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25, size = 0.75) +
  geom_abline(slope = 1, intercept = 0)
ggplot(gitr_flower_allo, aes(x=total.biomass.g, fill = treatment)) +
  geom_histogram()
ggplot(gitr_flower_allo, aes(x=flower.num, fill = treatment)) +
  geom_histogram()


gitr_fallo_rel <- lm(flower.num ~ total.biomass.g, data = gitr_flower_allo)
summary(gitr_fallo_rel)
## slope = 72.44

gitr_fallo_D <- lm(flower.num ~ total.biomass.g, data = gitr_flower_allo[gitr_flower_allo$treatment == "D",])
summary(gitr_fallo_D)
## slope = 79.22

gitr_fallo_C <- lm(flower.num ~ total.biomass.g, data = gitr_flower_allo[gitr_flower_allo$treatment == "C",])
summary(gitr_fallo_C)
## slope = 71.69

## These numbers are pretty different, so for the time being use drought and ambient values separately
gitr_seed_means

gitr_seeds <- gitr_clean %>%
  mutate(flower.num.predicted = ifelse(treatment == "C", total.biomass.g.rounded*71.69, total.biomass.g.rounded*79.22)) %>%
  mutate(seed.num.predicted = ifelse(treatment == "C", flower.num.predicted*11.64, flower.num.predicted*8.70)) %>%
  mutate(percap.pred.flower.num = flower.num.predicted/phyto.n.indiv,
         percap.pred.seed.num = seed.num.predicted/phyto.n.indiv)


ggplot(gitr_seeds, aes(x=treatment, y=percap.pred.seed.num)) +
  geom_boxplot()
ggplot(gitr_seeds, aes(x=treatment, y=percap.pred.flower.num)) +
  geom_boxplot()


gitr_seeds_summary <- gitr_seeds %>%
  group_by(treatment) %>%
  summarise(mean.percap.flowers = mean(percap.pred.flower.num),
            mean.percap.seeds = mean(percap.pred.seed.num),
            se.percap.flowers = calcSE(percap.pred.flower.num),
            se.percap.seeds = calcSE(percap.pred.seed.num))

ggplot(gitr_seeds_summary, aes(x=treatment, y=mean.percap.seeds, color = treatment)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#699FA1", "#DD8627")) +
  geom_errorbar(aes(ymin = mean.percap.seeds - se.percap.seeds, ymax = mean.percap.seeds + se.percap.seeds), width = 0.25) +
  xlab("Treatment") +
  ylab("GITR mean per capita seed #")

ggsave("gitr_seeds_tx.png", height = 3.5, width = 4)

gitr_seeds_bgs <- gitr_seeds %>%
  group_by(treatment, bkgrd) %>%
  summarise(mean.percap.flowers = mean(percap.pred.flower.num),
            mean.percap.seeds = mean(percap.pred.seed.num),
            se.percap.flowers = calcSE(percap.pred.flower.num),
            se.percap.seeds = calcSE(percap.pred.seed.num))

ggplot(gitr_seeds_bgs, aes(x=treatment, y=mean.percap.seeds, color = treatment)) +
  geom_point(size = 1.5) +
  scale_color_manual(values = c("#699FA1", "#DD8627")) +
  geom_errorbar(aes(ymin = mean.percap.seeds - se.percap.seeds, ymax = mean.percap.seeds + se.percap.seeds), width = 0.15) +
  xlab("Treatment") +
  ylab("GITR mean per capita seed #") +
  facet_wrap(~bkgrd)
ggsave("gitr_seeds_bg.png", height = 5, width = 7)

ggplot(gitr_seeds_bgs, aes(x=bkgrd, y=mean.percap.seeds, color = treatment)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("#699FA1", "#DD8627")) +
  geom_errorbar(aes(ymin = mean.percap.seeds - se.percap.seeds, ymax = mean.percap.seeds + se.percap.seeds), width = 0.25) +
  xlab("Background Species") +
  ylab("GITR mean per capita seed #") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("gitr_seeds_bg.png", height = 3.5, width = 7)

# Test Allo Relationship ####
range(gitr_clean$total.biomass.g.rounded)
## 0.002 - 5.931
range(gitr_flower_allo$total.biomass.g)
## 0.0027 - 2.9547
## These ranges are not well aligned at larger sample sizes, need to ensure allo samples cover a better range


mean(gitr_clean[gitr_clean$treatment == "C",]$total.biomass.g.rounded)
mean(gitr_clean[gitr_clean$treatment == "D",]$total.biomass.g.rounded)
mean(gitr_flower_allo[gitr_flower_allo$treatment == "C",]$total.biomass.g)
mean(gitr_flower_allo[gitr_flower_allo$treatment == "D",]$total.biomass.g)
## the mean is quite a bit smaller in the allo samples than in the actual data. 


calcSE(gitr_clean[gitr_clean$treatment == "C",]$total.biomass.g.rounded)
calcSE(gitr_flower_allo[gitr_flower_allo$treatment == "C",]$total.biomass.g)

calcSE(gitr_clean[gitr_clean$treatment == "D",]$total.biomass.g.rounded)
calcSE(gitr_flower_allo[gitr_flower_allo$treatment == "D",]$total.biomass.g)



var(gitr_clean[gitr_clean$treatment == "C",]$total.biomass.g.rounded)
var(gitr_flower_allo[gitr_flower_allo$treatment == "C",]$total.biomass.g)

var(gitr_clean[gitr_clean$treatment == "D",]$total.biomass.g.rounded)
var(gitr_flower_allo[gitr_flower_allo$treatment == "D",]$total.biomass.g)


gitrdphytos <- ggplot(gitr_clean[gitr_clean$treatment == "D",], aes(x=total.biomass.g.rounded)) +
  geom_histogram() +
  coord_cartesian(xlim = c(0,6), ylim = c(0,60)) +
  ggtitle("GITR All Samples - Drought") +
  xlab("GITR total biomass (g)")

gitrdallo <- ggplot(gitr_flower_allo[gitr_flower_allo$treatment == "D",], aes(x=total.biomass.g)) +
  geom_histogram() +
  coord_cartesian(xlim = c(0,6), ylim = c(0,4)) +
  ggtitle("GITR Allometry Samples - Drought") +
  xlab("GITR total biomass (g)")

gitrcphytos <- ggplot(gitr_clean[gitr_clean$treatment == "C",], aes(x=total.biomass.g.rounded)) +
  geom_histogram() +
  coord_cartesian(xlim = c(0,6), ylim = c(0,60)) +
  ggtitle("GITR All Samples - Control") +
  xlab("GITR total biomass (g)")

gitrcallo <- ggplot(gitr_flower_allo[gitr_flower_allo$treatment == "C",], aes(x=total.biomass.g)) +
  geom_histogram() +
  coord_cartesian(xlim = c(0,6), ylim = c(0,4)) +
  ggtitle("GITR Allometry Samples - Control") +
  xlab("GITR total biomass (g)")

ggarrange(gitrcphytos, gitrdphytos, gitrcallo, gitrdallo, 
          nrow = 2, ncol = 2)
ggsave("allometry/gitr_allo_distributions.png", height = 5, width = 7)
