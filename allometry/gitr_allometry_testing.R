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
gitr_flower_allo <- read.csv(paste0(allo_lead, "GITR-flowers_allometry-processing_", date, ".csv"))

drought <- c(1, 3, 4, 6, 12, 14) ## create treatment vector
gitr_seed_allo <- read.csv(paste0(allo_lead, "GITR-seeds_allometry-processing_", date, ".csv")) %>%
  select(1:6, 8) %>%
  mutate(treatment = ifelse(Block %in% drought, "D", "C")) %>%
  filter(!is.na(seed.num))


# Flower Dat Range ####
theme_set(theme_bw())

gitr_dat <- all_dat_final %>%
  filter(phyto == "GITR")

phyto<-ggplot(gitr_dat, aes(x=total.biomass.rounded.percap)) +
  geom_histogram() +
  facet_wrap(~treatment)

allo<-ggplot(gitr_flower_allo, aes(x=total.biomass.g)) +
  geom_histogram() +
  facet_wrap(~treatment) +
  coord_cartesian(xlim = c(0,4))


ggarrange(phyto, allo, ncol = 1, nrow=2)

#ggsave("gitr_allometry_check.png", height = 4, width = 6)


# Seed Distrib ####
ggplot(gitr_seed_allo, aes(x=seed.per.pod)) +
  geom_histogram()

## separate by treatment
ggplot(gitr_seed_allo, aes(x=seed.per.pod)) +
  geom_histogram()+
  facet_wrap(~treatment)

## look at sample num per treatment
nrow(gitr_seed_allo[gitr_seed_allo$treatment == "D",]) ## 19
nrow(gitr_seed_allo[gitr_seed_allo$treatment == "C",]) ## 33
## Q here ####
  ## somewhat uneven sample sizes. Does this matter here?


# Calc Seeds/Flower ####
## calc overall mean
gitr_mean_seeds <- mean(gitr_seed_allo$seed.per.pod, na.rm = T)
gitr_mean_seeds ## 10.55 

## calc mean by treatment
gitr_seed_means <- gitr_seed_allo %>%
  group_by(treatment) %>%
  summarise(mean_seeds = mean(seed.per.pod, na.rm = T), SE_seeds = calcSE(seed.per.pod))

## plot this
ggplot(gitr_seed_means, aes(x=treatment, y = mean_seeds)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_seeds - SE_seeds, ymax = mean_seeds + SE_seeds), width = 0.25) +
  ylab("Mean Seeds per Flower") + xlab ("Treatment")

## use an anova to test signif differences b/w categories
seedtrt <- aov(seed.per.pod~treatment, data = gitr_seed_allo)
## residual standard error: 4.15402 "Estimated effects may be unbalanced"
    ## don't know what this means.
summary(seedtrt)
TukeyHSD(seedtrt)

## Q here ####
    ## given signif differences b/w seeds per flower in drought vs. control, should we use separate numbers?



# TotBio - Flower Rel. ####
ggplot(gitr_flower_allo, aes(x=total.biomass.g, y=flower.num, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25, size = 0.75)

gitr_fallo_rel <- lm(flower.num ~ total.biomass.g, data = gitr_flower_allo)
summary(gitr_fallo_rel)
## slope = 55.505

gitr_fallo_D <- lm(flower.num ~ total.biomass.g, data = gitr_flower_allo[gitr_flower_allo$treatment == "D",])
summary(gitr_fallo_D)
## slope = 48.942

gitr_fallo_C <- lm(flower.num ~ total.biomass.g, data = gitr_flower_allo[gitr_flower_allo$treatment == "C",])
summary(gitr_fallo_C)
## slope = 66.484

## Q here ####
    ## how do we decide whether to differentiate the relationship or not?
    ## how do we test other models?


# Predict Flower Num ####


