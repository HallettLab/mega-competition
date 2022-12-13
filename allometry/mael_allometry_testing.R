## MAEL Allometry Testing

# set up env
library(tidyverse)

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

# Read in Data ####
# specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Allometry/Allometry_entered/")){
  # Carmen
  allo_lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Allometry/Allometry_entered/"
  
} else {
  # Marina
  allo_lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Allometry/Allometry_entered/"
} 

## Allometry data
mael_allo <- read.csv(paste0(allo_lead, "MAEL_allometry-processing_20221128.csv")) %>%
  select(-X, -X.1, -X.2)


# Seed Distrib ####
ggplot(mael_allo, aes(x = seeds.num)) +
  geom_histogram()

## separate by treatment
ggplot(mael_allo, aes(x = seeds.num)) +
  geom_histogram()+
  facet_wrap(~treatment)


# Calc Seeds/Flower ####
## calc overall mean
mean(mael_allo$seeds.num, na.rm = T)
## 12.55

## calc mean by treatment
mael_seed_means <- mael_allo %>%
  group_by(treatment) %>%
  summarise(mean_seeds = mean(seeds.num, na.rm = T), SE_seeds = calcSE(seeds.num))


## plot this
ggplot(mael_seed_means, aes(x=treatment, y = mean_seeds)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_seeds - SE_seeds, ymax = mean_seeds + SE_seeds), width = 0.25) +
  ylab("Mean Seeds per Flower") + xlab ("Treatment")

## use an anova to test signif differences b/w categories
seedtrt <- aov(seeds.num ~ treatment, data = mael_allo)

summary(seedtrt)
TukeyHSD(seedtrt)


## Use separate means for drought & control since they are significantly different

rm(mael_allo, seedtrt, allo_lead)
