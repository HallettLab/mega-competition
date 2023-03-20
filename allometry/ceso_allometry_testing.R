## CESO allometry testing

# set up env
library(tidyverse)
theme_set(theme_bw())

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
ceso_allo <- read.csv(paste0(allo_lead, "CESO_allometry-processing_20230123.csv")) 


# Seed Distrib ####
ggplot(ceso_allo, aes(x=seeds.num)) +
  geom_histogram()
ggplot(ceso_allo, aes(x=seeds.num)) +
  geom_histogram() +
  facet_wrap(~treatment)


# Calc Seeds/Flower ####
mean(ceso_allo$seeds.num, na.rm = T)
## 14.725

## calc mean by treatment
ceso_seed_means <- ceso_allo %>%
  group_by(treatment) %>%
  summarise(mean_seeds = mean(seeds.num, na.rm = T), SE_seeds = calcSE(seeds.num))

## visualize
ggplot(ceso_seed_means, aes(x=treatment, y=mean_seeds)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_seeds - SE_seeds, ymax = mean_seeds + SE_seeds), width = 0.25) +
  theme_bw() +
  ylab("Avg Seeds per Flower") + xlab("Treatment")


## do an anova to test whether seeds/flower is different in control vs. drought
seedtrt <- aov(seeds.num ~ treatment, data = ceso_allo)

summary(seedtrt)
TukeyHSD(seedtrt)
## marginally significant differences


# Q here ####
## should we use different C and D estimates since they are only marginally significantly different? 
## do we need more samples?

CESO.allo.output <- data.frame(Species = "CESO", 
                               intercept = 0, 
                               intercept_pval = NA, 
                               intercept_se = NA, 
                               
                               slope = NA, 
                               slope_pval = NA, 
                               slope_se = NA, 
                               
                               poly = NA, 
                               poly_pval = NA, 
                               poly_se = NA,
                               
                               seeds_C = ceso_seed_means[ceso_seed_means$treatment == "C",]$mean_seeds,
                               seeds_C_se = ceso_seed_means[ceso_seed_means$treatment == "C",]$SE_seeds,
                               seeds_D = ceso_seed_means[ceso_seed_means$treatment == "D",]$mean_seeds,
                               seeds_D_se = ceso_seed_means[ceso_seed_means$treatment == "D",]$SE_seeds,
                               
                               viability_C = NA,
                               viability_C_se = NA,
                               viability_D = NA,
                               viability_D_se = NA)

rm(ceso_allo, seedtrt, allo_lead, ceso_seed_means)
