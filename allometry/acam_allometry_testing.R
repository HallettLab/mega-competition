## ACAM Allometric Relationship
## this script 
    ## 1. checks that phyto & allometry data cover approx the same range
          ## after this is checked & confirmed to be okay, comment out this part so that we do not load & reload the same phyto data multiple times in later scripts.
    ## 2. tests & plots various allometric relationships
    ## 3. saves the output from the final best model for use later in predicting seed output.


library(tidyverse)
library(ggpubr)
library(openxlsx)

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}


# specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Allometry/Allometry_entered/")){
  # Carmen
  allo_lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Allometry/Allometry_entered/"
  
} else {
  # Marina
  allo_lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Allometry/Allometry_entered/"
} 

acam_flower_allo <- read.csv(paste0(allo_lead, "ACAM-flowers_allometry-processing_20221213.csv"))

acam_seed_allo <- read.csv(paste0(allo_lead, "ACAM-seeds_allometry-processing_20221213.csv"))

ggplot(acam_flower_allo, aes(x=total.biomass.g, y=flower.num)) +
  geom_point() +
  geom_smooth(method = lm)

# Seed Distrib ####
ggplot(acam_seed_allo, aes(x=seed.num)) +
  geom_histogram()

## separate by treatment
ggplot(acam_seed_allo, aes(x=seed.num)) +
  geom_histogram()+
  facet_wrap(~treatment)

## look at sample num per treatment
nrow(acam_seed_allo[acam_seed_allo$treatment == "D",]) ## 36
nrow(acam_seed_allo[acam_seed_allo$treatment == "C",]) ## 36


# Calc Seeds/Flower ####
## calc overall mean
acam_mean_seeds <- mean(acam_seed_allo$seed.num, na.rm = T)
acam_mean_seeds ## 1.29

## calc mean by treatment
acam_seed_means <- acam_seed_allo %>%
  group_by(treatment) %>%
  summarise(mean_seeds = mean(seed.num, na.rm = T), SE_seeds = calcSE(seed.num))

## plot this
ggplot(acam_seed_means, aes(x=treatment, y = mean_seeds)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_seeds - SE_seeds, ymax = mean_seeds + SE_seeds), width = 0.25) +
  ylab("Mean Seeds per Flower") + xlab ("Treatment")

## use an anova to test signif differences b/w categories
seedtrt <- aov(seed.num ~ treatment, data = acam_seed_allo)

summary(seedtrt)
TukeyHSD(seedtrt) # they differ but this seems to be because of empty pods

# TotBio - Flower Rel. ####
## Combine drought and controls together for biomass-flower relationship

## visualize ####
ggplot(acam_flower_allo, aes(x=total.biomass.g, y=flower.num)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25, size = 0.75, formula = y ~ x)

## plot as polynomial
ggplot(acam_flower_allo, aes(x=total.biomass.g, y=flower.num)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25, size = 0.75, formula = y ~ poly(x, 2))


## model ####
## test linear model first
acam_fallo_rel <- lm(flower.num ~ total.biomass.g, data = acam_flower_allo)
summary(acam_fallo_rel) # r2 = 0.962

## test polynomial model
# acam_fallo_rel <- lm(flower.num ~ total.biomass.g + I(total.biomass.g^2), data = acam_flower_allo)
# summary(acam_fallo_rel) # r2 = 0.963


## save the model outputs
ACAM.allo.output <- data.frame(Species = "ACAM", 
           intercept = 0, 
           intercept_pval = NA, 
           intercept_se = NA, 
           slope = acam_fallo_rel$coefficients[2], 
           slope_pval = summary(acam_fallo_rel)$coefficients[2,4], 
           slope_se = summary(acam_fallo_rel)$coefficients[2,2], 
           poly = NA, 
           poly_pval = NA, 
           poly_se = NA,
           seeds_C = acam_seed_means[acam_seed_means$treatment == "C",]$mean_seeds,
           seeds_C_se = acam_seed_means[acam_seed_means$treatment == "C",]$SE_seeds,
           seeds_D = acam_seed_means[acam_seed_means$treatment == "D",]$mean_seeds,
           seeds_D_se = acam_seed_means[acam_seed_means$treatment == "D",]$SE_seeds,
           viability_C = NA,
           viability_C_se = NA,
           viability_D = NA,
           viability_D_se = NA)

rm(list = c("allo_lead", "acam_fallo_rel", "acam_flower_allo", "acam_mean_seeds",  "acam_seed_allo", "seedtrt", "acam_seed_means"))




