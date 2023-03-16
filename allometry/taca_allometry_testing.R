## TACA Allometric Relationship
## this script 
    ## 1. checks that phyto & allometry data cover approx the same range
          ## after this is checked & confirmed to be okay, comment out this part so that we do not load & reload the same phyto data multiple times in later scripts.
    ## 2. tests & plots various allometric relationships
    ## 3. saves the output from the final best model for use later in predicting seed output.

library(tidyverse)

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

## Allometry data
taca_allo <- read.csv(paste0(allo_lead, "TACA_allometry-processing_20230116.csv"))

nrow(taca_allo[taca_allo$treatment == "D",]) ## 20
nrow(taca_allo[taca_allo$treatment == "C",]) ## 20

ggplot(taca_allo, aes(x = total.biomass.g, y = seeds.num, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm")

## use an anova to test signif differences b/w categories
seedtrt <- aov(seeds.num ~ treatment, data = taca_allo)

summary(seedtrt)
TukeyHSD(seedtrt) # no difference between treatments

## is this a better way to test if there are slope differences?
seedtrt2 <- lm(seeds.num ~ total.biomass.g + treatment, data = taca_allo)
summary(seedtrt2)

## Phyto data
#taca_phyto <- read.csv(paste0(allo_lead, "TACA_phyto-processing_20221108.csv"))


## visualize ####
ggplot(taca_allo, aes(x = total.biomass.g, y = seeds.num)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25, linewidth = 0.75, formula = y ~ x)

## plot as polynomial
ggplot(taca_allo[taca_allo$total.biomass.g<20,], aes(x = total.biomass.g, y = seeds.num)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25, linewidth = 0.75, formula = y ~ poly(x, 2))

## model ####
## test linear model first
taca_allo_rel_lin <- lm(seeds.num ~ total.biomass.g, data = taca_allo)
summary(taca_allo_rel_lin) # r2 = 0.9731

## test polynomial model
taca_allo_rel_pol <- lm(seeds.num ~ total.biomass.g + I(total.biomass.g^2), data = taca_allo)
summary(taca_allo_rel_pol) # r2 = 0.9973

# what about without outliers?
## test linear model first
taca_allo_rel <- lm(seeds.num ~ total.biomass.g, data = taca_allo[taca_allo$total.biomass.g<20,])
summary(taca_allo_rel) # r2 = 0.957

## test polynomial model
taca_allo_rel <- lm(seeds.num ~ total.biomass.g + I(total.biomass.g^2), data = taca_allo[taca_allo$total.biomass.g<20,])
summary(taca_allo_rel) # r2 = 0.9668


# Save Outputs ####
TACA.allo.output <- data.frame(Species = "TACA", 
                               intercept = 0, 
                               intercept_pval = NA, 
                               intercept_se = NA, 
                               
                               slope = taca_allo_rel_pol$coefficients[2], 
                               slope_pval = summary(taca_allo_rel_pol)$coefficients[2,4], 
                               slope_se = summary(taca_allo_rel_pol)$coefficients[2,2], 
                               
                               poly = summary(taca_allo_rel_pol)$coefficients[3], 
                               poly_pval = summary(taca_allo_rel_pol)$coefficients[3,4], 
                               poly_se = summary(taca_allo_rel_pol)$coefficients[3, 2],
                               
                               seeds_C = NA,
                               seeds_C_se = NA,
                               seeds_D = NA,
                               seeds_D_se = NA, 
                               
                               viability_C = NA,
                               viability_C_se = NA,
                               viability_D = NA,
                               viability_D_se = NA,
                               
                               viability_slope = NA,
                               viability_slope_pval = NA,
                               viability_slope_se = NA)

# Clean Env ####
rm(list = c("allo_lead", "taca_allo", "taca_allo_rel", "taca_allo_rel_lin", "taca_allo_rel_pol", "seedtrt", "seedtrt2"))