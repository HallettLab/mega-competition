## PLER Allometric Relationship
## this script 
## 1. checks that phyto & allometry data cover approx the same range
## after this is checked & confirmed to be okay, comment out this part so that we do not load & reload the same phyto data multiple times in later scripts.
## 2. tests & plots various allometric relationships
## 3. saves the output from the final best model for use later in predicting seed output.

# set up env
library(tidyverse)
library(ggpubr)

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

#Read in Data ####
# specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Allometry/Allometry_entered/")){
  # Carmen
  allo_lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Allometry/Allometry_entered/"
  
} else {
  # Marina
  allo_lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Allometry/Allometry_entered/"
} 

## Allometry data
pler_allo <- read.csv(paste0(allo_lead, "PLER_allometry-processing_20221101.csv"))

# Inflor-Seed Rel. ####
## Visualize ####
ggplot(pler_allo, aes(x=inflor.g, y=seed.num, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm")
## the error bars on the lines overlap for most of it - so probably not separate relationships here

ggplot(pler_allo, aes(x=inflor.g, y=seed.num)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(pler_allo, aes(x=inflor.g, y=seed.num)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x,2))

ggplot(pler_allo[pler_allo$inflor.g > 0,], aes(x=inflor.g, y=seed.num)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ log(x))

## Model ####
### *linear ####
inflorseeds <- lm(seed.num~inflor.g, data = pler_allo)
summary(inflorseeds)
## R2 = 0.9669

### poly ####
inflorseeds2 <- lm(seed.num ~ inflor.g + I(inflor.g^2), data = pler_allo)
summary(inflorseeds2)
## 0.967, but poly term is not signif; stick with linear



# Explored tot bio to seeds, but can't use since we didn't measure tot bio for all samples

# Biomass-Seed Rel. 
## Visualize 
#ggplot(pler_allo, aes(x=total.biomass.g, y=seed.num, color = treatment)) +
  #geom_point() +
  #geom_smooth(method = "lm")
## the error bars on the lines overlap for most of it - so probably not separate relationships here

#ggplot(pler_allo, aes(x=total.biomass.g, y=seed.num)) +
  #geom_point() +
  #geom_smooth(method = "lm")

#ggplot(pler_allo, aes(x=total.biomass.g, y=seed.num)) +
  #geom_point() +
  #geom_smooth(method = "lm", formula = y ~ poly(x,2))

## *Model 

#bioseeds <- lm(seed.num~total.biomass.g, data = pler_allo)
#summary(bioseeds)
# y = 3.143 + 346.885x
## we can't use this relationship as we haven't measured total biomass for all fo the samples...

# bioseeds2 <- lm(seed.num ~ total.biomass.g + I(total.biomass.g^2), data = pler_allo)
# summary(bioseeds2)


# Save Output ####
## save the model outputs
PLER.allo.output <- data.frame(Species = "PLER", 
           intercept = 0, 
           intercept_pval = NA, 
           intercept_se = NA, 
           
           slope = summary(inflorseeds)$coefficients[2,1], 
           slope_pval = summary(inflorseeds)$coefficients[2,4], 
           slope_se = summary(inflorseeds)$coefficients[2,2], 
           
           seeds_C = NA,
           seeds_C_se = NA,
           seeds_D = NA,
           seeds_D_se = NA, 
           
           viability_C = NA,
           viability_C_se = NA,
           viability_D = NA,
           viability_D_se = NA)

# Clean Env ####
rm(list = c("allo_lead", "pler_allo","inflorseeds", "inflorseeds2"))
