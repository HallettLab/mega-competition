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
inflorseeds <- lm(seed.num~inflor.g, data = pler_allo)
summary(inflorseeds)

inflorseeds2 <- lm(seed.num ~ inflor.g + I(inflor.g^2), data = pler_allo)
summary(inflorseeds2)

# Biomass-Seed Rel. ####
## Visualize ####
ggplot(pler_allo, aes(x=total.biomass.g, y=seed.num, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm")
## the error bars on the lines overlap for most of it - so probably not separate relationships here

ggplot(pler_allo, aes(x=total.biomass.g, y=seed.num)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(pler_allo, aes(x=total.biomass.g, y=seed.num)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x,2))

ggplot(pler_allo, aes(x=total.biomass.g, y=seed.num)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ log(x))

## Model ####

bioseeds <- lm(seed.num~total.biomass.g, data = pler_allo)
summary(bioseeds)
# y = 3.143 + 346.885x

bioseeds2 <- lm(seed.num ~ total.biomass.g + I(total.biomass.g^2), data = pler_allo)
summary(bioseeds2)

## save the model outputs
pler.allo.output <- bioseeds$coefficients

# Clean Env ####
rm(list = c("allo_lead", "pler_allo","inflorseeds", "inflorseeds2", "bioseeds2", "bioseeds"))
