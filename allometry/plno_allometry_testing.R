## PLNO Allometry

## Set up env
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
plno_flowers_allo <- read.csv(paste0(allo_lead, "PLNO-flowers_allometry-processing_20230130.csv"))

plno_seeds_allo <- read.csv(paste0(allo_lead, "PLNO-seeds_allometry-processing_20220130.csv"))


nrow(plno_flowers_allo[plno_flowers_allo$treatment == "D",]) ## 20
nrow(plno_flowers_allo[plno_flowers_allo$treatment == "C",]) ## 20

# Visualize ####
## flowers ####
ggplot(plno_flowers_allo, aes(x=total.biomass.g, y=flower.num)) +
  geom_point() +
  geom_smooth(method = "lm")
## visually doesn't look like a great fit

ggplot(plno_flowers_allo, aes(x=total.biomass.g, y=flower.num, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm")
## these are pretty different

ggplot(plno_flowers_allo, aes(x=total.biomass.g, y=flower.num)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25, linewidth = 0.75, formula = y ~ poly(x, 2))
## better than the linear fit

ggplot(plno_flowers_allo, aes(x=total.biomass.g, y=flower.num, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25, linewidth = 0.75, formula = y ~ poly(x, 2))
## better than linear but now the error bars overlap just a bit.

## seeds ####
plno_seed_means <- plno_seeds_allo %>%
  group_by(treatment) %>%
  summarise(mean.seeds = mean(seeds.num), se.seeds = calcSE(seeds.num))

ggplot(plno_seed_means, aes(x=treatment, y=mean.seeds)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin= mean.seeds-se.seeds, ymax = mean.seeds + se.seeds), width = 0.25)
## these look significantly different

# Models ####
## flowers ####
### linear ####
plno_allo_lin <- lm(flower.num~total.biomass.g, data = plno_flowers_allo)
summary(plno_allo_lin)
## R2 = 0.8824

plno_allo_lin2 <- lm(flower.num~total.biomass.g + treatment, data = plno_flowers_allo)
summary(plno_allo_lin2)
## R2 = 0.8963

### polynomial ####
plno_allo_pol <- lm(flower.num ~ total.biomass.g + I(total.biomass.g^2), data = plno_flowers_allo)
summary(plno_allo_pol)
## R2 = 0.9156

plno_allo_pol2 <- lm(flower.num ~ total.biomass.g + I(total.biomass.g^2) + treatment, data = plno_flowers_allo)
summary(plno_allo_pol2)
## R2 = 0.9249


## seeds ####
plno_seeds <- aov(seeds.num~treatment, data = plno_seeds_allo)
summary(plno_seeds)
## significant effect of treatment, use separate numbers

## save the model outputs
PLNO.allo.output <- data.frame(Species = "PLNO", 
                               intercept = 0, 
                               intercept_pval = NA, 
                               intercept_se = NA, 
                               
                               
                               slope = plno_allo_pol$coefficients[2], 
                               slope_pval = summary(plno_allo_pol)$coefficients[2,4], 
                               slope_se = summary(plno_allo_pol)$coefficients[2,2], 
                               poly = summary(plno_allo_pol)$coefficients[3], 
                               poly_pval = summary(plno_allo_pol)$coefficients[3,4], 
                               poly_se = summary(plno_allo_pol)$coefficients[3, 2],
                               
                               
                               seeds_C = plno_seed_means[plno_seed_means$treatment == "C",]$mean.seeds,
                               seeds_C_se = plno_seed_means[plno_seed_means$treatment == "C",]$se.seeds,
                               seeds_D = plno_seed_means[plno_seed_means$treatment == "D",]$mean.seeds,
                               seeds_D_se = plno_seed_means[plno_seed_means$treatment == "D",]$se.seeds, 
                               
                               viability_C = NA,
                               viability_C_se = NA,
                               viability_D = NA,
                               viability_D_se = NA)


rm(list = c("allo_lead", "plno_flowers_allo", "plno_seed_means", "plno_seeds_allo",  "plno_allo_lin", "plno_allo_lin2", "plno_allo_pol", "plno_allo_pol2", "plno_seeds"))