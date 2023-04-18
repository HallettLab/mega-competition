## LOMU allometry testing

## set up env
library(tidyverse)
theme_set(theme_bw())

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

# Read in Data ####
## Allo Data ####
# specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Allometry/Allometry_entered/")){
  # Carmen
  allo_lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Allometry/Allometry_entered/"
  
} else {
  # Marina
  allo_lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Allometry/Allometry_entered/"
} 


lomu_allo <- read.csv(paste0(allo_lead, "LOMU_allometry-processing_20230202.csv")) %>%
  mutate(phyto.unique = unique, 
         phyto.unique = ifelse(phyto.unique == "", NA, phyto.unique),
         allo.biomass = total.biomass.g)

## Phyto Data ####
## specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/"
  
} else {
  # Marina
  lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/"
} 
## basic cleaning function
source("data_cleaning/phyto-processing_data-cleaning/basic_cleaning_function.R")

lomu_phyto <- read.csv(paste0(lead, "LOMU_phyto-processing-redo_20230206.csv"))

## someone wrote "missing in the redo.total.biomass column
lomu_phyto[lomu_phyto$block == 14 & lomu_phyto$plot == 21 & lomu_phyto$sub == 2, ]$redo.total.biomass <- NA
lomu_phyto[lomu_phyto$block == 14 & lomu_phyto$plot == 21 & lomu_phyto$sub == 2, ]$redo.notes <- "sample missing"

lomu_phyto$redo.total.biomass <- as.numeric(lomu_phyto$redo.total.biomass)

lomu_phytoC <- basic_cleaning_func(lomu_phyto)

lomu_phytoC2 <- lomu_phytoC %>%
  mutate(final.total.biomass.g = ifelse(!is.na(redo.total.biomass), redo.total.biomass, total.biomass.g))

# Clean Data ####
lomu_combined <- left_join(lomu_allo, lomu_phytoC2, by = c("treatment", "block", "plot", "sub", "phyto.unique"))

ggplot(lomu_combined, aes(x=final.total.biomass.g, y=allo.biomass)) +
  geom_point() +
  geom_abline(slope = 1)
## one sample doesn't align on the 1:1 line

mismatch <- lomu_combined %>%
  filter(final.total.biomass.g != allo.biomass) %>%
  select(block, plot, sub, phyto.unique, final.total.biomass.g, allo.biomass)
## all of these should be changed!!!

lomu_alloC <- lomu_combined %>%
  mutate(allo.total.biomass.g = ifelse(is.na(final.total.biomass.g), allo.biomass, final.total.biomass.g)) %>%
  select(treatment:notes, allo.total.biomass.g, final.total.biomass.g)


ggplot(lomu_alloC, aes(x=final.total.biomass.g, y=allo.total.biomass.g)) +
  geom_point() +
  geom_abline(slope = 1)
## fixed


# Visualize ####
## Linear ####
ggplot(lomu_alloC, aes(x=allo.total.biomass.g, y=seeds.num, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm")
## relationships look very similar b/w treats! 
ggplot(lomu_alloC, aes(x=allo.total.biomass.g, y=seeds.num)) +
  geom_point() +
  geom_smooth(method = "lm")

## Polynomial ####
## plot as polynomial
ggplot(lomu_alloC, aes(x = allo.total.biomass.g, y = seeds.num)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25, linewidth = 0.75, formula = y ~ poly(x, 2))

# Model ####
lomu_allo_rel_lin <- lm(seeds.num ~ allo.total.biomass.g, data = lomu_alloC)
summary(lomu_allo_rel_lin) # r2 = 0.9511

lomu_allo_rel_pol <- lm(seeds.num ~ allo.total.biomass.g + I(allo.total.biomass.g^2), data = lomu_alloC)
summary(lomu_allo_rel_pol) # r2 = 0.9592

## polynomial is slightly better.

# Save Output ####
## save the model outputs
LOMU.allo.output <- data.frame(Species = "LOMU", 
                               intercept = 0, 
                               intercept_pval = NA, 
                               intercept_se = NA, 
                               
                               slope = lomu_allo_rel_lin$coefficients[2], 
                               slope_pval = summary(lomu_allo_rel_lin)$coefficients[2,4], 
                               slope_se = summary(lomu_allo_rel_lin)$coefficients[2,2], 
                               
                               seeds_C = NA,
                               seeds_C_se = NA,
                               seeds_D = NA,
                               seeds_D_se = NA, 
                               
                               viability_C = NA,
                               viability_C_se = NA,
                               viability_D = NA,
                               viability_D_se = NA)

## clean env
rm(list = c("lomu_allo", "lomu_allo_rel_lin", "lomu_allo_rel_pol", "allo_lead", "lomu_alloC", "lomu_combined", "lomu_phyto", "lomu_phytoC", "lomu_phytoC2", "mismatch"))
