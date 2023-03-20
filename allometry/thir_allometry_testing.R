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
thir_flowers_allo <- read.csv(paste0(allo_lead, "THIR-flowers_allometry-processing_20230119.csv")) %>%
  mutate(allo.total.biomass.g = total.biomass.g)

thir_viability_allo <- read.csv(paste0(allo_lead, "THIR-viability_allometry-processing_20230119.csv"))


## Phyto data
## specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/"
  
} else {
  # Marina
  lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/"
} 

thir_phyto <- read.csv(paste0(lead, "THIR_phyto-processing-redo_20230124.csv")) %>%
  mutate(final.total.biomass.g = ifelse(!is.na(redo.total.biomass.g), redo.total.biomass.g, total.biomass.g))

# Cleaning ####
## Check that weights match redo.weights
thir_combined <- left_join(thir_flowers_allo, thir_phyto, by = c("block", "plot", "sub", "phyto.unique"))

ggplot(thir_combined, aes(x=final.total.biomass.g, y=allo.total.biomass.g)) +
  geom_point() +
  geom_abline(slope = 1)
## looks good


# Bio-Flower Rel. ####
## Visualize ####
### linear ####
ggplot(thir_flowers_allo, aes(x=total.biomass.g, y=all.flowers.num)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(thir_flowers_allo, aes(x=total.biomass.g, y=all.flowers.num, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm")

### polynomial ####
ggplot(thir_flowers_allo, aes(x = total.biomass.g, y = all.flowers.num)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25, linewidth = 0.75, formula = y ~ poly(x, 2))

ggplot(thir_flowers_allo, aes(x = total.biomass.g, y = all.flowers.num, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25, linewidth = 0.75, formula = y ~ poly(x, 2))


## Model ####
thir_fallo_lin <- lm(all.flowers.num~total.biomass.g, data = thir_flowers_allo)
summary(thir_fallo_lin)
## r2 = 0.9216

thir_fallo_lin2 <- lm(all.flowers.num~total.biomass.g + treatment, data = thir_flowers_allo)
summary(thir_fallo_lin2)
## maybe use separate relationships?
## r2 = 0.9297

thir_fallo_pol <- lm(all.flowers.num ~ total.biomass.g + I(total.biomass.g^2), data = thir_flowers_allo)
summary(thir_fallo_pol) 
# r2 = 0.9216

thir_fallo_pol2 <- lm(all.flowers.num ~ total.biomass.g + I(total.biomass.g^2) + treatment, data = thir_flowers_allo)
summary(thir_fallo_pol2) 

## Use linear model; MAYBE include treatment? It has a significant effect but I'm not sure it's that necessary because the R2 values are very similar between including treatment vs. not including it

# Viability ####
## summarise ####
viability_sum <- thir_viability_allo %>%
  group_by(treatment) %>%
  summarise(mean.via = mean(viability), se.via = calcSE(viability), reps = n())

mean_viability <- thir_viability_allo %>%
  summarise(mean.via = mean(viability), se.via = calcSE(viability))

## visualize ####
ggplot(viability_sum, aes(x=treatment, y=mean.via)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean.via - se.via, ymax = mean.via + se.via), width = 0.25)
## error bars are overlapping a lot, probably not a significant difference here

## test ####
viability_test <- aov(viability~treatment, data = thir_viability_allo)
summary(viability_test)
## not significantly different, use an overall mean 







# Save Outputs ####
THIR.allo.output <- data.frame(Species = "THIR", 
                               intercept = 0, 
                               intercept_pval = NA, 
                               intercept_se = NA, 
                               
                               slope = thir_fallo_lin$coefficients[2], 
                               slope_pval = summary(thir_fallo_lin)$coefficients[2,4], 
                               slope_se = summary(thir_fallo_lin)$coefficients[2,2], 
                               
                               poly = NA, 
                               poly_pval = NA, 
                               poly_se = NA,
                               
                               seeds_C = 1,
                               seeds_C_se = NA,
                               seeds_D = 1,
                               seeds_D_se = NA,
                               
                               viability_C = mean_viability[,1],
                               viability_C_se = mean_viability[,2],
                               viability_D = mean_viability[,1],
                               viability_D_se = mean_viability[,2])

rm(list = c("mean_viability", "viability_test", "viability_sum", "thir_viability_allo", "thir_flowers_allo", "thir_fallo_pol2", "thir_fallo_pol", "thir_fallo_lin", "thir_fallo_lin2", "thir_combined", "thir_phyto"))
