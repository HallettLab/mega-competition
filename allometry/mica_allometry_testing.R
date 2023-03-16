## MICA Allometry Testing

## set up env
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


mica_allo <- read.csv(paste0(allo_lead, "MICA_allometry-processing_20230118.csv")) 

# Visualize ####
## Linear ####
ggplot(mica_allo, aes(x=total.biomass.g, y=seeds.num, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm")
## relationships look very similar b/w treats! 


## Polynomial ####
## plot as polynomial
ggplot(mica_allo, aes(x = total.biomass.g, y = seeds.num)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25, linewidth = 0.75, formula = y ~ poly(x, 2))
## visually this looks pretty good!


# Model ####
mica_allo_rel_lin <- lm(seeds.num ~ total.biomass.g, data = mica_allo)
summary(mica_allo_rel_lin) # r2 = 0.9694

mica_allo_rel_pol <- lm(seeds.num ~ total.biomass.g + I(total.biomass.g^2), data = mica_allo)
summary(mica_allo_rel_pol) # r2 = 0.981

# Save Output ####
## save the model outputs
MICA.allo.output <- data.frame(Species = "MICA", 
                               intercept = 0, 
                               intercept_pval = NA, 
                               intercept_se = NA, 
                               
                               slope = mica_allo_rel_pol$coefficients[2], 
                               slope_pval = summary(mica_allo_rel_pol)$coefficients[2,4], 
                               slope_se = summary(mica_allo_rel_pol)$coefficients[2,2], 
                               
                               poly = summary(mica_allo_rel_pol)$coefficients[3], 
                               poly_pval = summary(mica_allo_rel_pol)$coefficients[3,4], 
                               poly_se = summary(mica_allo_rel_pol)$coefficients[3, 2],
                               
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

## clean env
rm(list = c("mica_allo", "mica_allo_rel_lin", "mica_allo_rel_pol", "allo_lead"))
