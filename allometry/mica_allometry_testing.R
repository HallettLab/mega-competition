## MICA Allometry Testing

## set up env
library(tidyverse)
library(ggpubr)
theme_set(theme_classic())

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
final1 <- ggplot(mica_allo, aes(x=total.biomass.g)) +
  geom_histogram() +
  xlab("Aboveground Biomass (g)") +
  ylab("Count")

final2 <- ggplot(mica_allo, aes(x=total.biomass.g, y=seeds.num)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25, linewidth = 0.75, formula = y ~ x) +
  xlab("Aboveground Biomass (g)") +
  ylab("Seed Number")

## Polynomial ####
## plot as polynomial
#ggplot(mica_allo, aes(x = total.biomass.g, y = seeds.num)) +
  #geom_point() +
 # geom_smooth(method = "lm", alpha = 0.25, linewidth = 0.75, formula = y ~ poly(x, 2))
## visually this looks pretty good!

# Model ####
## *Linear ####
mica_allo_rel_lin <- lm(seeds.num ~ total.biomass.g, data = mica_allo)
summary(mica_allo_rel_lin) # r2 = 0.9694

## Poly ####
#mica_allo_rel_pol <- lm(seeds.num ~ total.biomass.g + I(total.biomass.g^2), data = mica_allo)
#summary(mica_allo_rel_pol) # r2 = 0.981

## switched to using linear as poly models are not good for predicting even if they might fit the existing data better.

# Methods Figure ####
plot <- ggarrange(final1, final2, labels = "AUTO", ncol = 2, nrow = 1)

annotate_figure(plot, top = text_grob("MICA", 
                                      color = "black", face = "bold", size = 14))

ggsave("allometry/methods_figures/MICA.png", height = 3, width = 6.5)

# Save Output ####
## save the model outputs
MICA.allo.output <- data.frame(Species = "MICA", 
                               intercept = 0, 
                               intercept_pval = NA, 
                               intercept_se = NA, 
                               
                               slope = mica_allo_rel_lin$coefficients[2], 
                               slope_pval = summary(mica_allo_rel_lin)$coefficients[2,4], 
                               slope_se = summary(mica_allo_rel_lin)$coefficients[2,2], 
                               
                               seeds_C = NA,
                               seeds_C_se = NA,
                               seeds_D = NA,
                               seeds_D_se = NA, 
                               
                               viability_C = NA,
                               viability_C_se = NA,
                               viability_D = NA,
                               viability_D_se = NA)

## clean env
rm(list = c("mica_allo", "mica_allo_rel_lin", "allo_lead", "final1", "final2", "plot"))
