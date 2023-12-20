## TACA Allometric Relationship
## this script 
    ## 1. checks that phyto & allometry data cover approx the same range
          ## after this is checked & confirmed to be okay, comment out this part so that we do not load & reload the same phyto data multiple times in later scripts.
    ## 2. tests & plots various allometric relationships
    ## 3. saves the output from the final best model for use later in predicting seed output.

library(tidyverse)
library(ggpubr)
theme_set(theme_classic())

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

## Phyto data
#taca_phyto <- read.csv(paste0(allo_lead, "TACA_phyto-processing_20221108.csv"))

nrow(taca_allo[taca_allo$treatment == "D",]) ## 20
nrow(taca_allo[taca_allo$treatment == "C",]) ## 20

## visualize ####
final1 <- ggplot(taca_allo, aes(x=total.biomass.g)) +
  geom_histogram() +
  xlab("Aboveground Biomass (g)") +
  ylab("Count")

ggplot(taca_allo, aes(x = total.biomass.g, y = seeds.num, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm")

final2 <- ggplot(taca_allo, aes(x = total.biomass.g, y = seeds.num)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25, linewidth = 0.75, formula = y ~ x) +
  xlab("Aboveground Biomass (g)") +
  ylab("Seed Number")
  

## plot as polynomial
#ggplot(taca_allo[taca_allo$total.biomass.g<20,], aes(x = total.biomass.g, y = seeds.num)) +
 # geom_point() +
  #geom_smooth(method = "lm", alpha = 0.25, linewidth = 0.75, formula = y ~ poly(x, 2))

## model ####
### *linear ####
taca_allo_rel_lin <- lm(seeds.num ~ total.biomass.g, data = taca_allo)
summary(taca_allo_rel_lin) # r2 = 0.9731

## is this a better way to test if there are slope differences?
taca_allo_rel_lin2 <- lm(seeds.num ~ total.biomass.g + treatment, data = taca_allo)
summary(taca_allo_rel_lin2)

anova(taca_allo_rel_lin, 
      taca_allo_rel_lin2)

### poly ####
#taca_allo_rel_pol <- lm(seeds.num ~ total.biomass.g + I(total.biomass.g^2), data = taca_allo)
#summary(taca_allo_rel_pol) # r2 = 0.9973

# what about without outliers?
## test linear model first
#taca_allo_rel <- lm(seeds.num ~ total.biomass.g, data = taca_allo[taca_allo$total.biomass.g<20,])
#summary(taca_allo_rel) # r2 = 0.957

## test polynomial model
#taca_allo_rel <- lm(seeds.num ~ total.biomass.g + I(total.biomass.g^2), data = taca_allo[taca_allo$total.biomass.g<20,])
#summary(taca_allo_rel) # r2 = 0.9668

# Methods Figure ####
plot <- ggarrange(final1, final2, labels = "AUTO", ncol = 2, nrow = 1)

annotate_figure(plot, top = text_grob("TACA", 
                                      color = "black", face = "bold", size = 14))

ggsave("allometry/methods_figures/TACA.png", height = 3, width = 6.5)

# Save Outputs ####
TACA.allo.output <- data.frame(Species = "TACA", 
                               intercept = 0, 
                               intercept_pval = NA, 
                               intercept_se = NA, 
                               
                               slope = taca_allo_rel_lin$coefficients[2], 
                               slope_pval = summary(taca_allo_rel_lin)$coefficients[2,4], 
                               slope_se = summary(taca_allo_rel_lin)$coefficients[2,2], 

                               seeds_C = NA,
                               seeds_C_se = NA,
                               seeds_D = NA,
                               seeds_D_se = NA, 
                               
                               viability_C = NA,
                               viability_C_se = NA,
                               viability_D = NA,
                               viability_D_se = NA)

# Clean Env ####
rm(list = c("allo_lead", "taca_allo", "taca_allo_rel_lin", "taca_allo_rel_lin2", "final1", "final2", "plot"))