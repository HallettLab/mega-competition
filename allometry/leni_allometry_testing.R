## LENI Allometry Testing

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
## Allo Data ####
# specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Allometry/Allometry_entered/")){
  # Carmen
  allo_lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Allometry/Allometry_entered/"
  
} else {
  # Marina
  allo_lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Allometry/Allometry_entered/"
} 

leni_allo <- read.csv(paste0(allo_lead, "LENI_allometry-processing_20230307.csv"))

nrow(leni_allo[leni_allo$treatment == "D",]) ## 27
nrow(leni_allo[leni_allo$treatment == "C",]) ## 21
  ## uneven sample numbers as some relationships were sampled with a lot of samples of similar sizes initially

# Visualize ####
## linear ####
final1 <- ggplot(leni_allo, aes(x=total.biomass.g)) +
  geom_histogram() +
  xlab("Aboveground Biomass (g)") +
  ylab("Count")

final2 <- ggplot(leni_allo, aes(x = total.biomass.g, y = pod.num)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25, linewidth = 0.75, formula = y ~ x) +
  xlab("Aboveground Biomass (g)") +
  ylab("Flower Number")

## polynomial ####
#ggplot(leni_allo, aes(x = total.biomass.g, y = pod.num)) +
 # geom_point() +
  #geom_smooth(method = "lm", alpha = 0.25, linewidth = 0.75, formula = y ~ poly(x, 2))

# Model ####
## *linear ####
leni_allo_rel_lin <- lm(pod.num ~ total.biomass.g, data = leni_allo)
summary(leni_allo_rel_lin) # r2 = 0.986

## polynomial ####
#leni_allo_rel_pol <- lm(pod.num ~ total.biomass.g + I(total.biomass.g^2), data = leni_allo)
#summary(leni_allo_rel_pol) # r2 = 0.986
## polynomial term is not significant and the model R2 is the same as the linear model. Might as well use linear and keep it simple.

# Methods Figure ####
plot <- ggarrange(final1, final2, labels = "AUTO", ncol = 2, nrow = 1)

annotate_figure(plot, top = text_grob("LENI", 
                                      color = "black", face = "bold", size = 14))

ggsave("allometry/methods_figures/LENI.png", height = 3, width = 6.5)

# Save Output ####
LENI.allo.output <- data.frame(Species = "LENI", 
                               intercept = 0, 
                               intercept_pval = NA, 
                               intercept_se = NA, 
                               
                               slope = leni_allo_rel_lin$coefficients[2], 
                               slope_pval = summary(leni_allo_rel_lin)$coefficients[2,4], 
                               slope_se = summary(leni_allo_rel_lin)$coefficients[2,2], 
                               
                               seeds_C = 2,
                               seeds_C_se = NA,
                               seeds_D = 2,
                               seeds_D_se = NA, 
                               
                               viability_C = NA,
                               viability_C_se = NA,
                               viability_D = NA,
                               viability_D_se = NA)

# Clean Env ####
rm(list = c("allo_lead", "leni_allo", "leni_allo_rel_lin", "final1", "final2", "plot"))