## AMME Allometry

## Set up env
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

## Allometry data
amme_flowers_allo <- read.csv(paste0(allo_lead, "AMME-flowers_allometry-processing_20230202.csv"))

amme_seeds_allo <- read.csv(paste0(allo_lead, "AMME-seeds_allometry-processing_20230202.csv"))


nrow(amme_flowers_allo[amme_flowers_allo$treatment == "D",]) ## 20
nrow(amme_flowers_allo[amme_flowers_allo$treatment == "C",]) ## 20

# Visualize ####
## flowers ####
final3 <- ggplot(amme_flowers_allo, aes(x=total.biomass.g)) +
  geom_histogram() +
  xlab("Aboveground Biomass (g)") +
  ylab("Count")

final4 <- ggplot(amme_flowers_allo, aes(x=total.biomass.g, y=flower.num)) +
  geom_point() +
  geom_smooth(method = "lm",alpha = 0.25, linewidth = 0.75, formula = y ~ x) +
  xlab("Aboveground Biomass (g)") +
  ylab("Flower Number")
## visually an okay fit, not great

ggplot(amme_flowers_allo, aes(x=total.biomass.g, y=flower.num, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm")
## these are somewhat different, but mostly due to one larger control sample

#ggplot(amme_flowers_allo, aes(x=total.biomass.g, y=flower.num)) +
 # geom_point() +
  #geom_smooth(method = "lm", alpha = 0.25, linewidth = 0.75, formula = y ~ poly(x, 2))
## better than the linear fit

#ggplot(amme_flowers_allo, aes(x=total.biomass.g, y=flower.num, color = treatment)) +
 # geom_point() +
  #geom_smooth(method = "lm", alpha = 0.25, linewidth = 0.75, formula = y ~ poly(x, 2))
## better, but again the relationships are so different due to one C endpoint

## seeds ####
nrow(amme_seeds_allo[amme_seeds_allo$treatment == "D",]) ## 30
nrow(amme_seeds_allo[amme_seeds_allo$treatment == "C",]) ## 30

final1 <- ggplot(amme_seeds_allo, aes(x=seeds.num)) +
  geom_histogram() +
  xlab("Seeds per Flower") +
  ylab("Count")

amme_seed_means <- amme_seeds_allo %>%
  group_by(treatment) %>%
  summarise(mean.seeds = mean(seeds.num), se.seeds = calcSE(seeds.num))

final2 <- ggplot(amme_seed_means, aes(x=treatment, y=mean.seeds)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin= mean.seeds-se.seeds, ymax = mean.seeds + se.seeds), width = 0.25) +
  xlab("Precipitation Treatment") +
  ylab("Seeds per Flower")
## not different

# Models ####
## flowers ####
### linear ####
amme_allo_lin <- lm(flower.num~total.biomass.g, data = amme_flowers_allo)
summary(amme_allo_lin)
## R2 = 0.8764

amme_allo_lin2 <- lm(flower.num~total.biomass.g + treatment, data = amme_flowers_allo)
summary(amme_allo_lin2)
## R2 = 0.8775

anova(amme_allo_lin,
      amme_allo_lin2)

### polynomial ####
#amme_allo_pol <- lm(flower.num ~ total.biomass.g + I(total.biomass.g^2), data = amme_flowers_allo)
#summary(amme_allo_pol)
## R2 = 0.8769

#amme_allo_pol2 <- lm(flower.num ~ total.biomass.g + I(total.biomass.g^2) + treatment, data = amme_flowers_allo)
#summary(amme_allo_pol2)
## R2 = 0.878


## seeds ####
amme_seeds <- aov(seeds.num~treatment, data = amme_seeds_allo)
summary(amme_seeds)
## NO significant effect of treatment, use overall mean

# Methods Figure ####
plot <- ggarrange(final1, final2, final3, final4, labels = "AUTO", ncol = 2, nrow = 2)

annotate_figure(plot, top = text_grob("AMME", 
                                      color = "black", face = "bold", size = 14))

ggsave("allometry/methods_figures/AMME.png", height = 5, width = 6.5)

# Save model outputs ####
AMME.allo.output <- data.frame(Species = "AMME", 
                               intercept = 0, 
                               intercept_pval = NA, 
                               intercept_se = NA, 
                
                               slope = amme_allo_lin$coefficients[2], 
                               slope_pval = summary(amme_allo_lin)$coefficients[2,4], 
                               slope_se = summary(amme_allo_lin)$coefficients[2,2], 
                               
                               seeds_C = amme_seed_means[amme_seed_means$treatment == "C",]$mean.seeds,
                               seeds_C_se = amme_seed_means[amme_seed_means$treatment == "C",]$se.seeds,
                               seeds_D = amme_seed_means[amme_seed_means$treatment == "D",]$mean.seeds,
                               seeds_D_se = amme_seed_means[amme_seed_means$treatment == "D",]$se.seeds, 
                               
                               viability_C = NA,
                               viability_C_se = NA,
                               viability_D = NA,
                               viability_D_se = NA)


rm(list = c("allo_lead", "amme_flowers_allo", "amme_seed_means", "amme_seeds_allo",  "amme_allo_lin", "amme_allo_lin2", "amme_seeds", "final1", "final2", "final3", "final4", "plot"))