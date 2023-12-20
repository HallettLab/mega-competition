## PLNO Allometry

## Set up env
library(tidyverse)
library(ggpubr)
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
final3 <- ggplot(plno_flowers_allo, aes(x=total.biomass.g)) +
  geom_histogram() +
  xlab("Aboveground Biomass (g)") +
  ylab("Count")

final4 <- ggplot(plno_flowers_allo, aes(x=total.biomass.g, y=flower.num)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25, linewidth = 0.75, formula = y ~ x)
## visually doesn't look like a great fit

ggplot(plno_flowers_allo, aes(x=total.biomass.g, y=flower.num, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm")
## these are pretty different

#ggplot(plno_flowers_allo, aes(x=total.biomass.g, y=flower.num)) +
 # geom_point() +
  #geom_smooth(method = "lm", alpha = 0.25, linewidth = 0.75, formula = y ~ poly(x, 2))
## better than the linear fit

#ggplot(plno_flowers_allo, aes(x=total.biomass.g, y=flower.num, color = treatment)) +
 # geom_point() +
  #geom_smooth(method = "lm", alpha = 0.25, linewidth = 0.75, formula = y ~ poly(x, 2))
## better than linear but now the error bars overlap just a bit.

## seeds ####
final1 <- ggplot(plno_seeds_allo, aes(x=seeds.num)) +
  geom_histogram() +
  facet_wrap(~treatment) +
  xlab("Seeds per Flower") +
  ylab("Count")

plno_seed_means <- plno_seeds_allo %>%
  group_by(treatment) %>%
  summarise(mean.seeds = mean(seeds.num), se.seeds = calcSE(seeds.num))

nrow(plno_seeds_allo[plno_seeds_allo$treatment == "C",])
nrow(plno_seeds_allo[plno_seeds_allo$treatment == "D",])

final2 <- ggplot(plno_seed_means, aes(x=treatment, y=mean.seeds)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin= mean.seeds-se.seeds, ymax = mean.seeds + se.seeds), width = 0.25) +
  xlab("Precipitation Treatment") +
  ylab("Seeds per Flower")
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

anova(plno_allo_lin, 
      plno_allo_lin2)

### polynomial ####
#plno_allo_pol <- lm(flower.num ~ total.biomass.g + I(total.biomass.g^2), data = plno_flowers_allo)
#summary(plno_allo_pol)
## R2 = 0.9156

#plno_allo_pol2 <- lm(flower.num ~ total.biomass.g + I(total.biomass.g^2) + treatment, data = plno_flowers_allo)
#summary(plno_allo_pol2)
## R2 = 0.9249

## seeds ####
plno_seeds <- aov(seeds.num~treatment, data = plno_seeds_allo)
summary(plno_seeds)
## significant effect of treatment, use separate numbers

# Methods Figure ####
plot <- ggarrange(final1, final2, final3, final4, labels = "AUTO", ncol = 2, nrow = 2)

annotate_figure(plot, top = text_grob("PLNO", 
                                      color = "black", face = "bold", size = 14))

ggsave("allometry/methods_figures/PLNO.png", height = 5, width = 6.5)

# Save Output ####
## save the model outputs
PLNO.allo.output <- data.frame(Species = "PLNO", 
                               intercept = 0, 
                               intercept_pval = NA, 
                               intercept_se = NA, 

                               slope = plno_allo_lin$coefficients[2], 
                               slope_pval = summary(plno_allo_lin)$coefficients[2,4], 
                               slope_se = summary(plno_allo_lin)$coefficients[2,2], 
 
                               seeds_C = plno_seed_means[plno_seed_means$treatment == "C",]$mean.seeds,
                               seeds_C_se = plno_seed_means[plno_seed_means$treatment == "C",]$se.seeds,
                               seeds_D = plno_seed_means[plno_seed_means$treatment == "D",]$mean.seeds,
                               seeds_D_se = plno_seed_means[plno_seed_means$treatment == "D",]$se.seeds, 
                               
                               viability_C = NA,
                               viability_C_se = NA,
                               viability_D = NA,
                               viability_D_se = NA)

rm(list = c("allo_lead", "plno_flowers_allo", "plno_seed_means", "plno_seeds_allo",  "plno_allo_lin", "plno_allo_lin2", "plno_seeds", "final1", "final2", "final3", "final4", "plot"))