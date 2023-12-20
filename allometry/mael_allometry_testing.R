## MAEL Allometry Testing

# set up env
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
mael_allo <- read.csv(paste0(allo_lead, "MAEL_allometry-processing_20221128.csv")) %>%
  select(-X, -X.1, -X.2)

# Seed Distrib ####
ggplot(mael_allo, aes(x = seeds.num)) +
  geom_histogram()

nrow(mael_allo[mael_allo$treatment == "C",])
nrow(mael_allo[mael_allo$treatment == "D",])


## separate by treatment
final1 <- ggplot(mael_allo, aes(x = seeds.num)) +
  geom_histogram()+
  facet_wrap(~treatment) +
  xlab("Seeds per Flower") +
  ylab("Count")

# Calc Seeds/Flower ####
## calc overall mean
mean(mael_allo$seeds.num, na.rm = T)
## 12.55

## calc mean by treatment
mael_seed_means <- mael_allo %>%
  group_by(treatment) %>%
  summarise(mean_seeds = mean(seeds.num, na.rm = T), SE_seeds = calcSE(seeds.num))

## plot this
final2 <- ggplot(mael_seed_means, aes(x=treatment, y = mean_seeds)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_seeds - SE_seeds, ymax = mean_seeds + SE_seeds), width = 0.25) +
  ylab("Seeds per Flower") + xlab ("Precipitation Treatment")

## use an anova to test signif differences b/w categories
seedtrt <- aov(seeds.num ~ treatment, data = mael_allo)
summary(seedtrt)
TukeyHSD(seedtrt)

## Use separate means for drought & control since they are significantly different

# Methods Figure ####
plot <- ggarrange(final1, final2, labels = "AUTO", ncol = 2, nrow = 1)

annotate_figure(plot, top = text_grob("MAEL", 
                                      color = "black", face = "bold", size = 14))

ggsave("allometry/methods_figures/MAEL.png", height = 3, width = 6.5)

# Save Output ####
MAEL.allo.output <- data.frame(Species = "MAEL", 
           intercept = 0, 
           intercept_pval = NA, 
           intercept_se = NA, 
           
           slope = NA, 
           slope_pval = NA, 
           slope_se = NA, 
           
           seeds_C = mael_seed_means[mael_seed_means$treatment == "C",]$mean_seeds,
           seeds_C_se = mael_seed_means[mael_seed_means$treatment == "C",]$SE_seeds,
           seeds_D = mael_seed_means[mael_seed_means$treatment == "D",]$mean_seeds,
           seeds_D_se = mael_seed_means[mael_seed_means$treatment == "D",]$SE_seeds, 
           
           viability_C = NA,
           viability_C_se = NA,
           viability_D = NA,
           viability_D_se = NA)

rm(mael_allo, seedtrt, allo_lead, mael_seed_means, final1, final2, plot)
