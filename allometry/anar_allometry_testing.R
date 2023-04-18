## ANAR Allometry

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


anar_allo <- read.csv(paste0(allo_lead, "ANAR-flowers_allometry-processing_20221213.csv")) 

anar_seeds <- read.csv(paste0(allo_lead, "ANAR-seeds_allometry-processing_20221213.csv"))


# Seeds per Flower ####
## Seed Distrib ####
ggplot(anar_seeds, aes(x=seed.num)) +
  geom_histogram()

## separate by treatment
ggplot(anar_seeds, aes(x=seed.num)) +
  geom_histogram()+
  facet_wrap(~Treatment)

## look at sample num per treatment
nrow(anar_seeds[anar_seeds$Treatment == "D",]) ## 39
nrow(anar_seeds[anar_seeds$Treatment == "C",]) ## 46


## Calc Seeds per Flower ####
## calc overall mean
anar_mean_seeds <- mean(anar_seeds$seed.num, na.rm = T)
anar_mean_seeds ## 12.34

## calc mean by treatment
anar_seed_means <- anar_seeds %>%
  group_by(Treatment) %>%
  summarise(mean_seeds = mean(seed.num, na.rm = T), SE_seeds = calcSE(seed.num))

## plot this
ggplot(anar_seed_means, aes(x=Treatment, y = mean_seeds)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_seeds - SE_seeds, ymax = mean_seeds + SE_seeds), width = 0.25) +
  ylab("ANAR Mean Seeds per Flower") + xlab ("Treatment")

#ggsave("ANAR_seeds_per_flower.png", width = 3, height = 3)

## use an anova to test signif differences b/w categories
seedtrt <- aov(seed.num ~ Treatment, data = anar_seeds)

summary(seedtrt)
TukeyHSD(seedtrt) # no sig diff, use mean seeds

## create df for saving output
anar_seed_means <- anar_seeds %>%
  summarise(mean_seeds = mean(seed.num, na.rm = T), SE_seeds = calcSE(seed.num))

anar_seed_means <- cbind(treatment = c("C", "D"), anar_seed_means)


# TotBio - Flower ####
## Combine drought and controls together for biomass-flower relationship

## Visualize ####
### Linear ####
ggplot(anar_allo, aes(x = total.biomass.g, y = flower.num)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25, size = 0.75, formula = y ~ x)

# ggplot(anar_allo, aes(x = total.biomass.g, y = flower.num, col = treatment)) +
#   geom_point() +
#   geom_smooth(method = "lm", alpha = 0.25, size = 0.75, formula = y ~ x)

anar_allo[anar_allo$total.biomass.g > 5,]$flower.num
## 419 flowers on the largest sample.

## plot as linear w/o outlier
ggplot(anar_allo[anar_allo$total.biomass.g<5,], aes(x = total.biomass.g, y = flower.num)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25, size = 0.75) +
  ylab("ANAR Flower Number") + xlab("Total AG Biomass (g)")

### Poly ####
## plot as polynomial
ggplot(anar_allo, aes(x = total.biomass.g, y = flower.num)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25, size = 0.75, formula = y ~ poly(x, 2)) +
  ylab("ANAR Flower Number") + xlab("Total AG Biomass (g)")

#ggsave("anar_flower_poly_outlier.png", width = 3, height = 3)

## plot as polynomial w/o outlier
ggplot(anar_allo[anar_allo$total.biomass.g<5,], aes(x = total.biomass.g, y = flower.num)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25, size = 0.75, formula = y ~ poly(x, 2)) +
  ylab("ANAR Flower Number") + xlab("Total AG Biomass (g)")

#ggsave("anar_flower_poly_no_outlier.png", width = 3, height = 3)

## model ####
### *linear ####
anar_fallo_lin <- lm(flower.num ~ total.biomass.g, data = anar_allo)
summary(anar_fallo_lin) # r2 = 0.9314

### poly ####
anar_fallo_poly <- lm(flower.num ~ total.biomass.g + I(total.biomass.g^2), data = anar_allo)
summary(anar_fallo_poly) # r2 = 0.9343

### No outlier ####
#### Linear ####
anar_fallo_lin_no <- lm(flower.num ~ total.biomass.g, data = anar_allo[anar_allo$total.biomass.g<5,])
summary(anar_fallo_lin_no) # r2 = 0.774

#### Poly ####
anar_fallo_poly_no <- lm(flower.num ~ total.biomass.g + I(total.biomass.g^2), data = anar_allo[anar_allo$total.biomass.g<5,])
summary(anar_fallo_poly_no) # r2 = 0.8296

# Save Output ####
## save the model outputs
ANAR.allo.output <- data.frame(Species = "ANAR", 
           intercept = 0, 
           intercept_pval = NA, 
           intercept_se = NA, 
           
           slope = anar_fallo_lin$coefficients[2], 
           slope_pval = summary(anar_fallo_lin)$coefficients[2,4], 
           slope_se = summary(anar_fallo_lin)$coefficients[2,2], 
           
           seeds_C = anar_seed_means[anar_seed_means$treatment == "C",]$mean_seeds,
           seeds_C_se = anar_seed_means[anar_seed_means$treatment == "C",]$SE_seeds,
           seeds_D = anar_seed_means[anar_seed_means$treatment == "D",]$mean_seeds,
           seeds_D_se = anar_seed_means[anar_seed_means$treatment == "D",]$SE_seeds,
           
           viability_C = NA,
           viability_C_se = NA,
           viability_D = NA,
           viability_D_se = NA)

rm(list = c("allo_lead", "anar_fallo_lin_no", "anar_fallo_poly_no", "anar_fallo_poly", "anar_fallo_lin", "anar_allo",  "anar_seeds", "anar_mean_seeds", "seedtrt", "anar_seed_means"))
