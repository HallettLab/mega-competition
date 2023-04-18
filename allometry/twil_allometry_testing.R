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
twil_flowers_allo <- read.csv(paste0(allo_lead, "TWIL-flowers_allometry-processing_20230206.csv"))

twil_viability_allo <- read.csv(paste0(allo_lead, "TWIL-viability_allometry-processing_20230206.csv"))

twil_seeds_allo <- read.csv(paste0(allo_lead, "TWIL-seeds_allometry-processing_20230206.csv"))

## Processing data

## phyto-processing data
## specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/"
  
} else {
  # Marina
  lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/"
} 

twil_phyto <- read.csv(paste0(lead, "TWIL_phyto-processing-redo_20230206.csv")) %>%
  mutate(final.total.biomass.g = ifelse(!is.na(redo.total.biomass.g), redo.total.biomass.g, total.biomass.g))


# Cleaning ####
## Check that weights match redo.weights
twil_allo_phytos <- twil_flowers_allo %>%
  filter(!is.na(sub)) %>%
  mutate(allo.total.biomass = total.biomass.g) %>%
  select(block, plot, sub, allo.total.biomass, total.flower.num)

twil_combined <- left_join(twil_allo_phytos, twil_phyto, by = c("block", "plot", "sub"))

ggplot(twil_combined, aes(x=final.total.biomass.g, y=allo.total.biomass)) +
  geom_point() +
  geom_abline(slope = 1)
## one point is off the 1:1 line 
    ## corrected this now, looks good

twil_mismatch <- twil_combined %>%
  filter(final.total.biomass.g != allo.total.biomass) %>%
  select(block, plot, sub, allo.total.biomass, final.total.biomass.g, redo.total.biomass.g, total.biomass.g)

## 14-30-11 has a decently large mismatch: it was one of the original allometry samples and weighed at 0.131. During phyto processing it was weighed as 0.0083. It may have lost biomass during allo processing or one scale is off. Either way, need to reweigh.
    ## Fixed

## the other 4 samples are off by slight amounts that are likely due to use of diff scales.

## 14-36-11 is just big enough of a diff that we should use the redo.total.biomass for this one. 
twil_flowers_allo[twil_flowers_allo$block == 14 & twil_flowers_allo$plot == 36,]$total.biomass.g <- 0.251


# Bio-Flower Rel. ####
## Visualize ####
### linear ####
ggplot(twil_flowers_allo, aes(x=total.biomass.g, y=total.flower.num)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(twil_flowers_allo, aes(x=total.biomass.g, y=total.flower.num, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm")
## lots of overlap between treatments.

### polynomial ####
ggplot(twil_flowers_allo, aes(x = total.biomass.g, y = total.flower.num)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25, linewidth = 0.75, formula = y ~ poly(x, 2)) +
  ggtitle("TWIL poly")
#ggsave("allometry/preliminary_figs/TWIL_flowers_poly.png", width = 4, height = 2.5)

ggplot(twil_flowers_allo, aes(x = total.biomass.g, y = total.flower.num, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25, linewidth = 0.75, formula = y ~ poly(x, 2))


## Model ####
twil_fallo_lin <- lm(total.flower.num~total.biomass.g, data = twil_flowers_allo)
summary(twil_fallo_lin)
## r2 = 0.9699

twil_fallo_pol <- lm(total.flower.num ~ total.biomass.g + I(total.biomass.g^2), data = twil_flowers_allo)
summary(twil_fallo_pol) 
# r2 = 0.9703


# Viability ####
## summarise ####
twil_viability_sum <- twil_viability_allo %>%
  group_by(treatment) %>%
  summarise(mean.via = mean(viability), se.via = calcSE(viability), reps = n())

mean_viability <- twil_viability_allo %>%
  summarise(mean.via = mean(viability), se.via = calcSE(viability))

## visualize ####
ggplot(twil_viability_sum, aes(x=treatment, y=mean.via)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean.via - se.via, ymax = mean.via + se.via), width = 0.25) +
  ggtitle("TWIL")
## error bars are overlapping a lot, probably not a significant difference here

ggsave("allometry/preliminary_figs/twil_mean_via.png", width = 3, height = 2)

## test ####
viability_test <- aov(viability~treatment, data = twil_viability_allo)
summary(viability_test)
## not significantly different, use an overall mean 

## check viability-bio rel ####
viability_bio <- merge(twil_viability_allo, twil_flowers_allo[,6:9], by.x="flower.rep", by.y = "rep")

ggplot(viability_bio, aes(x=total.biomass.g, y=viability)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("TWIL") +
  ylab("prop viable flowers")

#ggsave("allometry/preliminary_figs/twil_via_bio.png", width = 4, height = 2.5)

vb.model <- lm(viability~total.biomass.g, data = viability_bio)
summary(vb.model)
## marginally significant

# Seeds ####
twil_seed_means <- twil_seeds_allo %>%
  group_by(treatment) %>%
  summarise(mean.seeds = mean(seeds.per.flower), se.seeds = calcSE(seeds.per.flower), reps = n())

seed_overall_mean <- twil_seeds_allo %>%
  summarise(mean.via = mean(seeds.per.flower), se.via = calcSE(seeds.per.flower), reps = n())

## visualize ####
ggplot(twil_seed_means, aes(x=treatment, y=mean.seeds)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean.seeds - se.seeds, ymax = mean.seeds + se.seeds), width = 0.25) +
  ggtitle("TWIL") +
  ylab("mean.seeds.per.flower")
## same mean

ggsave("allometry/preliminary_figs/twil_seed_means.png", width = 3, height = 2)

## test ####
seed_test <- aov(seeds.per.flower~treatment, data = twil_seeds_allo)
summary(seed_test)
## not significantly different, use an overall mean 


# Save output ####
## save the model outputs
TWIL.allo.output <- data.frame(Species = "TWIL", 
                               intercept = 0, 
                               intercept_pval = NA, 
                               intercept_se = NA, 
                               
                               slope = twil_fallo_lin$coefficients[2], 
                               slope_pval = summary(twil_fallo_lin)$coefficients[2,4], 
                               slope_se = summary(twil_fallo_lin)$coefficients[2,2], 
                               
                               seeds_C = seed_overall_mean[,1],
                               seeds_C_se = seed_overall_mean[,2],
                               seeds_D = seed_overall_mean[,1],
                               seeds_D_se = seed_overall_mean[,2],
                               
                               viability_C = mean_viability[,1],
                               viability_C_se = mean_viability[,2],
                               viability_D = mean_viability[,1],
                               viability_D_se = mean_viability[,2])


## clean environment 
rm(list = c("allo_lead", "calcSE", "lead", "mean_viability", "seed_overall_mean", "seed_test", "twil_allo_phytos", "twil_combined", "twil_fallo_lin", "twil_fallo_pol", "twil_flowers_allo", "twil_mismatch", "twil_phyto", "twil_seed_means", "twil_seeds_allo", "twil_viability_allo", "twil_viability_sum", "viability_test", "viability_bio", "vb.model"))