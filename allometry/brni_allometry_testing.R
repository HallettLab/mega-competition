## BRNI Allometry Testing 

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

## Allometry data
brni_allo <- read.csv(paste0(allo_lead, "BRNI_allometry-processing_20230315.csv"))

brni_seeds <- read.csv(paste0(allo_lead, "BRNI-seeds_allometry-processing_20230315.csv"))


## phyto-processing data
## specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/"
  
} else {
  # Marina
  lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/"
} 

brni.phyto <- read.csv(paste0(lead, "BRNI_phyto-processing_20230315.csv"))




# Clean ####
brni_allo2 <- brni_allo %>%
  mutate(total.flower.num = pod.num + flower.num, ## calc total flowers out
         prop.viable = viable.pod.num/pod.num)

## merge with phyto data for exploration with viability
brni.allo.phyto <- left_join(brni_allo2, brni.phyto, by = c("block", "plot", "sub", "phyto.unique"))


# Bio-Flower Relationship ####
## Visualize ####
### linear ####
ggplot(brni_allo2, aes(x=total.biomass.g, y=total.flower.num)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(brni_allo2, aes(x=total.biomass.g, y=viable.pod.num)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(brni_allo2, aes(x=total.biomass.g, y=total.flower.num, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm")
## the two treatments have pretty different slopes, the D treatment appears largely driven by one large sample though.

### polynomial ####
ggplot(brni_allo2, aes(x = total.biomass.g, y = total.flower.num)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25, linewidth = 0.75, formula = y ~ poly(x, 2)) +
  ggtitle("BRNI poly")
#ggsave("allometry/preliminary_figs/TWIL_flowers_poly.png", width = 4, height = 2.5)

ggplot(brni_allo2, aes(x = total.biomass.g, y = total.flower.num, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0.25, linewidth = 0.75, formula = y ~ poly(x, 2))

## Model ####
### linear ####
brni_allo_lin <- lm(total.flower.num~total.biomass.g, data = brni_allo2)
summary(brni_allo_lin)
## r2 = 0.9425


### poly ####
brni_allo_pol <- lm(total.flower.num ~ total.biomass.g + I(total.biomass.g^2) + treatment, data = brni_allo2)
summary(brni_allo_pol) 
# r2 = 0.9426
## no signif effect of the poly treatment, so use linear

### lin with trt ####
brni_allo_lin_trt <- lm(total.flower.num~total.biomass.g+treatment, data = brni_allo2)
summary(brni_allo_lin_trt)
## r2 = 0.9492



# Viability ####
## visually explore ####

## check distribution
ggplot(brni_allo2, aes(x=prop.viable)) +
  geom_histogram()

## distribution by treatment
ggplot(brni_allo2, aes(x=prop.viable)) +
  geom_histogram() +
  facet_wrap(~treatment)
## control distribution has a peak closer to 0.25; drought distribution is fairly even all the way across...


## viability prop by bio
ggplot(brni_allo2, aes(x=total.biomass.g, y=prop.viable)) +
  geom_point()

## viability prop by bio by treatment
ggplot(brni_allo2, aes(x=total.biomass.g, y=prop.viable, color = treatment)) +
  geom_point()

## viability prop by block
ggplot(brni_allo2, aes(x=block, y=prop.viable)) +
  geom_point()


## viability prop by total flowers
ggplot(brni_allo2, aes(x=total.flower.num, y=prop.viable)) +
  geom_point() +
  geom_smooth(method = "lm") +
  coord_cartesian(ylim = c(0, 1))

## viability prop by bio by leaves present
ggplot(brni.allo.phyto, aes(x=total.biomass.g.x, y=prop.viable)) +
  geom_point() +
  facet_wrap(~leaves.present)# +
# geom_smooth(method = "lm")

## viability prop by bkgrd
ggplot(brni.allo.phyto, aes(x=bkgrd, y=prop.viable)) +
  geom_point()

ggplot(brni.allo.phyto, aes(x=total.biomass.g.x, y=prop.viable)) +
  geom_point() +
  facet_wrap(~bkgrd)

## viability prop is really not related to any measured variables, try viable pod number
    ## by bio and treatment
ggplot(brni.allo.phyto, aes(x=total.biomass.g.x, y=viable.pod.num, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~leaves.present)
    ## by just bio
ggplot(brni.allo.phyto, aes(x=total.biomass.g.x, y=viable.pod.num)) +
  geom_point() +
  geom_smooth(method = "lm")
## looking better 

## explore models ####
viability.test <- lm(prop.viable ~ treatment*block*plot, data = brni_allo2)
summary(viability.test)
viability.test2 <- lm(prop.viable ~ total.flower.num, data = brni_allo2)
summary(viability.test2)
## borderline significant relationship with total.flower.num
viability.test3 <- lm(prop.viable ~ pod.num, data = brni_allo2)
summary(viability.test3)
## not a signficant relationship with pod.num


## tot via flowers by bio ####
### calc tot via flowers ####
viability_calcs <- brni.allo.phyto %>%
  mutate(viable.flowers = flower.num*prop.viable, ## prop.viable is viable pods
         total.viable.flowers = viable.flowers + viable.pod.num)

### model ####
## check viable pods by bio
brni_via_lin <- lm(viable.pod.num~total.biomass.g, data = brni_allo2)
summary(brni_via_lin)
## R2 = 0.6602

#### *tot via flowers by bio ####
brni_via_lin3 <- lm(total.viable.flowers~total.biomass.g.x, data = viability_calcs)
summary(brni_via_lin3)
## R2 = 0.7875
## use this relationship!

### visualize ####
ggplot(viability_calcs, aes(x=total.biomass.g.x, y=total.viable.flowers)) +
  geom_point() +
  geom_smooth(method = "lm")


# Seeds/flower ####
brni_mean_seeds_trt <- brni_seeds %>%
  group_by(treatment) %>%
  summarise(mean_seeds = mean(seeds.per.one.pod), se_seeds = calcSE(seeds.per.one.pod))

brni_mean_seeds <- brni_seeds %>%
  summarise(mean_seeds = mean(seeds.per.one.pod), se_seeds = calcSE(seeds.per.one.pod))

ggplot(brni_mean_seeds_trt, aes(x=treatment, y=mean_seeds)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = mean_seeds - se_seeds, ymax = mean_seeds + se_seeds), width = 0.25)
## interestingly, drought has higher average seeds/pod; some mild overlap of error bars

seed.test <- aov(seeds.per.one.pod ~ treatment, data = brni_seeds)
summary(seed.test)
## no significant effect of treatment, use the same mean for both treatments



# Save output ####
## save the model outputs
BRNI.allo.output <- data.frame(Species = "BRNI", 
                               intercept = 0, 
                               intercept_pval = NA, 
                               intercept_se = NA, 
                               
                               slope = brni_via_lin3$coefficients[2], 
                               slope_pval = summary(brni_via_lin3)$coefficients[2,4], 
                               slope_se = summary(brni_via_lin3)$coefficients[2,2], 
                               poly = NA, 
                               poly_pval = NA, 
                               poly_se = NA,
                               
                               seeds_C = brni_mean_seeds[,1],
                               seeds_C_se = brni_mean_seeds[,2],
                               seeds_D = brni_mean_seeds[,1],
                               seeds_D_se = brni_mean_seeds[,2],
                               
                               viability_C = NA,
                               viability_C_se = NA,
                               viability_D = NA,
                               viability_D_se = NA)

# Clean Env ####
rm(list = c("brni.allo.phyto", "brni.phyto", "brni_allo", "brni_allo_lin", "brni_allo_lin_trt", "brni_allo_pol", "brni_allo2", "brni_mean_seeds", "brni_mean_seeds_trt", "brni_seeds",  "brni_via_lin", "brni_via_lin3", "lead", "seed.test", "viability.test", "viability.test2", "viability.test3", "viability_calcs"))
