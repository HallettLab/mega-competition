library(tidyverse)
library(ggpubr)

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

# Read in Data ####
## Processing data
source("data_cleaning/merge_processing_collections_data.R")

# specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Allometry/Allometry_entered/")){
  # Carmen
  allo_lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Allometry/Allometry_entered/"
  
} else {
  # Marina
  allo_lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Allometry/Allometry_entered/"
} 

date <- 20221019

## Allometry data
brho_allo <- read.csv(paste0(allo_lead, "BRHO_allometry-processing_", date, ".csv")) %>%
  mutate(inflor.g = inflorescence.weight.g, seed.num = seeds.num) %>% ## standardize column names
  mutate(treatment = ifelse(treatment == "ambient", "C", "D")) ## standardize treatment values


# Dat Range ####
theme_set(theme_bw())

brho_dat <- all_dat_final %>%
  filter(phyto == "BRHO")

phyto <- ggplot(brho_dat, aes(x=inflor.g.rounded.percap)) +
  geom_histogram() +
  facet_wrap(~treatment)+
  coord_cartesian(xlim = c(0,2))

allo <- ggplot(brho_allo, aes(x=inflor.g)) +
  geom_histogram() +
  facet_wrap(~treatment) +
  coord_cartesian(xlim = c(0,2))

ggarrange(phyto, allo, ncol = 1, nrow=2)

#ggsave("allometry/preliminary_figs/allometric_relationship_fits/brho_allometry_check.png", height = 4, width = 6)


## To-Do ####
  ## Looks like BRHO control could use a few more samples in the lower range.


# Inflor-Seed Rel. ####
## Visualize ####
ggplot(brho_allo, aes(x=inflor.g, y=seed.num, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm")
## the error bars on the lines overlap for most of it - so probably not separate relationships here

ggplot(brho_allo, aes(x=inflor.g, y=seed.num)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(brho_allo, aes(x=inflor.g, y=seed.num)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x,2))

ggplot(brho_allo, aes(x=inflor.g, y=seed.num)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)

ggplot(brho_allo, aes(x=inflor.g, y=seed.num)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ log(x))


## Model ####
inflorseeds <- lm(seed.num~inflor.g, data = brho_allo)
summary(inflorseeds)
## slope = 951.7297
# y = 0.7543 + 951.7297x

inflorseeds2 <- lm(seed.num ~ inflor.g + I(inflor.g^2), data = brho_allo)
summary(inflorseeds2)


# Predict Seed Num ####
brho_final <- brho_dat %>%
  mutate(predicted.seed.num = 0.7543 + 951.7297*inflor.g.rounded.percap)

## Other things to do before data are ready for modeling: 
    ## remove extraneous columns
    ## add in background indiv seed output?
    ## add in background seed input?
    ## phytometer seeds in??
    ## go through notes and make sure census data are fully cleaned, at least in GITR backgrounds for the moment...




