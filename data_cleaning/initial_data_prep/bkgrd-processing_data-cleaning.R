## Background Data Cleaning

## The purpose of this script is to: 
    ## 1. clean background data up
    ## 2. calculate avg. background individual
    ## 3. calculate avg background indiv seed output using allometric relationships

# set up env
library(tidyverse)

# Read in Data ####
# specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Background-Processing/Background-Processing_entered/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Background-Processing/Background-Processing_entered/"
  
} else {
  # Marina
  lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Processing/Background-Processing/Background-Processing_entered/"
} 

## Background Data
bg_indiv <- read.csv(paste0(lead, "bkgrd-processing_20221102.csv"))

## Allometric Relationships
source("allometry/merge_allometric_relationships.R")


# Clean Data ####
drought <- c(1, 3, 4, 6, 12, 14)

ggplot(bg_indiv, aes(x=n.indiv)) +
  geom_histogram() +
  facet_wrap(~bkgrd)
unique(bg_indiv$bkgrd)
bg_indiv[bg_indiv$bkgrd == "",]


bg.ind <- bg_indiv %>%
  filter(plot < 43) %>% ## get rid of inoc subexperiment
  mutate(avg.ind = ifelse(bkgrd == "LENI", total.stem.length.mm/n.indiv, total.biomass.g/n.indiv)) %>% ## Calc the avg bg indiv
  select(-date.collect, -initials) %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C")) ## add treatment column

## at some point we may need a different column than the 'avg individual?'

## I think we should calculate the background seed output here, otherwise we will need to repeat this for each phyto species


# Calc Avg seeds out ####
## GITR ####
## filter gitr bgs 
gitr.bg <- bg.ind %>%
  filter(bkgrd == "GITR")

## multiply avg ind by allo relationship to calc seed output of avg ind
gitr.bg.SO <- gitr.bg %>%
  mutate(avg.flower.num = (gitr.allo.output[1] + (gitr.allo.output[2]*avg.ind) + (gitr.allo.output[3]*(avg.ind^2))),
         avg.seed.num = ifelse(treatment == "D", avg.flower.num*8.701754, avg.flower.num*11.640625))

rm("gitr.bg")

## BRHO ####


## EVENTUALLY: 
## probably want this in one dataframe

# Clean Env ####
rm("bg_indiv")

# In development ####
# when all allo rel are complete we can probably do this by for loop?

## 1 rel species ####
## BRHO, LOMU, TACA, LENI, MICA, PLER etc.

#bg.sp <- unique(bg.ind$bkgrd)

## for each background species, 
#for (i in 1:length(bg.sp)) {

## filter species
# tmp <- bg.ind %>%
#   filter(bkgrd == bg.sp[i])

## get correct allo rel

## calc seed output
# seedtmp <- tmp %>%
#  mutate(avg.seed.num = )


#}

## 2 rel species ####
## GITR, ACAM, ANAR

