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

bg.ind <- bg_indiv %>%
  filter(plot < 43) %>% ## get rid of inoc subexperiment
  mutate(avg.ind = ifelse(bkgrd == "LENI", total.stem.length.mm/n.indiv, total.biomass.g/n.indiv)) %>% ## Calc the avg bg indiv
  select(-date.collect, -initials) %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C")) ## add treatment column

## at some point we may need a different column than the 'avg individual?'

## I think we should calculate the background seed output here, otherwise we will need to repeat this for each phyto species

# Calc Avg BG seed output
## BG seeds ####
## calc BG seed output 
## filter gitr bgs 
gitr.bg <- bg.ind %>%
  filter(bkgrd == "GITR")
## multiply avg ind size by allo relationship to calc the seed output of the avg ind
gitr.bg.SO <- gitr.bg %>%
  mutate(avg.flower.num = (0.3267 + (77.6127*avg.ind) - (7.1135*(avg.ind^2))),
         avg.seed.num = ifelse(treatment == "D", avg.flower.num*8.701754, avg.flower.num*11.640625))


rm("bg_indiv")
