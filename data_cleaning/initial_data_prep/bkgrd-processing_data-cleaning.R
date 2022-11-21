## Background Data Cleaning

## The purpose of this script is to: 
    ## 1. clean background data up
    ## 2. calculate avg. background individual
    ## 3. calculate avg background indiv seed output using allometric relationships

## Relevant Outputs of this script: 
    ## bg.seeds

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
  mutate(avg.ind = ifelse(bkgrd == "LENI", total.stem.length.mm/n.indiv, 
                          ifelse(bkgrd == "BRHO" | bkgrd == "PLER", inflor.g/n.indiv, 
                                 ifelse(bkgrd == "AVBA", glume.num/n.indiv, total.biomass.g/n.indiv)))) %>% ## Calc the avg bg indiv
  select(-date.collect, -initials) %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C")) ## add treatment column


# Calc Avg Seed Output ####
## separate out finished species
finished <- c("BRHO", "GITR", "AVBA", "PLER")

## separate by types of allometric relationships
bio.to.seeds <- c("BRHO", "MICA", "LENI", "LOMU", "PLER", "THIR-I", "TWIL-I")
bio.to.flower.to.seeds <- c("ACAM", "ANAR", "GITR")
seeds <- c("AVBA")


bg.sp <- unique(bg.ind[bg.ind$bkgrd %in% finished,]$bkgrd)

bg.ind.avg <- data.frame(treatment = NA, block = NA, plot = NA, bkgrd = NA, dens = NA, bg.avg.seed.num = NA)

## for each unique background species
for (i in 1:length(bg.sp)){
  
  ## filter species
  tmp.sp <- bg.ind[bg.ind$bkgrd == bg.sp[i],]
  
  ## filter model results
  tmp.model <- allo.df[allo.df$species == bg.sp[i],]
  
  ## separate by allo-rel type and calc avg seeds out per indiv
  if (bg.sp[i] %in% bio.to.seeds) {
    
    tmp.ind <- tmp.sp %>%
      mutate(bg.avg.seed.num = (tmp.model[1,2] + ((tmp.model[1,3])*avg.ind) + (tmp.model[1,4]*(avg.ind^2)))) %>%
      select(treatment, block, plot, bkgrd, dens, bg.avg.seed.num)
    
  }
  
  else if (bg.sp[i] %in% bio.to.flower.to.seeds) {
    
    tmp.ind <- tmp.sp %>%
      mutate(avg.flower.num = (tmp.model[1,2] + ((tmp.model[1,3])*avg.ind) + (tmp.model[1,4]*(avg.ind^2))),
             bg.avg.seed.num = ifelse(treatment == "D", avg.flower.num*8.701754, avg.flower.num*11.640625)) %>%
      select(treatment, block, plot, bkgrd, dens, bg.avg.seed.num)
  ## currently specific to GITR, change later.
    
  }
  
  else {
    
    tmp.ind <- tmp.sp %>%
      mutate(bg.avg.seed.num = avg.ind*2) %>%
      select(treatment, block, plot, bkgrd, dens, bg.avg.seed.num)
    ## currently specific to AVBA, which has counted glume #

  }
  
  bg.ind.avg <- rbind(bg.ind.avg, tmp.ind)
  
}


bg.seeds <- bg.ind.avg %>%
  filter(!is.na(block)) ## filter out the one row of NAs


# Clean Env ####
rm("bg_indiv", "bg.ind", "bg.sp", "bio.to.flower.to.seeds", "bio.to.seeds", "BRHO.allo.output", "finished", "GITR.allo.output", "seeds", "temp", "tmp.ind", "tmp.sp", "tmp.model", "sp", "lead")