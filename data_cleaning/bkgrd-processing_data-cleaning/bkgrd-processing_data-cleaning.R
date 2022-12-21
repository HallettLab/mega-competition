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
bg_indiv <- read.csv(paste0(lead, "bkgrd-processing_20221221.csv"))

## Allometric Relationships
source("allometry/merge_allometric_relationships.R")


# Clean Data ####

## Finished Sp ####
## separate out finished species
finished <- c("BRHO", "GITR", "AVBA", "PLER", "ACAM", "ANAR", "MAEL")

## Allo Types ####
## separate by types of allometric relationships
totbio.to.seeds <- c("MICA", "LOMU", "THIR-I", "TWIL-I", "TACA")
inflor.bio.to.seeds <- c("BRHO", "PLER")
stem.to.seeds <- c("LENI")
bio.to.flower.to.seeds <- c("ACAM", "ANAR", "GITR")
seeds.per.flower <- c("MAEL", "CESO")
seeds <- c("AVBA")

## make a vector of drought blocks
drought <- c(1, 3, 4, 6, 12, 14)

## Make Mods ####
## clean up bg data
bg_indivC <- bg_indiv %>%
  filter(plot < 43) %>% ## get rid of inoc subexperiment
  mutate(across(where(is.character), str_trim)) %>%
  mutate_all(na_if,"") %>% ## make blank values NAs
  select(-date.collect, -initials) %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C")) ## add treatment column

## check structure
str(bg_indivC)
    ## flower.num is a chr
unique(bg_indivC$flower.num) ## one value is "F"

### Change Val ####
bg_indivC[bg_indivC$block == 16 & bg_indivC$plot == 17,]$flower.num <- NA
## change this flower.num val from "F" to NA

bg_indivC$flower.num <- as.numeric(bg_indivC$flower.num)


## Calc Avg Indiv ####
bg.ind <- bg_indivC %>%
  mutate(avg.ind = ifelse(bkgrd %in% stem.to.seeds, total.stem.length.mm/n.indiv, 
                          ifelse(bkgrd %in% inflor.bio.to.seeds, inflor.g/n.indiv, 
                                 ifelse(bkgrd %in% seeds, glume.num/n.indiv, 
                                        ifelse(bkgrd %in% seeds.per.flower, flower.num/n.indiv, total.biomass.g/n.indiv))))) ## Calc the avg bg indiv, use the most appropriate measurement for each species
  
  
# Calc Avg Seed Output ####
## make a vector of finished bg plots
bg.sp <- unique(bg.ind[bg.ind$bkgrd %in% finished,]$bkgrd)

## make an empty dataframe for the output
bg.ind.avg <- data.frame(treatment = NA, block = NA, plot = NA, bkgrd = NA, dens = NA, bg.avg.seed.num = NA)

## for each unique background species
for (i in 1:length(bg.sp)){
  
  ## filter species
  tmp.sp <- bg.ind[bg.ind$bkgrd == bg.sp[i],]
  
  ## filter model results
  tmp.model <- allo.df[allo.df$Species == bg.sp[i],]
  
  ## separate by allo-rel type and calc avg seeds out per indiv
  if (bg.sp[i] %in% totbio.to.seeds | bg.sp[i] %in% inflor.bio.to.seeds | bg.sp[i] %in% stem.to.seeds) {
    
    tmp.ind <- tmp.sp %>%
      mutate(bg.avg.seed.num = (tmp.model$intercept + ((tmp.model$slope)*avg.ind + (tmp.model$poly*(avg.ind^2))))) %>%
      select(treatment, block, plot, bkgrd, dens, bg.avg.seed.num)
    
  }
  
  else if (bg.sp[i] %in% bio.to.flower.to.seeds) {
    
    tmp.ind <- tmp.sp %>%
      mutate(avg.flower.num = (tmp.model$intercept + ((tmp.model$slope)*avg.ind) + (tmp.model$poly*(avg.ind^2))),
             bg.avg.seed.num = ifelse(treatment == "D", avg.flower.num*tmp.model$seeds_D, avg.flower.num*tmp.model$seeds_C)) %>%
      select(treatment, block, plot, bkgrd, dens, bg.avg.seed.num)

  }
  
  else if (bg.sp[i] %in% seeds.per.flower) {
    
    tmp.ind <- tmp.sp %>%
      mutate(bg.avg.seed.num = ifelse(treatment == "D", avg.ind*tmp.model$seeds_D, avg.ind*tmp.model$seeds_C)) %>%
      select(treatment, block, plot, bkgrd, dens, bg.avg.seed.num)
    
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
rm("bg_indiv", "bg.ind", "bg.sp", "bio.to.flower.to.seeds", "finished", "seeds", "tmp.ind", "tmp.sp", "tmp.model", "lead", "totbio.to.seeds", "inflor.bio.to.seeds", "stem.to.seeds", "bg.ind.avg", "bg_indivC")
