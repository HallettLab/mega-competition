## BRHO Model Prep ##

## the purpose of this script is to make the final preparations for modeling
    ## 1. isolate only necessary columns
    ## 2. calc phyto seed output


rm(list = ls())
# Read in Data ####
## read in clean phyto data
source("data_cleaning/mid_level_QAQC/merged_QAQC_BRHO.R")

## read in allometric relationships
source("allometry/merge_allometric_relationships.R")

## read in background data
source("data_cleaning/initial_data_prep/bkgrd-processing_data-cleaning.R")

# Merge in avg BG seed output ####

# Multiply avg BG seed output by # indiv ####

# Calc Phyto Seed Output ####





## BG seeds ####
## calc BG seed output 
## multiply this by the bkgrd.n.indiv 

brhoP.gitrBG <- left_join(brho_final[brho_final$bkgrd == "GITR",], gitr.bg.SO[, c(1:5,18)], by = c("block", "plot", "bkgrd", "dens"))

## get BRHO ready for modeling
brho.model <- brhoP.gitrBG %>%
  mutate(phyto.seed.out = predicted.seed.num*phyto.n.indiv, ## should we model 1 phyto or the actual # of phytos?
         bkgrd.seed.out = bkgrd.n.indiv*avg.seed.num, 
         phyto.seed.in = 3,
         bkgrd.stems.in = bkgrd.n.indiv) %>%
  select(block, plot, sub, bkgrd, dens, phyto, phyto.unique, phyto.n.indiv, phyto.seed.in, phyto.seed.out, bkgrd.stems.in, bkgrd.seed.out, unique.ID)



# Predict Seed Num ####
brho_final <- brho_dat %>%
  mutate(predicted.seed.num = 0.7543 + 951.7297*inflor.g.rounded.percap)

## what do we do when our bg indiv data do not align with the allometric relationship? How will we calculate bg seed ouput? How are we calculating bg seed in? We have it at the plot level, not the subplot...

## might be able to get away with stems in seeds out for bg data?
    ## can calculate seeds out for GITR but probably not for BRHO currently