
rm(list = ls())
# Read in Data ####
## read in the phyto seeds out data
source("allometry/brho_allometry_testing.R")
#source("allometry/gitr_allometry_testing.R")

## read in background data
source("data_cleaning/bkgrd-processing_data-cleaning.R")

# Calc Seed Output ####
## phyto seeds ####
# Predict Seed Num ####
gitr_final <- gitr_dat %>%
  mutate(predicted.flower.num = (0.3267 + (77.6127*total.biomass.rounded.percap) - (7.1135*(total.biomass.rounded.percap^2))),
         predicted.seed.num = ifelse(treatment == "D", predicted.flower.num*8.701754, predicted.flower.num*11.640625))




## BG seeds ####
## calc BG seed output 
## filter gitr bgs 
gitr.bg <- bg.ind %>%
  filter(bkgrd == "GITR")
## multiply avg ind size by allo relationship to calc the seed output of the avg ind
gitr.bg.SO <- gitr.bg %>%
  mutate(avg.flower.num = (0.3267 + (77.6127*avg.ind) - (7.1135*(avg.ind^2))),
       avg.seed.num = ifelse(treatment == "D", avg.flower.num*8.701754, avg.flower.num*11.640625))

## multiply this by the bkgrd.n.indiv 

brhoP.gitrBG <- left_join(brho_final[brho_final$bkgrd == "GITR",], gitr.bg.SO[, c(1:5,18)], by = c("block", "plot", "bkgrd", "dens"))

## get BRHO ready for modeling
brho.model <- brhoP.gitrBG %>%
  mutate(phyto.seed.out = predicted.seed.num*phyto.n.indiv, ## should we model 1 phyto or the actual # of phytos?
         bkgrd.seed.out = bkgrd.n.indiv*avg.seed.num, 
         phyto.seed.in = 3,
         bkgrd.stems.in = bkgrd.n.indiv) %>%
  select(block, plot, sub, bkgrd, dens, phyto, phyto.unique, phyto.n.indiv, phyto.seed.in, phyto.seed.out, bkgrd.stems.in, bkgrd.seed.out, unique.ID)

## what do we do when our bg indiv data do not align with the allometric relationship? How will we calculate bg seed ouput? How are we calculating bg seed in? We have it at the plot level, not the subplot...

## might be able to get away with stems in seeds out for bg data?
    ## can calculate seeds out for GITR but probably not for BRHO currently