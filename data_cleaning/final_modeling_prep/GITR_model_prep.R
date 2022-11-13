## GITR Model Prep ##

## the purpose of this script is to make the final preparations for modeling
## 1. isolate only necessary columns
## 2. calc phyto seed output

# Read in Data ####
## read in clean phyto data
source("data_cleaning/mid_level_QAQC/merged_QAQC_GITR.R")

## read in allometric relationships
source("allometry/merge_allometric_relationships.R")

## read in background data
source("data_cleaning/initial_data_prep/bkgrd-processing_data-cleaning.R")

# Merge in avg BG seed output ####
gitrP.w.BG <- left_join(gitr, bg.seeds, by = c("treatment", "block", "plot", "bkgrd", "dens"))

## get GITR ready for modeling
gitr.model <- gitrP.w.BG %>%
  mutate(flowers.out = (allo.df[allo.df$species == "GITR",2] + 
                             (allo.df[allo.df$species == "GITR",3]*total.biomass.rounded.percap) +
                             (allo.df[allo.df$species == "GITR",4]*total.biomass.rounded.percap^2))*phyto.n.indiv,
         GITR.seed.out = ifelse(treatment == "D", flowers.out*8.701754, flowers.out*11.640625),
       #  bkgrd.seed.out = bkgrd.n.indiv*bg.avg.seed.num, 
         phyto.seed.in = 3)
#,
        # bkgrd.stem.in = bkgrd.n.indiv) %>%
  #select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.unique, phyto.n.indiv, phyto.seed.in, phyto.seed.out, bkgrd.stem.in, bkgrd.seed.out, unique.ID)

