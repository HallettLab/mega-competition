## BRHO Model Prep ##

## the purpose of this script is to make the final preparations for modeling
    ## 1. isolate only necessary columns 
    ## 2. calc phyto seed input/output 

# Read in Data ####
## read in clean phyto data
source("data_cleaning/mid_level_QAQC/merged_QAQC_BRHO.R")

## read in allometric relationships
source("allometry/merge_allometric_relationships.R")

## read in background data
source("data_cleaning/initial_data_prep/bkgrd-processing_data-cleaning.R")

# BRHO phyto df ####
brho.phyto <- brho %>%
  mutate(BRHO.seed.out = (allo.df[allo.df$species == "BRHO",2] + 
                            (allo.df[allo.df$species == "BRHO",3]*inflor.g.rounded.percap))*phyto.n.indiv,
         #bkgrd.seed.out = bkgrd.n.indiv*bg.avg.seed.num, 
         BRHO.seed.in = 3,
         #bkgrd.stem.in = bkgrd.n.indiv
  ) %>%
  select(unique.ID, phyto.n.indiv, brho.seed.in, brho.seed.out)

# Create Unique ID key with treatment info also



# Merge in avg BG seed output ####
brhoP.w.BG <- left_join(brho, bg.seeds, by = c("treatment", "block", "plot", "bkgrd", "dens"))

## get BRHO ready for modeling
brho.model <- brhoP.w.BG %>%
  mutate(BRHO.seed.out = (allo.df[allo.df$species == "BRHO",2] + 
                             (allo.df[allo.df$species == "BRHO",3]*inflor.g.rounded.percap))*phyto.n.indiv,
         #bkgrd.seed.out = bkgrd.n.indiv*bg.avg.seed.num, 
         BRHO.seed.in = 3,
         #bkgrd.stem.in = bkgrd.n.indiv
         ) #%>%
  #select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.unique, phyto.n.indiv, phyto.seed.in, phyto.seed.out, bkgrd.stem.in, bkgrd.seed.out, unique.ID)


# Outstanding Qs ####
## should we model 1 phyto or the actual # of phytos?
    ## model the actual # of phytos

## what do we do when our bg indiv data do not align with the allometric relationship? How will we calculate bg seed ouput? 
    ## hopefully this doesn't happen

## How are we calculating bg seed in? We have it at the plot level, not the subplot...