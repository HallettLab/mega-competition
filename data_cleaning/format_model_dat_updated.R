## Purpose: This script combines phyto and background seeds in/out and gets data in a model ready format! 

# Read in Data ####
## phyto seeds in/out
source("data_cleaning/phyto-processing_data-cleaning/compile_phyto-processing_data.R")

## bkgrd stems
source("data_cleaning/phyto-collections_data-cleaning/phyto-collections_data-cleaning.R")
## an update from 9/1/2023 when determined should use stems data, NOT back calculated seeds in

## germination
#source("data_cleaning/germination_data-cleaning/germination_data_cleaning.R")


## Filter OUT controls; don't think I want to model with these data
all.phytos <- all.phytos %>%
  filter(bkgrd != "Control", bkgrd != "ERBO")

# Join Data ####
## join phyto & bkgrd data
phyto.bg.dat <- left_join(all.phytos, phyto.census, by = c("unique.ID")) %>%
  select(-phyto.n.indiv.y) %>%
  mutate(phyto.n.indiv = phyto.n.indiv.x) %>%
  select(-phyto.n.indiv.x)

## check to see if the merge caused any errors (likely due to mismatch b/w phyto.n.indiv)
na.bg <- phyto.bg.dat %>%
  filter(is.na(bkgrd.n.indiv))
## all good!

# Replicate Info ####
## create df with number of reps for each precip-background-phyto species combo
reps <- phyto.bg.dat %>%
  filter(dens != "none") %>%
  mutate(bg.indiv.absent = ifelse(bkgrd.n.indiv == 0, 1, 0)) %>%
  group_by(treatment, phyto, bkgrd) %>%
  summarise(bad.reps = sum(bg.indiv.absent), 
            tot.reps = n()) %>%
  ungroup() %>%
  mutate(combos = paste(phyto, bkgrd, treatment, sep = "_"), 
         true.reps = tot.reps-bad.reps) 
## lose 128/645 pairwise by treatment combinations

## filter good reps
good.reps <- reps %>%
  filter(true.reps > 3)

## make vector 
good.reps.vec <- unique(good.reps$combos)

# Format for Models ####
model.dat.init <- phyto.bg.dat %>%
  mutate(bkgrd.names.in = bkgrd) %>%
  pivot_wider(names_from = "bkgrd.names.in", values_from = "bkgrd.n.indiv", values_fill = 0) %>%
  #select(-bkgrd.n.indiv) %>% 
  mutate(combos = paste(phyto, bkgrd, treatment, sep = "_")) %>%
  filter(combos %in% good.reps.vec) %>% ## filter by rep #
  mutate(weeds = CRCO + ERBO + FIGA + GAMU + HYGL + SIGA + other, 
         trt = ifelse(treatment == "D", 0, 1)) %>% ## lump all the weeds together
  select(-CRCO, -ERBO, -FIGA, -GAMU, -HYGL, -SIGA, -other, -phyto.seed.in)
## have decided to model intraspecific phytos as seeds out from the phytometer,
## NOT the seeds out from the phyto + estimated bg seeds out

## will model focal seeds in with STEMS not by multiplying seeds in x germination

model.dat <- model.dat.init

# Join Germ Data ####
#model.dat <- left_join(model.dat.init, mean.germ, by = c("phyto", "treatment"))

# Clean Env ####
rm(all.phytos, allo.df, calcSE, collectionsC, i, lead, phyto.census, unique.key, with.controls, tmp.repeated.reps, tmp.controls, repeated.controls, bkgrd.df, all.blocks,  bkgrds, blocks, control.reps, j, k, tmp.block, tmp.rep, tmp.sp, all.reps, model.dat.init, bkgrd.n.indiv, mean.germ, na.bg, phyto.bg.dat, reps, good.reps)
