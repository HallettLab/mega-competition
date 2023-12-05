## Purpose: This script combines phyto and background seeds in/out and gets data in a model ready format! 

# Read in Data ####
## phyto seeds in/out
source("data_cleaning/phyto-processing_data-cleaning/compile_phyto-processing_data.R")

## bkgrd stems
source("data_cleaning/phyto-collections_data-cleaning/phyto-collections_data-cleaning.R")
## an update from 9/1/2023 when determined should use stems data, NOT back calculated seeds in

## germination
#source("data_cleaning/germination_data-cleaning/germination_data_cleaning.R")

all.phytos <- all.phytos %>%
  mutate(bkgrd = ifelse(bkgrd == "ERBO", "Control", bkgrd))
## set ERBO backgrounds as controls, as several were used this way

# Replicate Control Rows ####
## replicate control rows in the df so that each phytometer has an associated set of control values during model runs; each replicated set of control rows will be saved with a phyto name

## create vectors
blocks <- unique(all.phytos$block) 
species <- unique(all.phytos$phyto)
bkgrds <- unique(all.phytos$bkgrd)

## make column of backgrounds to join w/replicated control lines later
bkgrd.df <- as.data.frame(bkgrds[bkgrds != "Control"])
colnames(bkgrd.df) <- "bkgrd"

## make df to store outputs
repeated.controls <- data.frame()

## loop through each species
for (i in 1:length(species)) {
  
  tmp.sp <- species[i] ## select phyto species
  
  all.blocks <- data.frame() ## make dataframe to store all blocks for one species
  
  ## loop through each block
  for (j in 1:length(blocks)) {
    
    tmp.block <- blocks[j] ## select the block
    
    tmp.controls <- all.phytos %>% ## filter to controls of particular species
      filter(phyto == tmp.sp,
             bkgrd == "Control",
             block == tmp.block)
    
    ## if there are controls in a particular block, 
    ## replicate each row 18x and add in a unique background for each
    if (nrow(tmp.controls) > 0 ) {
      
      ## make vector of each rep in the block
      control.reps <- unique(tmp.controls$unique.ID) 
      
      ## create df to store output of all reps within a block
      all.reps <- data.frame()
      
      ## loop through each control sample in one block
      for (k in 1:length(control.reps)) {
        
        ## select rep
        tmp.rep <- control.reps[k] 
        
        tmp.repeated.reps <- tmp.controls %>%
          filter(unique.ID == tmp.rep) %>% ## filter to correct rep
          slice(rep(1:n(), each = 18)) %>% ## repeat each row 18 times
          select(-bkgrd) %>% ## get rid of old bg column
          mutate(bkgrd = bkgrd.df[,1], ## add in 18 unique backgrounds
                 dens = "none") ## fill out density column as 'none'
        
        ## store all reps together
        all.reps <- rbind(all.reps, tmp.repeated.reps)
      }
      
    ## if there isn't a control in a particular block, just make a blank df to combine
    } else {
      
      all.reps <- data.frame()
      
    }
    
    ## add all reps in a block into one block wide df
    all.blocks <- rbind(all.blocks, all.reps)
    
  }
  
  ## add all blocks of a species into the repeated controls df
  repeated.controls <- rbind(repeated.controls, all.blocks)
}

## join back with main phyto dataframe
with.controls <- rbind(all.phytos, repeated.controls) %>%
  filter(bkgrd != "Control") 
    ## remove 'control' backgrounds as the controls are now saved as phytos; 
    ## they are still identifiable as controls by their unique.ID

unique.id.controls <- sort(unique(repeated.controls$unique.ID))
## make a vector of control unique.IDs

# Join Data ####
## join phyto & bkgrd data
phyto.bg.dat <- left_join(with.controls, phyto.census, by = c("unique.ID")) %>%
  select(-phyto.n.indiv.y) %>%
  mutate(phyto.n.indiv = phyto.n.indiv.x) %>%
  ## if an observation is a control background, input 0 seeds
  mutate(bkgrd.n.indiv = ifelse(dens == "none", 0, bkgrd.n.indiv)) %>%
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
## lose 122/640 pairwise by treatment combinations

## filter good reps
good.reps <- reps %>%
  filter(true.reps > 3)

## make vector 
good.reps.vec <- unique(good.reps$combos)

# Format for Models ####
model.dat <- phyto.bg.dat %>%
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

write.csv(model.dat, "data/model_dat.csv")

# Clean Env ####
rm(all.phytos, allo.df, calcSE, collectionsC, i, lead, phyto.census, unique.key, with.controls, tmp.repeated.reps, tmp.controls, repeated.controls, bkgrd.df, all.blocks,  bkgrds, blocks, control.reps, j, k, tmp.block, tmp.rep, tmp.sp, all.reps, model.dat.init, bkgrd.n.indiv, mean.germ, na.bg, phyto.bg.dat, reps, good.reps)
