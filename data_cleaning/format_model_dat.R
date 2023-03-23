## This script combines phyto and background seeds in/out and gets data in a model ready format! 

# Read in Data ####
## phyto seeds in/out ####
source("data_cleaning/phyto-processing_data-cleaning/compile_phyto-processing_data.R")

## bkgrd seeds in/out ####
source("data_cleaning/bkgrd-processing_data-cleaning/bkgrd_calculations.R")


# Double checks ####
all.phytos[all.phytos$unique.ID == 5462,]
## this phyto has a seed out val currently. It just loses it somewhere along the way


# Replicate Control Rows ####
all.phytos.info <- left_join(all.phytos, unique.key, by = c("unique.ID", "phyto")) %>%
  mutate(bkgrd = ifelse(bkgrd == "ERBO", "Control", bkgrd))
## set ERBO backgrounds as controls


blocks <- unique(all.phytos.info$block) 
species <- unique(all.phytos.info$phyto)
bkgrds <- unique(all.phytos.info$bkgrd)


## make column of backgrounds to join w/replicated control lines later
bkgrd.df <- as.data.frame(bkgrds[bkgrds != "Control"])
colnames(bkgrd.df) <- "bkgrd"

## store outputs
repeated.controls <- data.frame()



## loop through each species
for (i in 1:length(species)) {
  
  tmp.sp <- species[i] ## select phyto species
  
  all.blocks <- data.frame() ## make dataframe to store all blocks for one species
  
  ## loop through each block
  for (j in 1:length(blocks)) {
    
    tmp.block <- blocks[j] ## select the block
    
    tmp.controls <- all.phytos.info %>% ## filter to controls of particular species
      filter(phyto == tmp.sp,
             bkgrd == "Control",
             block == tmp.block)
    
    ## if there are controls in a particular block, replicate each row 18x and add in a unique background for each
    if (nrow(tmp.controls) > 0 ) {
      
      control.reps <- unique(tmp.controls$unique.ID) ## make vector of each rep in the block
      
      ## create df to store output of all reps within a block
      all.reps <- data.frame()
      
      ## loop through each control sample in one block
      for (k in 1:length(control.reps)) {
        
        tmp.rep <- control.reps[k] ## select rep
        
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

with.controls <- rbind(all.phytos.info, repeated.controls) %>%
  filter(bkgrd != "Control")

unique.id.controls <- sort(unique(repeated.controls$unique.ID))
## make a vector of control unique.IDs

# Join Data ####
## join phyto & bkgrd data ####
bg.phyto.seeds <- left_join(with.controls, bkgrd.seeds, by = c("unique.ID", "bkgrd")) %>%
  select(-phyto.n.indiv) %>%
  mutate(bg.seeds.in = ifelse(is.na(bg.seeds.in) & unique.ID %in% unique.id.controls, 0, bg.seeds.in),
         bg.seeds.out = ifelse(is.na(bg.seeds.out) & unique.ID %in% unique.id.controls, 0, bg.seeds.out))

bg.phyto.seeds[bg.phyto.seeds$unique.ID == 5462,]
## CESO sample still ok here

## reorder columns
bg.phyto.seeds <- bg.phyto.seeds[,c(1,5:8,11,9:10,2:4,12:13)] 

check <- bg.phyto.seeds %>%
  filter(is.na(bg.seeds.in))
## No NAs
## PLER 7958 in ANAR background

check2 <- bg.phyto.seeds %>%
  filter(is.na(phyto.seed.in))
## several MAEL phytos with no seeds in vals. that's probably why the model wasn't working for this species...

check3 <- bg.phyto.seeds %>%
  filter(is.na(phyto.seed.out))

# Format for Models ####
## pivot wider ####
model.dat.init <- bg.phyto.seeds %>%
  mutate(phyto.seeds.in.final = ifelse(bkgrd == phyto & dens != "none", bg.seeds.in, phyto.seed.in)) %>% ## if bg & phyto are the same, use bg.seeds in; intraspecific control phytos are a specific case - for these we need to use the phyto seeds in
  
  mutate(bkgrd.names.in = bkgrd) %>% ## make an extra bkgrd column bc will need 2
  pivot_wider(names_from = "bkgrd.names.in", values_from = "bg.seeds.in", values_fill = 0) %>% ## seeds in for each background sp as a separate col
  mutate(phyto.seeds.out.final = ifelse(bkgrd == phyto, phyto.seed.out + bg.seeds.out, phyto.seed.out)) %>% ## for intra phytos, add phyto & bg seeds out values
  select(-phyto.seed.in, -phyto.seed.out, -bg.seeds.out) ## drop extraneous cols

model.dat.init <- model.dat.init[,c(1:10,29,11:28)] ## reorder

## fill in phyto.seeds.in ####
## create a vector of species names
species <- colnames(model.dat.init)[12:ncol(model.dat.init)] 

## for each species, when it is the phyto, fill in the appropriate seeds.in col with the values from phyto.seeds.in.final
for(i in species){
  model.dat.init[model.dat.init$phyto == i, i] <- model.dat.init[model.dat.init$phyto == i,]$phyto.seeds.in.final
}

model.dat.init <- model.dat.init [,-10] ## get rid of phyto.seeds.in.final column

## filter by rep num ####
ok.reps <- model.dat.init %>%
  filter(dens != "none") %>%
  group_by(treatment, phyto, bkgrd) %>%
  summarise(reps = n()) %>%
  mutate(combos = paste(phyto, bkgrd, sep = "_")) %>%
  filter(reps > 2)

### add in weed census ####
model.dat.filtered <- left_join(model.dat.init, phyto.census[,c(1,5:10)], by = "unique.ID") %>%
  mutate(combos = paste(phyto, bkgrd, sep = "_")) %>%
  filter(combos %in% ok.reps$combos)

# Make Lambda Priors df ####
lambda_priors <- all.phytos.info %>%
  filter(bkgrd == "Control") %>%
  group_by(phyto) %>%
  summarise(max_seeds_ctrl = max(phyto.seed.out), 
            sd_seeds = sd(phyto.seed.out))
## there is only one CLPU control sample. So there's no SD and therefore the model won't run correctly since this is an automatic input.

## Fix CLPU ####
clpu <- bg.phyto.seeds %>%
  filter(phyto == "CLPU", bg.seeds.in == 0, bkgrd == "AVBA") %>%
  summarise(max_seeds_ctrl = max(phyto.seed.out), 
            sd_seeds = sd(phyto.seed.out))
## will substitue AVBA bgs with no indiv in the bg as controls for this purpose

lambda_priors[lambda_priors$phyto == "CLPU",]$max_seeds_ctrl <- clpu$max_seeds_ctrl
lambda_priors[lambda_priors$phyto == "CLPU",]$sd_seeds <- clpu$sd_seeds

# Clean env ####
rm(all.phytos, allo.df, bg.phyto.seeds, bkgrd.seeds, block.plots, calcSE, collectionsC, i, lead, phyto.census, plot.dates, tmp.germ, tmp.plot, unique.key, with.controls, tmp.repeated.reps, tmp.controls, repeated.controls, ok.reps, bkgrd.df, all.blocks, all.phytos.info, bkgrds, blocks, control.reps, j, k, tmp.block, tmp.rep, tmp.sp, all.reps)
