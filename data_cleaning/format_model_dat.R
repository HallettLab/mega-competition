## This script combines phyto and background seeds in/out and gets data in a model ready format! 

# Read in Data ####
## phyto seeds in/out
source("data_cleaning/phyto-processing_data-cleaning/compile_phyto-processing_data.R")

## bkgrd seeds in/out
source("data_cleaning/bkgrd-processing_data-cleaning/bkgrd_calculations.R")


# change THIR and TWIL names in germ data  #
germ.sum.sp.DC <- germ.sum.sp.DC %>% 
  mutate(species = ifelse(species == "THIR-I", "THIR", species), 
         species = ifelse(species == "TWIL-I", "TWIL", species))

# change THIR and TWIL names in unique ID key
unique.key <- unique.key %>% 
  mutate(phyto = ifelse(phyto == "THIR-I", "THIR", phyto), 
         bkgrd = ifelse(bkgrd == "THIR-I", "THIR", bkgrd), 
         phyto = ifelse(phyto == "TWIL-I", "TWIL", phyto), 
         bkgrd = ifelse(bkgrd == "TWIL-I", "TWIL", bkgrd))

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
## join phyto & bkgrd data
bg.phyto.seeds <- left_join(with.controls, bkgrd.seeds, by = c("unique.ID", "bkgrd")) %>%
  select(-phyto.n.indiv) %>%
  mutate(bg.seeds.in = ifelse(is.na(bg.seeds.in) & unique.ID %in% unique.id.controls, 0, bg.seeds.in),
         bg.seeds.out = ifelse(is.na(bg.seeds.out) & unique.ID %in% unique.id.controls, 0, bg.seeds.out))



## join with unique.ID key to get block, plot, etc info
    ## also, adjust trifolium names to take out the annoying "-I"
#bg.phyto.seeds2 <- left_join(bg.phyto.seeds, unique.key, by = c("unique.ID", "phyto", "bkgrd")) %>%
 # mutate(phyto = ifelse(phyto == "THIR-I", "THIR", phyto), 
  #       bkgrd = ifelse(bkgrd == "THIR-I", "THIR", bkgrd), 
   #      phyto = ifelse(phyto == "TWIL-I", "TWIL", phyto), 
    #     bkgrd = ifelse(bkgrd == "TWIL-I", "TWIL", bkgrd))

## reorder columns
bg.phyto.seeds <- bg.phyto.seeds[,c(1,5:8,11,9:10,2:4,12:13)] 

# Format for Models ####
model.dat.init <- bg.phyto.seeds %>%
  mutate(phyto.seeds.in.final = ifelse(bkgrd == phyto & dens != "none", bg.seeds.in, phyto.seed.in)) %>% ## if bg & phyto are the same, use bg.seeds in; intraspecific control phytos are a specific case - for these we need to use the phyto seeds in
  
  mutate(bkgrd.names.in = bkgrd) %>% ## make an extra bkgrd column bc will need 2
  pivot_wider(names_from = "bkgrd.names.in", values_from = "bg.seeds.in", values_fill = 0) %>% ## seeds in for each background sp as a separate col
  mutate(phyto.seeds.out.final = ifelse(bkgrd == phyto, phyto.seed.out + bg.seeds.out, phyto.seed.out)) %>% ## for intra phytos, add phyto & bg seeds out values
  select(-phyto.seed.in, -phyto.seed.out, -bg.seeds.out) ## drop extraneous cols

model.dat.init <- model.dat.init[,c(1:10,29,11:28)] ## reorder

## extraneous now- this change bg colnames back to sp names, but changed that above
#colnames(model.dat)[12:ncol(model.dat)] <- substr(colnames(model.dat)[12:ncol(model.dat)], 1, 4)

## create a vector of species names
species <- colnames(model.dat.init)[12:ncol(model.dat.init)] 

## for each species, when it is the phyto, fill in the appropriate seeds.in col with the values from phyto.seeds.in.final
for(i in species){
  model.dat.init[model.dat.init$phyto == i, i] <- model.dat.init[model.dat.init$phyto == i,]$phyto.seeds.in.final
}

model.dat.init <- model.dat.init [,-10] ## get rid of phyto.seeds.in.final column


ok.reps <- model.dat.init %>%
  filter(dens != "none") %>%
  group_by(treatment, phyto, bkgrd) %>%
  summarise(reps = n()) %>%
  mutate(combos = paste(phyto, bkgrd, sep = "_")) %>%
  filter(reps > 2)

model.dat.filtered <- model.dat.init %>%
  mutate(combos = paste(phyto, bkgrd, sep = "_")) %>%
  filter(combos %in% ok.reps$combos)


## clean env
rm(all.phytos, allo.df, bg.phyto.seeds, bkgrd.seeds, block.plots, calcSE, collectionsC, i, lead, phyto.census, plot.dates, tmp.germ, tmp.plot, unique.key, with.controls, tmp.repeated.reps, tmp.controls, repeated.controls, ok.reps, bkgrd.df, all.blocks, all.phytos.info, bkgrds, blocks, control.reps, j, k, tmp.block, tmp.rep, tmp.sp, all.reps)
