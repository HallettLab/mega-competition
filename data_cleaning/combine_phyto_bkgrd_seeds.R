## This script combines phyto and background seeds in/out and gets data in a model ready format! 

# Read in Data ####
## phyto seeds in/out
source("data_cleaning/phyto-processing_data-cleaning/compile_phyto-processing_data.R")

## bkgrd seeds in/out
source("data_cleaning/bkgrd-processing_data-cleaning/bkgrd_calculations.R")


# Join Data ####
## join phyto & bkgrd data
bg.phyto.seeds <- left_join(all.phytos, bkgrd.seeds, by = "unique.ID") %>%
  select(-phyto.n.indiv)

## join with unique.ID key to get block, plot, etc info
    ## also, adjust trifolium names to take out the annoying "-I"
bg.phyto.seeds2 <- left_join(bg.phyto.seeds, unique.key, by = c("unique.ID", "phyto", "bkgrd")) %>%
  mutate(phyto = ifelse(phyto == "THIR-I", "THIR", phyto), 
         bkgrd = ifelse(bkgrd == "THIR-I", "THIR", bkgrd), 
         phyto = ifelse(phyto == "TWIL-I", "TWIL", phyto), 
         bkgrd = ifelse(bkgrd == "TWIL-I", "TWIL", bkgrd))

## reorder columns
bg.phyto.seeds2 <- bg.phyto.seeds2[,c(1, 8:11,13,2,7,12, 3:6)] 

# Format for Models ####
model.dat <- bg.phyto.seeds2 %>%
  mutate(phyto.seeds.in.final = ifelse(bkgrd == phyto, bg.seeds.in, phyto.seed.in)) %>%
  mutate(bkgrd.names.in = paste0(bkgrd, ".seeds.in")) %>% 
  pivot_wider(names_from = "bkgrd.names.in", values_from = "bg.seeds.in", values_fill = 0) %>% ## seeds in for each background as a separate col
  mutate(phyto.seeds.out.final = ifelse(bkgrd == phyto, phyto.seed.out + bg.seeds.out, phyto.seed.out)) %>% ## for intra phytos, add phyto & bg seeds out values
  select(-Control.seeds.in, -NA.seeds.in, -phyto.seed.in, -phyto.seed.out, -bg.seeds.out)

model.dat <- model.dat[,c(1:10,29,11:28)]

colnames(model.dat)[12:ncol(model.dat)] <- substr(colnames(model.dat)[12:ncol(model.dat)], 1, 4)

species <- colnames(model.dat)[12:ncol(model.dat)] 

for(i in species){
  model.dat[model.dat$phyto == i, i] <- model.dat[model.dat$phyto == i,]$phyto.seeds.in.final
}

model.dat <- model.dat [,-10]


## clean env
rm(list = c("all.phytos", "allo.df", "bg.phyto.seeds", "bg.phyto.seeds2", "bkgrd.seeds", "block.plots", "calcSE", "collectionsC", "drought", "i", "lead", "med_scales", "phyto.census", "plot.dates", "seeds.per.flower", "tmp.germ", "tmp.plot", "unique.key"))
