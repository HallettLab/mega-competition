library(tidyverse)


# Read in Data ####
## clean env
rm(list=ls()) 

## specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Set-up/Plot-backgrounds/Plot-backgrounds_entered/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Set-up/Plot-backgrounds/Plot-backgrounds_entered/"
  
} else {
  # Marina
  lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Set-up/Plot-backgrounds/Plot-backgrounds_entered/"
} 

brho_bg_seed_input <- read.csv(paste0(lead, "brho_seed_weighing_checklist.csv"))
gitr_bg_seed_input <- read.csv(paste0(lead, "gitr_seed_weighing_checklist.csv"))

# BRHO ####
brho_bg_seed_in <- bg_seed_input %>%
  mutate(bkgrd = "BRHO", 
         block = Bag.., 
         dens = ifelse(Seeding.Density == "High (8g)", "H", "L"),
         seeds.in.g = Final.Weight, 
         notes = Notes) %>%
  select(bkgrd, block, dens, seeds.in.g, notes) %>%
  filter(!is.na(seeds.in.g))

# GITR ####
gitr_bg_seed_in <- bg_seed_input %>%
  mutate(bkgrd = "GITR", 
         block = Bag.., 
         dens = ifelse(Seeding.Density == "High (8g)", "H", "L"),
         seeds.in.g = Final.Weight, 
         notes = Notes) %>%
  select(bkgrd, block, dens, seeds.in.g, notes) %>%
  filter(!is.na(seeds.in.g))




