library(tidyverse)


# Read in Data ####
## per-capita growth rates
source("models/per-capita-GR/percapita_GR.R")

## traits data
## specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Traits/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Traits/"
  
} else {
  # Marina
  lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Traits/"
} 

traits <- read.csv(paste0(lead, "Megacomp_adult-traits.csv")) %>%
  mutate(phyto = code_4, 
         bkgrd = code_4) 

percap.with.traits <- left_join(percap.growth, traits, by = "phyto") %>%
  mutate(phyto.nativity = nativity,
         phyto.growth.form = growth_form) %>%
  select(1:14, 40:41)

percap.with.bg.traits <- left_join(percap.with.traits, traits, by = "bkgrd") %>%
  mutate(bkgrd.nativity = nativity,
         bkgrd.growth.form = growth_form, 
         phyto = phyto.x) %>%
  select(1:16, 43:44) %>%
  mutate(bkgrd.group = paste(bkgrd.nativity, bkgrd.growth.form, sep = "_"),
         phyto.group = paste(phyto.nativity, phyto.growth.form, sep = "_"), 
         log.percap = log(percap.growthrate))

ggplot(percap.with.bg.traits, aes(x=dens.num, y=percap.growthrate, color = phyto.group)) +
  geom_boxplot() +
  facet_wrap(~bkgrd.group)

ggplot(percap.with.bg.traits, aes(x=dens, y=percap.growthrate, fill = phyto.group, group = phyto.group)) +
  geom_boxplot() +
  facet_wrap(~bkgrd.group)

ggplot(percap.with.bg.traits, aes(x=dens, y=log.percap, by = phyto.group, fill = phyto.group)) +
  geom_boxplot() +
  facet_wrap(~bkgrd.group) +
  geom_hline(yintercept = 0, linetype = "dashed")

ggsave("models/per-capita-GR/log.per.cap.functional.groups.png", width = 8, height = 4)









