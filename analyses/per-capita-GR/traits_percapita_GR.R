library(tidyverse)
library(stats)


# Read in Data ####
## per-capita growth rates
source("models/CW/per-capita-GR/percapita_GR.R")

## traits
source("data_cleaning/trait_data-cleaning/adult_traits/adult_traits_cleaning.R")



percap.with.traits <- left_join(percap.growth, traits, by = "phyto") %>%
  mutate(phyto.nativity = nativity,
         phyto.growth.form = growth_form,
         phyto.height.cm = Height_cm,
         phyto.sla.cm2.g = SLA_cm2.g,
         phyto.lwc = LWC, 
         phyto.cn = CN,
         bkgrd = bkgrd.x) %>%
  select(unique.ID:sub, dens:dens.num, phyto.nativity:bkgrd)

percap.with.bg.traits <- left_join(percap.with.traits, traits, by = "bkgrd") %>%
  mutate(bkgrd.nativity = nativity,
         bkgrd.growth.form = growth_form, 
         bkgrd.height.cm = Height_cm,
         bkgrd.sla.cm.g = SLA_cm2.g,
         bkgrd.lwc = LWC,
         bkgrd.cn = CN,
         phyto = phyto.x) %>%
  select(unique.ID, phyto.n.indiv:bkgrd,bkgrd.nativity:phyto) %>%
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

#ggsave("models/per-capita-GR/log.per.cap.functional.groups.png", width = 8, height = 4)






