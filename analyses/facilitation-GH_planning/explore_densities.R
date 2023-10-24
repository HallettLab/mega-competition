
## get final model data
model.dat <- read.csv("data/model_dat.csv")

source("data_cleaning/germination_data-cleaning/germination_data_cleaning.R")

library(tidyverse)
theme_set(theme_classic())


## Densities
acam <- model.dat %>%
  filter(bkgrd == "ACAM") %>%
  mutate(stems.per.cm2 = ACAM/Nbrhood.size,
         GH.stems = stems.per.cm2*10.795)

ggplot(acam, aes(x=ACAM)) +
  geom_histogram() +
  facet_wrap(~Nbrhood.size)

ggsave("analyses/facilitation-GH_planning/bg_densities.png", width = 6, height = 3)

ggplot(acam, aes(x=stems.per.cm2)) +
  geom_histogram() #+
  #facet_wrap(~Nbrhood.size)

ggsave("analyses/facilitation-GH_planning/stems_per_area.png", width = 4, height = 3)

## pots are 10.795 cm2 

ggplot(acam, aes(x=GH.stems)) +
  geom_histogram() +
  ggtitle("Field stems/cm2 * pot size (10.795cm2)")
ggsave("analyses/facilitation-GH_planning/stems_per_area_pot_size.png", width = 4, height = 3)

