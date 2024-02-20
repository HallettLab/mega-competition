library(tidyverse)
## ACAM seed output
theme_set(theme_classic())

source("data_cleaning/phyto-processing_data-cleaning/ACAM_phyto.R")

acam.phyto <- acam.phyto %>%
  mutate(seeds.out.percap = phyto.seed.out/phyto.n.indiv)

ggplot(acam.phyto, aes(x=seeds.out.percap)) +
  geom_histogram(bins = 100) +
  xlab("ACAM seeds produced, per capita") +
  coord_cartesian(xlim = c(0,100)) +
  geom_vline(xintercept = median(acam.phyto$seeds.out.percap), linetype = "dashed") +
  geom_vline(xintercept = mean(acam.phyto$seeds.out.percap), linetype = "dashed", color = "red")

ggsave("ACAM_percap_seeds.png", width = 4, height = 3)

median(acam.phyto$seeds.out.percap)
mean(acam.phyto$seeds.out.percap)

## BRHO seed output
theme_set(theme_classic())

source("data_cleaning/phyto-processing_data-cleaning/BRHO_phyto.R")

brho.phyto <- brho.phyto %>%
  mutate(seeds.out.percap = phyto.seed.out/phyto.n.indiv)

ggplot(brho.phyto, aes(x=seeds.out.percap)) +
  geom_histogram(bins = 100) +
  xlab("BRHO seeds produced, per capita") +
  #coord_cartesian(xlim = c(0,100)) +
  geom_vline(xintercept = median(brho.phyto$seeds.out.percap), linetype = "dashed") +
  geom_vline(xintercept = mean(brho.phyto$seeds.out.percap), linetype = "dashed", color = "red")

ggsave("ACAM_percap_seeds.png", width = 4, height = 3)

median(brho.phyto$seeds.out.percap)
mean(brho.phyto$seeds.out.percap)
