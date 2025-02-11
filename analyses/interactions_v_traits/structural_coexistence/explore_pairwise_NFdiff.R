# Explore pairwise N/F diff

library(tidyverse)

theme_set(theme_classic())

## read in data
npdat = read.csv("data/ndiff_fdiff_pairwise_test_20241115.csv")


pairwise = npdat %>%
  filter(pairwise_Fdiff > 0) %>%
  mutate(coexistence = ifelse( (1+pairwise_Ndiff) < pairwise_Fdiff & (pairwise_Fdiff < 1), "Yes", "No"))

pairwise %>%
  filter(TACA == 1) %>%
ggplot(aes(y=pairwise_Fdiff, x = pairwise_Ndiff, color = coexistence)) +
  geom_point(size = 3) +
  facet_wrap(~rainfall) +
  geom_ribbon(aes(ymin = , ymax = ))


## rho < fitness diff < 1/rho





