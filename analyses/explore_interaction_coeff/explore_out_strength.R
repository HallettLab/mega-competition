
library(tidyverse)

theme_set(theme_classic())
## explore outstrength

ggplot(interaction_out[interaction_out$type == "net_facil",], aes(x=species, y=outstrength)) +
  geom_boxplot() +
  ylab("Facilitative Output") +
  xlab(NULL)

ggplot(interaction_out[interaction_out$type == "net_comp",], aes(x=species, y=outstrength)) +
  geom_boxplot() +
  ylab("Competitive Output") +
  xlab(NULL)

ggplot(interaction_out[interaction_out$type == "abs_value",], aes(x=species, y=outstrength)) +
  geom_boxplot() +
  ylab("Abs Value Output") +
  xlab(NULL)

ggplot(interaction_out[interaction_out$type == "net_value",], aes(x=species, y=outstrength)) +
  geom_boxplot() +
  ylab("Abs Value Output") +
  xlab(NULL)


ggplot(interaction_out, aes(x=type, y=outstrength)) +
  geom_boxplot() +
  #ylab("Abs Value Output") +
  xlab(NULL) +
  facet_wrap(~species, scales = "free") +
  geom_hline(yintercept = 0)


median_out <- interaction_out %>%
  group_by(species, treatment, type) %>%
  summarise(median_os = median(outstrength))

ggplot(median_out, aes(x=species, y=median_os, color = treatment)) +
  geom_point()+
  facet_wrap(~type, scales = "free")









