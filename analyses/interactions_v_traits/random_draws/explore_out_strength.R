
library(tidyverse)

theme_set(theme_classic())
## explore outstrength

fig_loc <- "analyses/explore_interaction_coeff/preliminary_figures/storyboard_v2/"

interaction_out <- read.csv("data/outstrength_20240424.csv") %>%
  mutate(combo = paste(species, treatment, sep = "_"))

ggplot(interaction_out[interaction_out$type == "net_facil",], aes(x=species, y=abs(outstrength_raw))) +
  geom_boxplot() +
  ylab("Net Facilitative Output, Raw") +
  xlab(NULL)

ggsave(paste0(fig_loc, "facil_output_raw_boxplot.png"), width = 7, height = 3)


ggplot(interaction_out[interaction_out$type == "net_comp",], aes(x=species, y=abs(outstrength_raw))) +
  geom_boxplot() +
  ylab("Net Competitive Output, Raw") +
  xlab(NULL)

ggsave(paste0(fig_loc, "comp_output_raw_boxplot.png"), width = 7, height = 3)





















ggplot(interaction_out[interaction_out$type == "net_facil",], aes(x=species, y=log(abs(outstrength_scaled)))) +
  geom_boxplot() +
  ylab("Log(Facilitative Output, Scaled)") +
  xlab(NULL)

ggsave(paste0(fig_loc, "facil_output_logged_scaled_boxplot.png"), width = 7, height = 3)

ggplot(interaction_out[interaction_out$type == "net_facil",], aes(x=outstrength_scaled)) +
  geom_density() +
  ylab("Facilitative Output") +
  xlab(NULL) +
  facet_wrap(~species, scales = "free")

ggplot(interaction_out[interaction_out$type == "net_comp",], aes(x=species, y=log(outstrength))) +
  geom_boxplot() +
  ylab("Log(Competitive Output, Scaled)") +
  xlab(NULL)

ggplot(interaction_out[interaction_out$type == "abs_value",], aes(x=species, y=outstrength)) +
  geom_boxplot() +
  ylab("Abs Value Output") +
  xlab(NULL)

ggplot(interaction_out[interaction_out$type == "net_value",], aes(x=species, y=outstrength)) +
  geom_boxplot() +
  ylab("Abs Value Output") +
  xlab(NULL)


ggplot(interaction_out[interaction_out$type %in% c("net_facil", "net_comp"),], aes(x=type, y=outstrength)) +
  geom_boxplot() +
  #ylab("Abs Value Output") +
  xlab(NULL) +
  facet_wrap(~species, scales = "free") +
  geom_hline(yintercept = 0)

ggsave(paste0(fig_loc, "net_facil_comp.png"), width = 10, height = 8)


median_out <- interaction_out %>%
  group_by(species, treatment, type) %>%
  summarise(median_os = median(outstrength))

ggplot(median_out, aes(x=species, y=median_os, color = treatment)) +
  geom_point()+
  facet_wrap(~type, scales = "free")


dissim.params <- left_join(interaction_out, dissim.long, by = c("combo"))






