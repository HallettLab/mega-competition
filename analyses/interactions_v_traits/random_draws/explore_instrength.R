## Explore In-strength for each species

# Set up ####
library(tidyverse)

theme_set(theme_classic())

fig_loc <- "analyses/interactions_v_traits/random_draws/preliminary_figures/"

in_strength <- read.csv("data/instrength_filtered_raw_&_scaled_20240521.csv")

# Visualize ####
ggplot(in_strength[in_strength$type == "net_facil" & in_strength$species != "MAEL",], aes(x=species, y=abs(instrength_raw))) +
  geom_boxplot() +
  ylab("Net Facilitative Input, Raw") +
  xlab(NULL)

ggsave(paste0(fig_loc, "facil_input_raw_boxplot.png"), width = 7, height = 3)

ggplot(in_strength[in_strength$type == "net_comp" & in_strength$species != "MAEL",], aes(x=species, y=abs(instrength_raw))) +
  geom_boxplot() +
  ylab("Net Competitive Input, Raw") +
  xlab(NULL)

ggsave(paste0(fig_loc, "comp_input_raw_boxplot.png"), width = 7, height = 3)


ggplot(in_strength[in_strength$type == "net_value" & in_strength$species != "MAEL",], aes(x=species, y=instrength_raw)) +
  geom_boxplot() +
  ylab("Net Input, Raw") +
  xlab(NULL)

ggsave(paste0(fig_loc, "net_input_raw_boxplot.png"), width = 7, height = 3)

