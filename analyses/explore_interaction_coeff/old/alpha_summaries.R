
library(tidyverse)
theme_set(theme_classic())

params <- read.csv("data/parameter_summaries_20231218_models.csv")

ggplot(params[!params$parameter_type %in% c("alpha_weed", "lambda"),], aes(x= species, y=median_parameter, color = treatment)) +
  geom_point() +
  facet_wrap(~parameter_type, scales = "free") +
  geom_errorbar(aes(ymin = hdi_lo, ymax = hdi_hi)) +
  theme(axis.text.x=element_text(angle=90, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#70a494", "#de8a5a"))

ggsave("analyses/explore_interaction_coeff/preliminary_figures/median_alphas.png", width = 12, height = 8)

facil_median <- params %>%
  filter(median_parameter < 0)

ggplot(facil_median[!facil_median$parameter_type %in% c("alpha_weed", "lambda"),], aes(x= species, y=median_parameter, color = treatment)) +
  geom_point() +
  facet_wrap(~parameter_type, scales = "free") +
  geom_errorbar(aes(ymin = hdi_lo, ymax = hdi_hi)) +
  theme(axis.text.x=element_text(angle=90, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#70a494", "#de8a5a")) +
  ggtitle("Facilitation Only")

ggsave("analyses/explore_interaction_coeff/preliminary_figures/median_alphas_facil.png", width = 12, height = 8)
