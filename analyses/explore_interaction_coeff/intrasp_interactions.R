
## need to find a way to calc mean, mode, or whatever the most appropriate statistic is for this type of distribution...

## read in data
source("analyses/explore_interaction_coeff/classify_interactions.R")

## filter to just intraspecific interactions
intra <- alpha_sums_89hdi.filt %>%
  filter(species == toupper(substr(alpha_name, 7, 10)))

## plot summary of intraspecific interactions
ggplot(intra, aes(x = interaction_type)) +
  geom_bar() +
  xlab("Interaction Type") +
  ggtitle("INTRAspecific interaction types")

ggsave("analyses/explore_interaction_coeff/preliminary_figures/intra_interaction_type_summary.png", width = 3, height = 2)

intra_posteriors <- interxn %>%
  filter(species == toupper(substr(alpha_name, 7, 10)))

test <- left_join(intra_posteriors, intra, by = c("species", "alpha_name", "combo")) %>%
  group_by(species, alpha_name, combo) %>%
  filter(alpha_value > ci_lo & alpha_value < ci_hi) %>%
  mutate(treatment = ifelse(substr(alpha_name, 12, 13) == "d", "D", "C"),
         alpha_name = substr(alpha_name, 1, 10))

test2 <- test %>%
  group_by(species, alpha_name, treatment) %>%
  summarise(median_alpha = median(alpha_value))


ggplot(test) +
  geom_density(aes(x=alpha_value, fill = treatment, linetype = interaction_type), linewidth = 1) +
  facet_wrap(~species, scales = "free", ncol= 5, nrow = 3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("#70a494", "#de8a5a")) +
  scale_linetype_manual(values = c("solid", "dotted", "dashed")) +
  xlab("Alpha Value") + ylab("Density") +
  labs(fill = "Precip Treatment", linetype = "Interaction Type") +
  ggtitle("Intraspecific Interaction Coefficients, 0.89 CI") +
  geom_point(data = test2, aes(x=median_alpha, y=0, fill = treatment), pch = 21, color = "black", size = 4, stroke = 1)

ggsave("analyses/explore_interaction_coeff/preliminary_figures/intra_interaction_coeffs.png", width = 12, height = 6)

ggplot(intra_posteriors, aes(x=alpha_value)) +
  geom_histogram() +
  facet_wrap(~species, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed")
