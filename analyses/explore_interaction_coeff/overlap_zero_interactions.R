# GOAL 
## explore the interactions that overlap 0

## interested in the means - when are they below, above, or pretty much 0?

## is the majority of the posterior distrib above, below, or centered on 0?

## filter out interactions that overlap 0
overlapzero <- alpha_sums_89hdi.filt %>%
  filter(interaction_type == "overlaps zero")

## create a vector of combos to filter by
oz <- unique(overlapzero$combo)

## filter out these interaction combos from full posterior distribs
overlapzero2 <- interxn %>%
  filter(combo %in% oz)

overlapzero3 <- left_join(overlapzero2, overlapzero, by = c("species", "alpha_name", "combo"))


acambrho <- overlapzero3 %>%
  filter(species == "BRHO", 
         alpha_name == "alpha_acam_c")

ggplot(acambrho, aes(x=alpha_value)) +
  geom_density() +
  geom_vline(xintercept = acambrho$ci_hi, linetype = "dashed") +
  geom_vline(xintercept = acambrho$ci_lo, linetype = "dashed")


ggplot(overlapzero3[overlapzero3$alpha_name == "alpha_acam_c" & overlapzero3$species == "BRHO",], aes(x=alpha_value)) +
  geom_histogram() +
  geom_vline(xintercept = overlapzero3$ci_hi) +
  facet_wrap(~species)



# Visualize 
ggplot(overlapzero2[overlapzero2$alpha_name == "alpha_acam_c",], aes(x=alpha_value)) +
  geom_density() +
  facet_wrap(~species)



