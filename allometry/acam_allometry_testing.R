

acam_dat <- all_dat_final %>%
  filter(phyto == "ACAM")


ggplot(acam_dat, aes(x=total.biomass.rounded.percap)) +
  geom_histogram() +
  facet_wrap(~treatment)
