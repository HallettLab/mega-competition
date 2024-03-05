## Explore Equilibrium abundances further


## species that returned a value in wet conditions
# c("ANAR", "BRHO", "GITR", "LENI", "LOMU", "MICA", "PLER", "TACA")

## species that returned a value in wet conditions
# c("BRHO", "CESO", "GITR", "LOMU", "MICA", "TACA", "TWIL")


wet_wsp <- residents_wet_long %>%
  filter(species %in% c("ANAR", "BRHO", "GITR", "LENI", "LOMU", "MICA", "PLER", "TACA"))

ggplot(wet_wsp, aes(x=equil_abund)) +
  geom_histogram() +
  facet_wrap(~species, scale = "free")

ggsave(paste0("analyses/classic_MCT/preliminary_equil_abundance/", date, "/explore_working_sp/equil_abund_hist_ricker_negbinom_C_workingsp_", date, ".png"), height = 6, width = 10)

ggplot(wet_wsp[wet_wsp$equil_abund < 100000,], aes(x=equil_abund)) +
  geom_histogram() +
  facet_wrap(~species, scale = "free")

ggplot(wet_wsp[wet_wsp$equil_abund < 1,], aes(x=equil_abund)) +
  geom_histogram() +
  facet_wrap(~species, scale = "free")
ggsave(paste0("analyses/classic_MCT/preliminary_equil_abundance/", date, "/explore_working_sp/equil_abund_hist_ricker_negbinom_C_workingsp_filt_lessthan1", date, ".png"), height = 6, width = 10)


dry_wsp <- residents_dry_long %>%
  filter(species %in% c("BRHO", "GITR", "LOMU", "MICA", "TACA"))


ggplot(dry_wsp, aes(x=num, y=equil_abund)) +
  geom_point() +
  facet_wrap(~species, scale = "free") +
  ggtitle("Dry; working species; no filtering")

ggsave(paste0("analyses/classic_MCT/preliminary_equil_abundance/", date, "/explore_working_sp/equil_abund_ricker_negbinom_D_workingsp_", date, ".png"), height = 6, width = 10)


ggplot(dry_wsp, aes(x=equil_abund)) +
  geom_histogram() +
  facet_wrap(~species, scale = "free") +
  ggtitle("Dry; working species; no filtering")

ggsave(paste0("analyses/classic_MCT/preliminary_equil_abundance/", date, "/explore_working_sp/equil_abund_hist_ricker_negbinom_D_workingsp_", date, ".png"), height = 6, width = 10)

ggplot(dry_wsp[dry_wsp$equil_abund < 10000,], aes(x=equil_abund)) +
  geom_histogram() +
  facet_wrap(~species, scale = "free") +
  ggtitle("Dry; working species; eq abund < 10000")

ggsave(paste0("analyses/classic_MCT/preliminary_equil_abundance/", date, "/explore_working_sp/equil_abund_hist_ricker_negbinom_D_workingsp_filt_lessthan10000", date, ".png"), height = 6, width = 10)

ggplot(dry_wsp[dry_wsp$equil_abund < 1,], aes(x=equil_abund)) +
  geom_histogram() +
  facet_wrap(~species, scale = "free") +
  ggtitle("Dry; working species; eq abund < 1")

ggsave(paste0("analyses/classic_MCT/preliminary_equil_abundance/", date, "/explore_working_sp/equil_abund_hist_ricker_negbinom_D_workingsp_filt_lessthan1", date, ".png"), height = 6, width = 10)



