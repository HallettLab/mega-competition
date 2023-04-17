# mean abundances ####
equil_abund <- as.data.frame(rbind(apply(residents_dry, 2, mean),
                                   apply(residents_wet, 2, mean)))
equil_abund$trt <- c("dry","wet")

equil_abund <- equil_abund %>%
  pivot_longer(cols = PLER:MICA, names_to = "species", values_to = "abundance")


#rm(list=setdiff(ls(), c("invasion_dry", "invasion_wet","equil_abund","params")))

# plot ####

dry_means <- invasion_dry %>% 
  summarise_all(list(mean))

wet_means <- invasion_wet %>% 
  summarise_all(list(mean))

colnames(wet_means) <- str_sub(names(wet_means), start = 1, end = 14)
colnames(dry_means) <- str_sub(names(dry_means), start = 1, end = 14)

invasion_means <- rbind(dry_means, wet_means)
invasion_means$trt <- c("dry","wet")

invasion_means <- invasion_means %>%
  pivot_longer(cols = PLER_into_BRHO:MICA_into_LOMU, 
               names_to = "invasion", values_to = "growth")

invasion_means$invader <- str_sub(invasion_means$invasion, start = 1, end = 4)
invasion_means$resident <- str_sub(invasion_means$invasion, start = 11, end = 14)

ggplot(invasion_means, aes(x = resident, y = growth, col = trt, group = trt)) + 
  geom_point(size = 3) + 
  facet_wrap(~invader, ncol = 3, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed")

#trait <- read.csv("/users/Marina/Documents/Dropbox/Mega_Competition/Data/Traits/Megacomp_adult-traits.csv")

invasion_means <- merge(invasion_means, trait[,c(2,3,6)], by.x = "invader", by.y = "code_4")

names(invasion_means)[6] <- "invader.nativity"
names(invasion_means)[7] <- "invader.growth_form"

invasion_means <- merge(invasion_means, trait[,c(2,3,6)], by.x = "resident", by.y = "code_4")

names(invasion_means)[8] <- "resident.nativity"
names(invasion_means)[9] <- "resident.growth_form"

invasion_means$invader.fungroup <- paste(invasion_means$invader.nativity, invasion_means$invader.growth_form, sep = " ")

invasion_means$resident.fungroup <- paste(invasion_means$resident.nativity, invasion_means$resident.growth_form, sep = " ")

ggplot(invasion_means, aes(x = resident.fungroup, y = growth, fill = trt)) +
  geom_boxplot() +
  facet_wrap(~invader.fungroup, ncol = 3, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed")

invasion_means_summary <- invasion_means %>%
  group_by(trt, invader.fungroup, resident.fungroup) %>%
  summarize(ldgr.mean = mean(growth),
            ldgr.se = calcSE(growth))

ggplot(invasion_means_summary, aes(x = resident.fungroup, y = ldgr.mean, col = trt, group = trt)) +
  geom_point() +
  geom_errorbar(aes(ymin = ldgr.mean - ldgr.se, ymax = ldgr.mean + ldgr.se, width = 0.2)) +
  facet_wrap(~invader.fungroup, ncol = 3, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed")
