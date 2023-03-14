lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Seed-Survival/"


seed.surv <- read.csv(paste0(lead, "seed-bag-survival_20230219.csv"))


# Seed Survival #####
seed.surv2 <- seed.surv %>%
  filter(n.viable > 0) %>%
  group_by(Species) %>% 
  mutate(reps = n()) %>%
  filter(reps > 2) %>%
  mutate(spcode = toupper(strsplit(Species, " ") %>%
                              sapply(tail, 1))) %>%
  mutate(genus = toupper(substr(Species, 1, 2)),
         sp = substr(spcode, 1, 2), 
         species = paste0(genus, sp)) %>%
  select(-spcode, -genus, -sp, -X) %>%
  mutate(species = ifelse(species == "UNHI", "THIR", species))


seed.surv.grwr <- merge(invasion_means, seed.surv2, by.x = "invader", by.y = "species") %>%
  filter(growth != "NaN")

ggplot(seed.surv.grwr, aes(x=n.viable, y=growth, color = invader)) +
  geom_point()

ggplot(invasion_means_summary, aes(y=ldgr.mean, x=invader.fungroup, color = trt)) + geom_point() +
  geom_errorbar(aes(ymin = ldgr.mean - ldgr.se, ymax = ldgr.mean + ldgr.se))




temp <- invasion_means %>%
  group_by(invader.fungroup,trt) %>%
  summarise(mean.ldgr = mean(growth, na.rm = T), se.ldgr = calcSE(growth))

ggplot(temp, aes(x=invader.fungroup, y=mean.ldgr, color = trt)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = mean.ldgr - se.ldgr, ymax = mean.ldgr + se.ldgr), width = 0.25)
ggsave("mean_ldgr_invader_fungroup.png", width = 4, height = 3)


# PCA ####
adults.pca

invasion_means

ldgr.pca <- merge(invasion_means, adults.pca[,c(21,46:49)], by.x="invader", by.y="code_4")

ggplot(ldgr.pca, aes(x=PC2, y=growth, color = trt)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~trt)
ggsave("pc2.ldgr.png", width = 5, height = 3)


ggplot(ldgr.pca, aes(x=PC2, y=growth, color = FunGroup)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~trt)
ggsave("pc2.ldgr.FG.png", width = 5, height = 3)
