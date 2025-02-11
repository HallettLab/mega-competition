fig_loc = "analyses/interactions_v_traits/structural_coexistence/prelim_figs/CNGA/"

sp6sum_wide = sp6sum %>%
  ungroup() %>%
  select(comp, Rainfall, mean_niche, mean_fitness, num.inv, num.legume, num.grass, num.non.leg.inv.forb) %>%
  pivot_wider(names_from = "Rainfall", values_from = c("mean_niche", "mean_fitness")) %>%
  mutate(trt_diff = (mean_niche_Ambient - mean_niche_Drought)*-1,
         trt_fit_diff = (mean_fitness_Ambient - mean_fitness_Drought)*-1)
## is taking the abs of all of these the correct way to assess when the differences is above or below 0??

max(sp6sum$mean_niche)

model.dat = sp6sum_wide %>%
  filter(!is.na(trt_diff), 
         trt_diff != -Inf,
         !is.nan(trt_diff), 
         trt_diff != Inf)

model.dat.niche = sp6sum %>%
  filter(!is.na(mean_niche), 
         mean_niche != -Inf,
         !is.nan(mean_niche), 
         mean_niche != Inf)

# Overall ####
ggplot(model.dat, aes(x=trt_diff)) +
  geom_histogram() +
  geom_vline(xintercept = 0)


# Invasive Sp overall ####
ggplot(sp6sum_wide, aes(x=as.factor(num.inv), y=trt_diff)) +
  geom_jitter(alpha = 0.5) +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Number of Invasive Species") +
  ylab("Ambient - Drought Niche Differences")
## probably shouldn't use 0 & 6 here as they don't have enough communities; the trend from 1-5 is still pretty clear though

ggplot(model.dat, aes(x=as.factor(num.inv), y=trt_diff)) +
  geom_jitter(alpha = 0.5) +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Number of Invasive Species") +
  ylab("Ambient - Drought Niche Differences")

test = aov(trt_diff~as.factor(num.inv), data = model.dat)
summary(test)

# Legumes ####
ggplot(sp6sum[sp6sum$rainfall == "C",], aes(x=as.factor(num.legume), y=mean_niche)) +
  geom_jitter(alpha = 0.15) +
  ylab("Niche Differences") +
  xlab(" ") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  xlab("Number of Legume Species")

ggsave(paste0(fig_loc, "niche_diff_num_legume_amb.png"), width = 4, height = 3)

ggplot(model.dat, aes(x=as.factor(num.legume), y=trt_diff)) +
  geom_jitter(alpha = 0.15) +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Number of Legume Species") +
  ylab("Amb - Drought Niche Diffs")

ggsave(paste0(fig_loc, "trt_diff_niche_num_legume.png"), width = 4, height = 3)

mlegume = aov(trt_diff ~ as.factor(num.legume), data = model.dat)
summary(mlegume)
TukeyHSD(mlegume)

# Grasses ####
sp6sum %>%
  filter(rainfall == "C",
         !is.na(mean_niche), 
         mean_niche != -Inf,
         !is.nan(mean_niche), 
         mean_niche != Inf) %>%
ggplot(aes(x=as.factor(num.grass), y=mean_niche)) +
  geom_jitter(alpha = 0.5) +
  ylab("Niche Differences") +
  xlab(" ") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  xlab("Number of Invasive Grass Species")

ggsave(paste0(fig_loc, "niche_diff_num_grass_amb.png"), width = 4, height = 3)

ggplot(model.dat, aes(x=as.factor(num.grass), y=trt_diff)) +
  geom_jitter(alpha = 0.5) +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Number of Invasive Grass Species") +
  ylab("Amb - Drought Niche Diffs")

ggsave(paste0(fig_loc, "trt_diff_niche_num_grass.png"), width = 4, height = 3)

mgrass = aov(trt_diff ~ as.factor(num.grass), data = model.dat)
summary(mgrass)
TukeyHSD(mgrass)

ggplot(sp6sum_wide, aes(x=as.factor(num.grass), y=trt_fit_diff)) +
  geom_jitter(alpha = 0.5) +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Number of Invasive Grass Species") +
  ylab("Amb - Drought Fitness Diffs")

ggsave(paste0(fig_loc, "trt_diff_fitness_num_grass.png"), width = 4, height = 3)

## Invasive Forbs ####
sp6sum %>%
  filter(rainfall == "C",
         !is.na(mean_niche), 
         mean_niche != -Inf,
         !is.nan(mean_niche), 
         mean_niche != Inf) %>%
ggplot(aes(x=as.factor(num.non.leg.inv.forb), y=mean_niche)) +
  geom_jitter(alpha = 0.25) +
  ylab("Niche Differences") +
  xlab(" ") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  xlab("Number of Invasive Forbs")

ggsave(paste0(fig_loc, "niche_diff_num_invforb_NL_amb.png"), width = 4, height = 3)

minvforbA = aov(mean_niche ~ as.factor(num.non.leg.inv.forb), data = model.dat.niche[model.dat.niche$rainfall == "C",])

summary(minvforbA)
TukeyHSD(minvforbA)

ggplot(model.dat, aes(x=as.factor(num.non.leg.inv.forb), y=trt_diff)) +
  geom_jitter(alpha = 0.25) +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Number of Invasive Forb Species") +
  ylab("Amb - Drought Niche Diffs")

ggsave(paste0(fig_loc, "trt_diff_niche_num_invforb_NL.png"), width = 4, height = 3)

minvforb = aov(trt_diff ~ as.factor(num.non.leg.inv.forb), data = model.dat)
summary(minvforb)
TukeyHSD(minvforb)




ggplot(sp6sum_wide, aes(x=as.factor(num.non.leg.inv.forb), y=trt_fit_diff)) +
  geom_jitter(alpha = 0.5) +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Number of Invasive Forb Species") +
  ylab("Amb - Drought Fitness Diffs")

ggsave(paste0(fig_loc, "trt_diff_fitness_num_inv_forb.png"), width = 4, height = 3)

