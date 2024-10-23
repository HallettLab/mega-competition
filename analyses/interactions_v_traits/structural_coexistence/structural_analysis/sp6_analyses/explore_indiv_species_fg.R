## explore functional group and individual species effects

# Functional Groups ####
## Grass ####
### P/A
ggplot(sp6sum, aes(x=grass, y=mean_niche)) +
  geom_jitter(alpha = 0.15) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), linewidth = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  facet_wrap(~Rainfall) +
  xlab(NULL) +
  ylab("Niche Differences")

ggsave(paste0(fig_loc, "grass_PA_rainfall_niche.png"), width = 5, height = 4)

### Num grass species
grn = ggplot(sp6sum, aes(x=as.factor(num.grass), y=mean_niche)) +
  geom_jitter(alpha = 0.15) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), linewidth = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  facet_wrap(~Rainfall) +
  xlab(" ") +
  ylab("Niche Differences")

grf = ggplot(sp6sum, aes(x=as.factor(num.grass), y=mean_fitness)) +
  geom_jitter(alpha = 0.15) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), linewidth = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  facet_wrap(~Rainfall) +
  xlab("Grass Species Richness") +
  ylab("Fitness Differences")

ggarrange(grn, grf, ncol = 1, labels = "AUTO")

ggsave(paste0(fig_loc, "grass_richness_rainfall_nichefitness.png"), width = 7, height = 7)

## Invasive Forb ####
### Num species
frn = ggplot(sp6sum, aes(x=as.factor(num.inv.forb), y=mean_niche)) +
  geom_jitter(alpha = 0.15) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), linewidth = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  facet_wrap(~Rainfall) +
  ylab("Niche Differences") +
  xlab(" ")

frf = ggplot(sp6sum, aes(x=as.factor(num.inv.forb), y=mean_fitness)) +
  geom_jitter(alpha = 0.15) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), linewidth = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  facet_wrap(~Rainfall) +
  ylab("Fitness Differences") +
  xlab("Invasive Forb Species Richness")

ggarrange(frn, frf, ncol = 1, labels = "AUTO")

ggsave(paste0(fig_loc, "inv_forb_richness_rainfall_nichefitness.png"), width = 7, height = 7)

### Non-Legumes
nlrn = ggplot(sp6sum, aes(x=as.factor(num.non.leg.inv.forb), y=mean_niche)) +
  geom_jitter(alpha = 0.15) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), linewidth = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  facet_wrap(~Rainfall) +
  ylab("Niche Differences") +
  xlab(" ")

nlrf = ggplot(sp6sum, aes(x=as.factor(num.non.leg.inv.forb), y=mean_fitness)) +
  geom_jitter(alpha = 0.15) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), linewidth = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  facet_wrap(~Rainfall) +
  ylab("Fitness Differences") +
  xlab("Non-Legume Invasive Forb Species Richness")

ggarrange(nlrn, nlrf, ncol = 1, labels = "AUTO")

ggsave(paste0(fig_loc, "nl_inv_forb_richness_rainfall_nichefitness.png"), width = 7, height = 7)

### P/A
ggplot(sp6sum, aes(x=inv_forb, y=mean_niche, color = rainfall)) +
  geom_jitter(alpha = 0.15) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), linewidth = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  facet_wrap(~rainfall)



# Indiv Species ####
## LOMU
ggplot(sp6sum, aes(x=as.factor(LOMU), y=mean_niche, color = rainfall)) +
  geom_jitter(alpha = 0.15) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), linewidth = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  facet_wrap(~rainfall)

## ANAR
ggplot(sp6sum, aes(x=as.factor(ANAR), y=mean_niche, color = rainfall)) +
  geom_jitter(alpha = 0.15) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  facet_wrap(~rainfall)

## BRHO
ggplot(sp6sum, aes(x=as.factor(BRHO), y=mean_niche, color = rainfall)) +
  geom_jitter(alpha = 0.15) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  facet_wrap(~rainfall)

## BRNI
ggplot(sp6sum, aes(x=as.factor(BRNI), y=mean_niche, color = rainfall)) +
  geom_jitter(alpha = 0.15) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  facet_wrap(~rainfall)

## CESO
ggplot(sp6sum, aes(x=as.factor(CESO), y=mean_niche, color = rainfall)) +
  geom_jitter(alpha = 0.15) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  facet_wrap(~rainfall)

## TACA
ggplot(sp6sum, aes(x=as.factor(TACA), y=mean_niche, color = rainfall)) +
  geom_jitter(alpha = 0.15) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  facet_wrap(~rainfall)

## THIR
ggplot(sp6sum, aes(x=as.factor(THIR), y=mean_niche, color = rainfall)) +
  geom_jitter(alpha = 0.15) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  facet_wrap(~rainfall)

