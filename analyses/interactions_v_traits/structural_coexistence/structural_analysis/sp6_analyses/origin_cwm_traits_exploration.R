# Set up ####
file_path = "analyses/interactions_v_traits/structural_coexistence/structural_analysis/sp6_analyses/"
fig_loc = "analyses/interactions_v_traits/structural_coexistence/prelim_figs/sept_2024/sp6/"

library(cowplot)

## read in prepped data
source(paste0(file_path, "sp6_prep_data_for_vis.R"))

rm(cwm6, fdiv6, net6all, preds, sp6_clean, sp6net, sp6trait, traitdat)

sp6sum = sp6sum %>%
  mutate(Rainfall = ifelse(rainfall == "C", "Ambient", "Drought"),
         grass = ifelse((BRHO + TACA + LOMU > 0), "grass", "forb only"),
         num.grass = BRHO + TACA + LOMU,
         inv_forb = ifelse(ANAR + BRNI + CESO + THIR > 0, "inv forb", "nat forb + inv grass"),
         num.inv.forb = ANAR + BRNI + CESO + THIR,
         num.non.leg.inv.forb = ANAR + BRNI + CESO)

# Origin x Rainfall ####
ndiff_inv = ggplot(sp6sum, aes(x=as.factor(num.inv), y=mean_niche)) +
  geom_jitter(alpha = 0.15) +
  ylab("Niche Differences") +
  xlab(" ") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  facet_wrap(~rainfall)

fdiff_inv = ggplot(sp6sum, aes(x=as.factor(num.inv), y=mean_fitness)) +
  geom_jitter(alpha = 0.15) +
  ylab("Fitness Differences") +
  xlab("Number Invasive Species") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  facet_wrap(~rainfall)

ggarrange(ndiff_inv, 
          fdiff_inv, ncol = 1, nrow = 2)

ggsave(paste0(fig_loc, "inv_rainfall_ndiff_fdiff.png"), width = 7, height = 6)


# Legumes ####
ndiff_leg = ggplot(sp6sum, aes(x=as.factor(num.legume), y=mean_niche)) +
  geom_jitter(alpha = 0.15) +
  ylab("Niche Differences") +
  xlab(" ") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  facet_wrap(~rainfall)

fdiff_leg = ggplot(sp6sum, aes(x=as.factor(num.legume), y=mean_fitness)) +
  geom_jitter(alpha = 0.15) +
  ylab("Fitness Differences") +
  xlab("Number of Legumes") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  facet_wrap(~rainfall)

ggarrange(ndiff_leg, fdiff_leg, ncol = 1, nrow = 2)

ggsave(paste0(fig_loc, "legume_rainfall_ndiff_fdiff.png"), width = 7, height = 6)

# Traits x Rainfall ####
## aboveground ####

### niche ####
nhr = ggplot(sp6sum, aes(x=cwm.height, y=mean_niche, color = rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm", size = 1.5) +
  ylab(" ") +
  labs(color = "Rainfall")  +
  xlab("CWM Height") +
  scale_color_manual(values = c("#66C5CC","#F89C74"))

nlr = ggplot(sp6sum, aes(x=cwm.ldmc, y=mean_niche, color = rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm", size = 1.5) +
  #ggtitle(" ") +
  ylab("Niche Differences") +
  xlab("CWM LDMC")+
  scale_color_manual(values = c("#66C5CC","#F89C74"))  +
  theme(legend.position = "none")

nsr = ggplot(sp6sum, aes(x=cwm.sla, y=mean_niche, color = rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm", size = 1.5) +
  #ggtitle(" ") +
  ylab(" ") +
  xlab("CWM SLA")+
  scale_color_manual(values = c("#66C5CC","#F89C74"))  +
  labs(color = "Rainfall")
  #theme(legend.position = "none")

### fitness ####
fhr = ggplot(sp6sum, aes(x=cwm.height, y=mean_fitness, color= rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  size = 1.5) +
  ylab(" ") +
  xlab("CWM Height") +
  scale_color_manual(values = c("#66C5CC","#F89C74")) +
  theme(legend.position = "none")

flr = ggplot(sp6sum, aes(x=cwm.ldmc, y=mean_fitness, color= rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  size = 1.5) +
  ylab("Fitness Differences") +
  xlab("CWM LDMC") +
  scale_color_manual(values = c("#66C5CC","#F89C74"))  +
  theme(legend.position = "none")

fsr = ggplot(sp6sum, aes(x=cwm.sla, y=mean_fitness, color= rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  size = 1.5) +
  ylab(" ") +
  xlab("CWM SLA") +
  scale_color_manual(values = c("#66C5CC","#F89C74")) +
  theme(legend.position = "none")

### cwms ####
h = ggplot(sp6sum, aes(y=cwm.height, x=as.factor(num.inv))) +
  geom_jitter(alpha = 0.15) +
  ylab("CWM Height") +
  xlab(" ") +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)

s = ggplot(sp6sum, aes(y=cwm.sla, x=as.factor(num.inv))) +
  geom_jitter(alpha = 0.15) +
  ylab("CWM SLA") +
  xlab("Num. Invasives") +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)

l = ggplot(sp6sum, aes(y=cwm.ldmc, x=as.factor(num.inv))) +
  geom_jitter(alpha = 0.15) +
  ylab("CWM LDMC") +
  xlab(" ") +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)

### put together ####
ggarrange(nsr, fsr, s, nlr, flr, l, nhr, fhr, h, ncol = 3, nrow = 3, common.legend = TRUE, legend = "bottom", labels = "AUTO")

ggsave(paste0(fig_loc, "cwm_AG_nichefit_numinv.png"), width = 9, height = 10)

## belowground ####
#### niche ####
nrr = ggplot(sp6sum, aes(x=cwm.rmf, y=mean_niche, color = rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  linewidth = 1.5) +
  #ggtitle(" ") +
  ylab(" ") +
  xlab("CWM RMF")+
  scale_color_manual(values = c("#66C5CC","#F89C74")) +
  labs(color = "Rainfall")

ncr = ggplot(sp6sum, aes(x=cwm.crsl, y=mean_niche, color = rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  linewidth = 1.5) +
  ylab("Niche Differences") +
  xlab("CWM CRSL")+
  scale_color_manual(values = c("#66C5CC","#F89C74")) +
  labs(color = "Rainfall")

npr = ggplot(sp6sum, aes(x=cwm.pf, y=mean_niche, color = rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  linewidth = 1.5) +
  #ggtitle(" ") +
  ylab("Niche Differences") +
  xlab("CWM PF")+
  scale_color_manual(values = c("#66C5CC","#F89C74"))

ndr = ggplot(sp6sum, aes(x=cwm.d, y=mean_niche, color = rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  linewidth = 1.5) +
  #ggtitle(" ") +
  ylab(" ") +
  xlab("CWM Diameter") +
  scale_color_manual(values = c("#66C5CC","#F89C74"))

#### fitness ####
frr = ggplot(sp6sum, aes(x=cwm.rmf, y=mean_fitness, color = rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  size = 1.5) +
 # ggtitle(" ") +
  ylab(" ") +
  xlab("CWM RMF") +
  scale_color_manual(values = c("#66C5CC","#F89C74")) +
  labs(color = "Rainfall")

fcr = ggplot(sp6sum, aes(x=cwm.crsl, y=mean_fitness, color = rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  size = 1.5) +
  #ggtitle("6 Species") +
  ylab("Fitness Differences") +
  xlab("CWM CRSL") +
  scale_color_manual(values = c("#66C5CC","#F89C74")) +
  labs(color = "Rainfall")

fpr = ggplot(sp6sum, aes(x=cwm.pf, y=mean_fitness, color = rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  size = 1.5) +
  #ggtitle(" ") +
  ylab("Fitness Differences") +
  xlab("CWM PF") +
  scale_color_manual(values = c("#66C5CC","#F89C74"))

fdr = ggplot(sp6sum, aes(x=cwm.d, y=mean_fitness, color = rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  size = 1.5) +
  #ggtitle(" ") +
  ylab(" ") +
  xlab("CWM Diameter") +
  scale_color_manual(values = c("#66C5CC","#F89C74"))

#### cwms ####
c = ggplot(sp6sum, aes(y=cwm.crsl, x=as.factor(num.inv))) +
  geom_jitter(alpha = 0.15) +
  ylab("CWM CRSL") +
  xlab(" ") +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)

r = ggplot(sp6sum, aes(y=cwm.rmf, x=as.factor(num.inv))) +
  geom_jitter(alpha = 0.15) +
  ylab("CWM RMF") +
  xlab(" ") +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)

p = ggplot(sp6sum, aes(y=cwm.pf, x=as.factor(num.inv))) +
  geom_jitter(alpha = 0.15) +
  ylab("CWM PF") +
  xlab(" ") +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)

d = ggplot(sp6sum, aes(y=cwm.d, x=as.factor(num.inv))) +
  geom_jitter(alpha = 0.15) +
  ylab("CWM Diameter") +
  xlab("Num Invasive Sp") +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)

#### put together ####
ggarrange(ncr, fcr, c, nrr, frr, r, npr, fpr, p, ndr, fdr, d, common.legend = T, legend = "bottom", ncol = 3, nrow = 4, labels = "AUTO")

ggsave(paste0(fig_loc, "cwm_BG_nichefit_numinv.png"), width = 9, height = 12)
