# Set up ####
file_path = "analyses/interactions_v_traits/structural_coexistence/structural_analysis/sp6_analyses/"
source(paste0(file_path, "sp6_prep_data_for_vis.R"))

fig_loc = "analyses/interactions_v_traits/structural_coexistence/prelim_figs/sept_2024/sp6/"

# Interactions ####
## CWMS/Rainfall ####
### AG traits ####
#### Niche diff ####
nhr = ggplot(sp6sum, aes(x=cwm.height, y=mean_niche, color = rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm", size = 1.5) +
  ggtitle("6 Species") +
  ylab("Niche Differences") +
  xlab("CWM Height") +
  scale_color_manual(values = c("#66C5CC","#F89C74"))

nlr = ggplot(sp6sum, aes(x=cwm.ldmc, y=mean_niche, color = rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm", size = 1.5) +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM LDMC")+
  scale_color_manual(values = c("#66C5CC","#F89C74"))

nsr = ggplot(sp6sum, aes(x=cwm.sla, y=mean_niche, color = rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm", size = 1.5) +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM SLA")+
  scale_color_manual(values = c("#66C5CC","#F89C74"))

#### Fitness diff ####
fhr = ggplot(sp6sum, aes(x=cwm.height, y=mean_fitness, color= rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  size = 1.5) +
  ggtitle("6 Species") +
  ylab("Fitness Differences") +
  xlab("CWM Height") +
  scale_color_manual(values = c("#66C5CC","#F89C74")) +
  labs(color = "Rainfall")

flr = ggplot(sp6sum, aes(x=cwm.ldmc, y=mean_fitness, color= rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  size = 1.5) +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM LDMC") +
  scale_color_manual(values = c("#66C5CC","#F89C74"))

ggplot(sp6sum, aes(x=cwm.ldmc, y=mean_fitness, color= rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  size = 1.5) +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM LDMC") +
  scale_color_manual(values = c("#66C5CC","#F89C74")) +
  facet_wrap(~rainfall)

fsr = ggplot(sp6sum, aes(x=cwm.sla, y=mean_fitness, color= rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  size = 1.5) +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM SLA") +
  scale_color_manual(values = c("#66C5CC","#F89C74"))

#### Plot together ####

ggarrange(fhr, flr, fsr, ncol = 3, nrow = 1, common.legend = T, legend = "bottom")
ggsave(paste0(fig_loc, "cwm_ag_rainfall_fdiff.png"), width = 10, height = 3.5)

ggarrange(nhr, nlr, nsr, 
          fhr, flr, fsr,
          ncol = 3, nrow = 2, common.legend = T, labels = "AUTO")

ggsave(paste0(fig_loc, "cwm_ag_rainfall_ndiff_fdiff.png"), width = 10, height = 3.5)


### BG Traits ####
nrr = ggplot(sp6sum, aes(x=cwm.rmf, y=mean_niche, color = rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  size = 1.5) +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM RMF")+
  scale_color_manual(values = c("#66C5CC","#F89C74"))

ncr = ggplot(sp6sum, aes(x=cwm.crsl, y=mean_niche, color = rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  size = 1.5) +
  ggtitle("6 Species") +
  ylab("Niche Differences") +
  xlab("CWM CRSL")+
  scale_color_manual(values = c("#66C5CC","#F89C74"))

npr = ggplot(sp6sum, aes(x=cwm.pf, y=mean_niche, color = rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  size = 1.5) +
  ggtitle(" ") +
  ylab("Niche Differences") +
  xlab("CWM PF")+
  scale_color_manual(values = c("#66C5CC","#F89C74"))

ndr = ggplot(sp6sum, aes(x=cwm.d, y=mean_niche, color = rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  size = 1.5) +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM Diameter") +
  scale_color_manual(values = c("#66C5CC","#F89C74"))

ggarrange(ncr, nrr, npr, ndr, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")

ggsave(paste0(fig_loc, "cwm_bg_rainfall_ndiff.png"), width = 7, height = 6)



#### BG traits ####
frr = ggplot(sp6sum, aes(x=cwm.rmf, y=mean_fitness, color = rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  size = 1.5) +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM RMF") +
  scale_color_manual(values = c("#66C5CC","#F89C74")) +
  labs(color = "Rainfall")

ggplot(sp6sum, aes(x=cwm.rmf, y=mean_fitness, color = rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  size = 1.5) +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM RMF") +
  scale_color_manual(values = c("#66C5CC","#F89C74")) +
  labs(color = "Rainfall") +
  facet_wrap(~rainfall)

fcr = ggplot(sp6sum, aes(x=cwm.crsl, y=mean_fitness, color = rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  size = 1.5) +
  ggtitle("6 Species") +
  ylab("Fitness Differences") +
  xlab("CWM CRSL") +
  scale_color_manual(values = c("#66C5CC","#F89C74")) +
  labs(color = "Rainfall")

fpr = ggplot(sp6sum, aes(x=cwm.pf, y=mean_fitness, color = rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  size = 1.5) +
  ggtitle(" ") +
  ylab("Fitness Differences") +
  xlab("CWM PF") +
  scale_color_manual(values = c("#66C5CC","#F89C74"))

fdr = ggplot(sp6sum, aes(x=cwm.d, y=mean_fitness, color = rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  size = 1.5) +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM Diameter") +
  scale_color_manual(values = c("#66C5CC","#F89C74"))

ggarrange(fcr, frr, fpr, fdr, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")

ggsave(paste0(fig_loc, "cwm_bg_rainfall_fdiff.png"), width = 7, height = 6)

## FDIV/Rainfall ####
fdivndr = ggplot(sp6sum, aes(x=fdiv, y=mean_niche, color = rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  size = 1.5) +
  ylab("Niche Differences") +
  xlab("Functional Diversity") +
  ggtitle("6 Species") +
  scale_color_manual(values = c("#66C5CC","#F89C74")) +
  labs(color = "Rainfall")

fdivfdr = ggplot(sp6sum, aes(x=fdiv, y=mean_fitness, color = rainfall)) +
  geom_point(alpha = 0.25, size = 0.75) +
  geom_smooth(method = "lm",  size = 1.5) +
  ylab("Fitness Differences") +
  xlab("Functional Diversity") +
  ggtitle(" ") +
  scale_color_manual(values = c("#66C5CC","#F89C74"))


ggarrange(fdivndr, fdivfdr, ncol = 2, nrow = 1, labels = "AUTO", common.legend = T, legend = "bottom")

ggsave(paste0(fig_loc, "fdiv_rainfall_overall_patterns.png"), width = 7, height = 4)

## Indirect/Rainfall ####
ggplot(sp6sum, aes(x=rainfall, y=mean_cpo)) +
  geom_jitter(alpha = 0.15) +
  #ylab("Niche Differences") +
  xlab("Rainfall Treatment") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

ggplot(sp6sum, aes(x=rainfall, y=mean_cpo)) +
  #geom_jitter(alpha = 0.15) +
  #ylab("Niche Differences") +
  xlab("Rainfall Treatment") +
  theme(text = element_text(size = 15)) +
  #geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

ggplot(sp6sum, aes(x=rainfall, y=mean_cpd)) +
  geom_jitter(alpha = 0.15) +
  #ylab("Niche Differences") +
  xlab("Rainfall Treatment") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

ggplot(sp6sum, aes(x=rainfall, y=mean_cpd)) +
  #geom_jitter(alpha = 0.15) +
  #ylab("Niche Differences") +
  xlab("Rainfall Treatment") +
  theme(text = element_text(size = 15)) +
  #geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

ggplot(sp6sum, aes(x=mean_cpo, y=mean_niche, color = rainfall)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Niche Differences") +
  xlab("Community Pair Overlap") 

ggplot(sp6sum, aes(x=mean_cpd, y=mean_niche, color = rainfall)) +
  geom_point() +
  geom_smooth() +
  ylab("Niche Differences") +
  xlab("Community Pair Differential") +
  ggtitle(" ")

ggplot(sp6sum, aes(x=mean_cpo, y=mean_fitness, color = rainfall)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Fitness Differences") +
  xlab(" ") 

ggplot(sp6sum, aes(x=mean_cpd, y=mean_fitness, color = rainfall)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Fitness Differences") +
  xlab(" ") +
  ggtitle(" ") +
  facet_wrap(~rainfall)

## Indirect/Origin
ggplot(sp6sum, aes(x=as.factor(num.inv), y=mean_cpd)) +
  geom_jitter(alpha = 0.15) +
  #ylab("Niche Differences") +
  xlab("Number of Invasive Sp") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

ggplot(sp6sum, aes(x=as.factor(num.inv), y=mean_cpo)) +
  geom_jitter(alpha = 0.15) +
  #ylab("Niche Differences") +
  xlab("Number of Invasive Sp") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")
