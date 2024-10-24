## 6 species analyses

## explore how rainfall, origin, functional diversity, indirect interactions, and network metrics influence coexistence proportion, niche differences, and fitness differences

# Set up ####
file_path = "analyses/interactions_v_traits/structural_coexistence/structural_analysis/sp6_analyses/"
fig_loc = "analyses/interactions_v_traits/structural_coexistence/prelim_figs/sept_2024/sp6/"

## read in prepped data
source(paste0(file_path, "sp6_prep_data_for_vis.R"))

# Visualize ####
## Niche vs Fitness ####
ggplot(sp6sum, aes(x=mean_niche, y=mean_fitness)) +
  geom_point() +
  facet_wrap(~rainfall) +
  geom_smooth(method = "lm") +
  xlab("Niche Differences") +
  ylab("Fitness Differences")

ggsave(paste0(fig_loc, "niche_v_fitness.png"), width = 6, height = 3)

## RAINFALL ####
pf = ggplot(sp6sum, aes(x=rainfall, y=prop_feasible)) +
  geom_jitter(alpha = 0.15) +
  ylab("Proportion of Coexistence") +
  xlab("") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle("6 Species")

nd = ggplot(sp6sum, aes(x=rainfall, y=mean_niche)) +
  geom_jitter(alpha = 0.15) +
  ylab("Niche Differences") +
  xlab("Rainfall Treatment") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

fd = ggplot(sp6sum, aes(x=rainfall, y=mean_fitness)) +
  geom_jitter(alpha = 0.15) +
  ylab("Fitness Differences") +
  xlab("") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

ggarrange(pf, nd, fd, ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom", labels = "AUTO")

ggsave(paste0(fig_loc, "rain_violin_plots.png"), width = 9, height = 3.5)

## ORIGIN & FG ####
lpfo = ggplot(sp6sum, aes(x=as.factor(num.inv), y=log(prop_feasible))) +
  geom_jitter(alpha = 0.15) +
  ylab("Log(Prop. Coexistence)") +
  xlab("Number of Invasive Species") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)

pfo = ggplot(sp6sum, aes(x=as.factor(num.inv), y=prop_feasible)) +
  geom_jitter(alpha = 0.15) +
  ylab("Prop. Coexistence") +
  xlab("Number of Invasive Species") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)

### Num Inv ####
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


### Num Legumes ####
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

## FDIV ####
fdivpf = ggplot(sp6sum, aes(x=fdiv, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Proportion of Coexistence") +
  xlab(" ") +
  ggtitle("6 Species")

fdivnd = ggplot(sp6sum, aes(x=fdiv, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Niche Differences") +
  xlab("Functional Diversity") +
  ggtitle(" ")

fdivfd = ggplot(sp6sum, aes(x=fdiv, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Fitness Differences") +
  xlab("") +
  ggtitle(" ")

ggarrange(fdivpf, fdivnd, fdivfd, ncol = 3, nrow = 1, labels = "AUTO")

ggsave(paste0(fig_loc, "fdiv_overall_patterns.png"), width = 7, height = 3)

## CWMS ####
### Niche diff ####
nh = ggplot(sp6sum, aes(x=cwm.height, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("6 Species") +
  ylab("Niche Differences") +
  xlab("CWM Height")

nl = ggplot(sp6sum, aes(x=cwm.ldmc, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM LDMC")

ns = ggplot(sp6sum, aes(x=cwm.sla, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM SLA")

nr = ggplot(sp6sum, aes(x=cwm.rmf, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM RMF")

nc = ggplot(sp6sum, aes(x=cwm.crsl, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab("Niche Differences") +
  xlab("CWM CRSL")

np = ggplot(sp6sum, aes(x=cwm.pf, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM PF")

nd = ggplot(sp6sum, aes(x=cwm.d, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM Diameter")

ggarrange(nh, nl, ns, nr, nc, np, nd, nrow = 2, ncol = 4)

ggsave(paste0(fig_loc, "cwm_ndiff.png"), width = 10, height = 4.5)

### Fitness diff ####
fh = ggplot(sp6sum, aes(x=cwm.height, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("6 Species") +
  ylab("Fitness Differences") +
  xlab("CWM Height")

fl = ggplot(sp6sum, aes(x=cwm.ldmc, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM LDMC")

fs = ggplot(sp6sum, aes(x=cwm.sla, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM SLA")

fr = ggplot(sp6sum, aes(x=cwm.rmf, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM RMF")

fc = ggplot(sp6sum, aes(x=cwm.crsl, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab("Fitness Differences") +
  xlab("CWM CRSL")

fp = ggplot(sp6sum, aes(x=cwm.pf, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM PF")

fd = ggplot(sp6sum, aes(x=cwm.d, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM Diameter")

ggarrange(fh, fl, fs, fr, fc, fp, fd, nrow = 2, ncol = 4)

ggsave(paste0(fig_loc, "cwm_fdiff.png"), width = 10, height = 4.5)

### Prop Coexist ####
pfh = ggplot(sp6sum, aes(x=cwm.height, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("6 Species") +
  ylab("Prop Feasible") +
  xlab("CWM Height")

pfl = ggplot(sp6sum, aes(x=cwm.ldmc, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM LDMC")

pfs = ggplot(sp6sum, aes(x=cwm.sla, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM SLA")

pfr = ggplot(sp6sum, aes(x=cwm.rmf, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM RMF")

pfc = ggplot(sp6sum, aes(x=cwm.crsl, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab("Prop Feasible") +
  xlab("CWM CRSL")

pfp = ggplot(sp6sum, aes(x=cwm.pf, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM PF")

pfd = ggplot(sp6sum, aes(x=cwm.d, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM Diameter")

ggarrange(pfh, pfl, pfs, pfr, pfc, pfp, pfd, nrow = 2, ncol = 4)

ggsave(paste0(fig_loc, "cwm_propfeas.png"), width = 10, height = 4.5)

## INDIRECT INT ####
cpopf = ggplot(sp6sum, aes(x=mean_cpo, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Proportion of Coexistence") +
  xlab(" ") 

cpdpf = ggplot(sp6sum, aes(x=mean_cpd, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Proportion of Coexistence") +
  xlab(" ") +
  ggtitle("6 Species")

cpond = ggplot(sp6sum, aes(x=mean_cpo, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Niche Differences") +
  xlab("Community Pair Overlap") 

cpdnd = ggplot(sp6sum, aes(x=mean_cpd, y=mean_niche)) +
  geom_point() +
  geom_smooth() +
  ylab("Niche Differences") +
  xlab("Community Pair Differential") +
  ggtitle(" ")

cpofd = ggplot(sp6sum, aes(x=mean_cpo, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Fitness Differences") +
  xlab(" ") 

cpdfd = ggplot(sp6sum, aes(x=mean_cpd, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Fitness Differences") +
  xlab(" ") +
  ggtitle(" ")

ggarrange(cpdpf, cpdnd, cpdfd,
          cpopf, cpond, cpofd,
          nrow = 2, ncol = 3, labels = "AUTO")

ggsave(paste0(fig_loc, "indirect_interactions.png"), width = 8, height = 6)

## Network Metrics ####
### dominance ####
ndom = ggplot(sp6sum, aes(x=mean_dom, y=mean_niche)) +
  geom_point() +
  facet_wrap(~rainfall, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle("6 Species") +
  xlab("Mean Dominance") +
  ylab("Mean Niche Differences")

fdom = ggplot(sp6sum, aes(x=mean_dom, y=mean_fitness)) +
  geom_point() +
  facet_wrap(~rainfall, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  xlab("Mean Dominance") +
  ylab("Mean Fitness Differences")

domhist = ggplot(sp6sum, aes(x=mean_dom)) +
  geom_histogram() +
  xlab("Mean Dominance") +
  ylab("Count")

ggarrange(ndom, fdom, domhist, ncol = 1, nrow = 3, labels = "AUTO")

ggsave(paste0(fig_loc, "netmet_dom_ndiff_fdiff_hist.png"), width = 5, height = 7.5)

### asymmetry ####
nasym = ggplot(sp6sum, aes(x=mean_asym, y=mean_niche)) +
  geom_point() +
  facet_wrap(~rainfall, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle("6 Species") +
  xlab("Mean Asymmetry") +
  ylab("Mean Niche Differences")

fasym = ggplot(sp6sum, aes(x=mean_asym, y=mean_fitness)) +
  geom_point() +
  facet_wrap(~rainfall, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle("6 Species") +
  xlab("Mean Asymmetry") +
  ylab("Mean Fitness Differences")

asymhist = ggplot(sp6sum, aes(x=mean_asym)) +
  geom_histogram()  +
  xlab("Mean Asymmetry") +
  ylab("Count")

ggarrange(nasym, fasym, asymhist, ncol = 1, nrow = 3, labels = "AUTO")

ggsave(paste0(fig_loc, "netmet_asym_ndiff_fdiff_hist.png"), width = 5, height = 7.5)

### skewness ####
nskew = ggplot(sp6sum, aes(x=mean_skew, y=mean_niche)) +
  geom_point() +
  facet_wrap(~rainfall, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle("6 Species") +
  xlab("Mean Skewness") +
  ylab("Mean Niche Differences")

fskew = ggplot(sp6sum, aes(x=mean_skew, y=mean_fitness)) +
  geom_point() +
  facet_wrap(~rainfall, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle("6 Species") +
  xlab("Mean Skewness") +
  ylab("Mean Fitness Differences")

skewhist = ggplot(sp6sum, aes(x=mean_skew)) +
  geom_histogram()  +
  xlab("Mean Skewness") +
  ylab("Count")

ggarrange(nskew, fskew, skewhist, ncol = 1, nrow = 3, labels = "AUTO")

ggsave(paste0(fig_loc, "netmet_skew_ndiff_fdiff_hist.png"), width = 5, height = 7.5)

### modularity ####
nmod = ggplot(sp6sum, aes(x=mean_mod, y=mean_niche)) +
  geom_point() +
  facet_wrap(~rainfall, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle("6 Species") +
  xlab("Mean Modularity") +
  ylab("Mean Niche Differences")

ggplot(sp6sum, aes(x=mean_mod, y=mean_niche, color = as.factor(num.legume))) +
  geom_point() +
  facet_wrap(~rainfall, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle("6 Species") +
  xlab("Mean Modularity") +
  ylab("Mean Niche Differences")

ggplot(sp6sum, aes(x=mean_mod, fill = as.factor(num.legume), color = as.factor(num.legume))) +
  geom_histogram(alpha = 0.25) +
  facet_grid(as.factor(num.legume)~rainfall) +
  #geom_smooth(method = "lm") +
  ggtitle("6 Species") +
  xlab("Mean Modularity") +
  ylab("Count") +
  labs(fill = "Num Legumes", color = "Num Legumes")
ggsave(paste0(fig_loc, "mod_hist_num_legumes.png"), width = 6, height = 6)

fmod = ggplot(sp6sum, aes(x=mean_mod, y=mean_fitness)) +
  geom_point() +
  facet_wrap(~rainfall, scales = "free") +
  geom_smooth(method = "lm") +
  ggtitle("6 Species") +
  xlab("Mean Modularity") +
  ylab("Mean Fitness Differences")

modhist = ggplot(sp6sum, aes(x=mean_mod)) +
  geom_histogram()  +
  xlab("Mean Modularity") +
  ylab("Count")

ggarrange(nmod, fmod, modhist, ncol = 1, nrow = 3, labels = "AUTO")

ggsave(paste0(fig_loc, "netmet_mod_ndiff_fdiff_hist.png"), width = 5, height = 7.5)

# Summary ####
sp6fsum = sp6sum %>%
  group_by(prop_feasible) %>%
  summarise(numcomm = n())

sum(sp6fsum[sp6fsum$prop_feasible != 0,]$numcomm)