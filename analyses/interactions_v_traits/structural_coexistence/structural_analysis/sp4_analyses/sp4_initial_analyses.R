## 4 species analyses
## explore how rainfall, origin, functional diversity, indirect interactions, cwm traits, and network metrics influence coexistence, niche differences, and fitness differences

# Set up ####
## load packages
library(ggpubr)
library(tidyverse)

## set file paths
file_path = "analyses/interactions_v_traits/structural_coexistence/"

fig_loc = "analyses/interactions_v_traits/structural_coexistence/prelim_figs/sept_2024/sp4/"

## read in data 
### coexistence metrics/indirect interactions
sp4 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp4/4_sp_structural_results_20240828.csv"))

### functional div
fdiv4 = read.csv(paste0(file_path, "calc_comm_attributes/fdiv/sp4_fdiv.csv")) %>%
  select(-X)

### community weighted trait means
cwm4 = read.csv(paste0(file_path, "calc_comm_attributes/cwm/sp4_cwm.csv"))

### network metrics

## set plot theme
theme_set(theme_classic())

## create standard error function
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

# Format Data ####
## join fdiv + cwm
traitdat = left_join(cwm4, fdiv4, by = c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL", "richness"))

## join struct + trait dat
sp4trait = left_join(sp4, traitdat, by = c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL"))

## join in network metrics
#sp4_allpred = left_join(sp4_sum, netsums, by = c("comp", "rainfall", "ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL"))

## FOLLOW UP ####
## change niche diff to numeric
sp4trait$niche_diff = as.numeric(sp4trait$niche_diff)
## need to figure out which vals of niche diff are causing this to be a character and whether thats' an issue in analyses; for now just change these to NAs

sp4sum = sp4trait %>%
  group_by(comp, rainfall, fdiv, cwm.height, cwm.ldmc, cwm.sla, cwm.rmf, cwm.crsl, cwm.pf, cwm.d,
           ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL) %>%
  summarise(num_feas = sum(feasibility),
            prop_feasible = num_feas/n(),
            mean_niche = mean(niche_diff),
            mean_fitness = mean(fitness_diff),
            se_niche = calcSE(niche_diff),
            se_fitness = calcSE(fitness_diff),
            mean_cpo = mean(comm_pair_overlap),
            se_cpo = calcSE(comm_pair_overlap),
            mean_cpd = mean(comm_pair_diff),
            se_cpd = calcSE(comm_pair_diff))  %>%
  mutate(num.inv = sum(ANAR, BRHO, BRNI, CESO, LOMU, TACA, THIR),
         origin = ifelse(num.inv == 0, "Native",
                         ifelse(num.inv == 4, "Invasive", "Mixed")))

sp4sum$origin = as.factor(sp4sum$origin)

sp4sum = sp4sum %>%
  mutate(origin = fct_relevel(origin, "Mixed", "Native", "Invasive"))

# Explore missing dat####
sp4 = sp4 %>%
  mutate(comp = as.character(paste0(ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL)))

length(unique(sp4$comp))

sp4NA = sp4 %>%
  filter(is.na(feasibility)) %>%
  group_by(comp, rainfall) %>%
  summarise(num = n()) %>%
  ungroup() %>%
  group_by(rainfall) %>%
  summarise(numcomms = n())

# Visualize ####
## RAINFALL ####
pf = ggplot(sp4sum, aes(x=rainfall, y=prop_feasible)) +
  geom_jitter(alpha = 0.15) +
  ylab("Proportion of Coexistence") +
  xlab("") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle("4 Species")

nd = ggplot(sp4sum, aes(x=rainfall, y=mean_niche)) +
  geom_jitter(alpha = 0.15) +
  ylab("Niche Differences") +
  xlab("Rainfall Treatment") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

fd = ggplot(sp4sum, aes(x=rainfall, y=mean_fitness)) +
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

## ORIGIN ####
lpfo = ggplot(sp4sum, aes(x=as.factor(num.inv), y=log(prop_feasible))) +
  geom_jitter(alpha = 0.15) +
  ylab("Log(Prop. Coexistence)") +
  xlab("Number of Invasive Species") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)

pfo = ggplot(sp4sum, aes(x=as.factor(num.inv), y=prop_feasible)) +
  geom_jitter(alpha = 0.15) +
  ylab("Prop. Coexistence") +
  xlab("Number of Invasive Species") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)

ndo = ggplot(sp4sum, aes(x=as.factor(num.inv), y=mean_niche)) +
  geom_jitter(alpha = 0.15) +
  ylab("Niche Differences") +
  xlab("Number of Invasive Species") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)

fdo = ggplot(sp4sum, aes(x=as.factor(num.inv), y=mean_fitness)) +
  geom_jitter(alpha = 0.15) +
  ylab("Fitness Differences") +
  xlab("Number of Invasive Species") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)

ggarrange(pfo, lpfo, ndo, fdo, ncol = 1, nrow = 4, common.legend = TRUE, legend = "bottom", labels = "AUTO")

ggsave(paste0(fig_loc, "origin_violin_plots.png"), width = 6.5, height = 10)














ggplot(sp4sum, aes(x=rainfall, y=log(prop_feasible), color = origin)) +
  scale_color_manual(values = c("#fab14f", "#52BCA3", "#5D69B1")) +
  geom_violin() +
  geom_jitter(alpha = 0.15) +
  geom_boxplot(width=0.1) +
  facet_wrap(~origin) +
  ylab("Log(Proportion of Coexistence)") +
  xlab("Origin") +
  labs(color = "Origin") +
  ggtitle("4 Species") +
  stat_summary(fun.y=median, geom="point", size=3)

sp4_sum_noout = sp4_allpred %>%
  filter(prop_feasible < 0.5)

sp4_sum_noout$origin = as.factor(sp4_sum_noout$origin)

sp4_sum_noout = sp4_sum_noout %>%
  mutate(origin = fct_relevel(origin, "Mixed", "Native", "Invasive"))

## ORIGIN & RAINFALL ####
pf = ggplot(sp4_sum_noout, aes(x=rainfall, y=prop_feasible)) +
  scale_color_manual(values = c("#52BCA3", "#5D69B1", "#fab14f")) +
  geom_jitter(alpha = 0.15, aes(color = origin)) +
  facet_wrap(~origin) +
  ylab("Proportion of Coexistence") +
  xlab("") +
  geom_violin() +
  labs(color = "Origin") +
  theme(text = element_text(size = 15)) +
  ggtitle("4 Species") + 
  stat_summary(fun.y=median, geom="point", size=3, aes(color = origin))

nd = ggplot(sp4_allpred, aes(x=rainfall, y=mean_niche)) +
  scale_color_manual(values = c("#52BCA3", "#5D69B1", "#fab14f")) +
  geom_jitter(alpha = 0.15, aes(color = origin)) +
  facet_wrap(~origin) +
  ylab("Niche Differences") +
  xlab("") +
  geom_violin() +
  labs(color = "Origin") +
  #ggtitle("4 Species") + 
  theme(text = element_text(size = 15)) +
  geom_boxplot(width = 0.1, aes(color = origin)) +
  stat_summary(fun.y=median, geom="point", size=3, aes(color = origin))

fd = ggplot(sp4_allpred, aes(x=rainfall, y=mean_fitness)) +
  scale_color_manual(values = c("#52BCA3", "#5D69B1", "#fab14f")) +
  geom_jitter(alpha = 0.15, aes(color = origin)) +
  facet_wrap(~origin) +
  ylab("Fitness Differences") +
  xlab("Rainfall Treatment") +
  geom_violin() +
  theme(text = element_text(size = 15)) +
  labs(color = "Origin") +
  #ggtitle("4 Species") + 
  geom_boxplot(width = 0.1, aes(color = origin)) +
  stat_summary(fun.y=median, geom="point", size=3, aes(color = origin))

ggarrange(pf, nd, fd, ncol = 1, nrow = 3, common.legend = TRUE, legend = "bottom", labels = "AUTO")

ggsave(paste0(fig_loc, "rain_origin_violin_plots.png"), width = 5.5, height = 8)

ggplot(sp4_sum_noout, aes(x=rainfall, y=prop_feasible)) +
  scale_color_manual(values = c("#52BCA3", "#5D69B1", "#fab14f")) +
  geom_jitter(alpha = 0.15) +
  ylab("Proportion of Coexistence") +
  xlab("") +
  geom_violin() +
  theme(text = element_text(size = 15)) +
  ggtitle("4 Species") + 
  geom_boxplot(width = 0.1)


## FDIV ####
fdivpf = ggplot(sp4sum, aes(x=fdiv, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Proportion of Coexistence") +
  xlab(" ") +
  ggtitle("4 Species")

fdivnd = ggplot(sp4sum, aes(x=fdiv, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Niche Differences") +
  xlab("Functional Diversity") +
  ggtitle(" ")

fdivfd = ggplot(sp4sum, aes(x=fdiv, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Fitness Differences") +
  xlab("") +
  ggtitle(" ")

ggarrange(fdivpf, fdivnd, fdivfd, ncol = 3, nrow = 1, labels = "AUTO")

ggsave(paste0(fig_loc, "fdiv_overall_patterns.png"), width = 7, height = 3)

## CWMS ####
### Niche diff ####
nh = ggplot(sp4sum, aes(x=cwm.height, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("4 Species") +
  ylab("Niche Differences") +
  xlab("CWM Height")

nl = ggplot(sp4sum, aes(x=cwm.ldmc, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM LDMC")

ns = ggplot(sp4sum, aes(x=cwm.sla, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM SLA")

nr = ggplot(sp4sum, aes(x=cwm.rmf, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM RMF")

nc = ggplot(sp4sum, aes(x=cwm.crsl, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab("Niche Differences") +
  xlab("CWM CRSL")

np = ggplot(sp4sum, aes(x=cwm.pf, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM PF")

nd = ggplot(sp4sum, aes(x=cwm.d, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM Diameter")

ggarrange(nh, nl, ns, nr, nc, np, nd, nrow = 2, ncol = 4)

ggsave(paste0(fig_loc, "cwm_ndiff.png"), width = 10, height = 4.5)

### Fitness diff ####
fh = ggplot(sp4sum, aes(x=cwm.height, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("4 Species") +
  ylab("Fitness Differences") +
  xlab("CWM Height")

fl = ggplot(sp4sum, aes(x=cwm.ldmc, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM LDMC")

fs = ggplot(sp4sum, aes(x=cwm.sla, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM SLA")

fr = ggplot(sp4sum, aes(x=cwm.rmf, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM RMF")

fc = ggplot(sp4sum, aes(x=cwm.crsl, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab("Fitness Differences") +
  xlab("CWM CRSL")

fp = ggplot(sp4sum, aes(x=cwm.pf, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM PF")

fd = ggplot(sp4sum, aes(x=cwm.d, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM Diameter")

ggarrange(fh, fl, fs, fr, fc, fp, fd, nrow = 2, ncol = 4)

ggsave(paste0(fig_loc, "cwm_fdiff.png"), width = 10, height = 4.5)

### Prop Coexist ####
pfh = ggplot(sp4sum, aes(x=cwm.height, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("4 Species") +
  ylab("Prop Feasible") +
  xlab("CWM Height")

pfl = ggplot(sp4sum, aes(x=cwm.ldmc, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM LDMC")

pfs = ggplot(sp4sum, aes(x=cwm.sla, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM SLA")

pfr = ggplot(sp4sum, aes(x=cwm.rmf, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM RMF")

pfc = ggplot(sp4sum, aes(x=cwm.crsl, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab("Prop Feasible") +
  xlab("CWM CRSL")

pfp = ggplot(sp4sum, aes(x=cwm.pf, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM PF")

pfd = ggplot(sp4sum, aes(x=cwm.d, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM Diameter")

ggarrange(pfh, pfl, pfs, pfr, pfc, pfp, pfd, nrow = 2, ncol = 4)

ggsave(paste0(fig_loc, "cwm_propfeas.png"), width = 10, height = 4.5)


## INDIRECT INT ####
cpopf = ggplot(sp4_allpred, aes(x=mean_cpo, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Proportion of Coexistence") +
  xlab(" ") 

cpdpf = ggplot(sp4_allpred, aes(x=mean_cpd, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Proportion of Coexistence") +
  xlab(" ") +
  ggtitle("4 Species")

cpond = ggplot(sp4_allpred, aes(x=mean_cpo, y=mean_niche)) +
  geom_point() +
  geom_smooth() +
  ylab("Niche Differences") +
  xlab("Community Pair Overlap") 

cpdnd = ggplot(sp4_allpred, aes(x=mean_cpd, y=mean_niche)) +
  geom_point() +
  geom_smooth() +
  ylab("Niche Differences") +
  xlab("Community Pair Differential") +
  ggtitle(" ")

cpofd = ggplot(sp4_allpred, aes(x=mean_cpo, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Fitness Differences") +
  xlab(" ") 

cpdfd = ggplot(sp4_allpred, aes(x=mean_cpd, y=mean_fitness)) +
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
### asymmetry ####
aspf = ggplot(sp4_allpred, aes(x=mean_asym, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Proportion of Coexistence") +
  xlab("") +
  ggtitle("4 Species")

asnd = ggplot(sp4_allpred, aes(x=mean_asym, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Niche Differences") +
  xlab("Asymmetry") +
  ggtitle("")

asfd = ggplot(sp4_allpred, aes(x=mean_asym, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Fitness Differences") +
  xlab("") +
  ggtitle("")

ggarrange(aspf, asnd, asfd, ncol = 3, nrow = 1, labels = "AUTO")

ggsave(paste0(fig_loc, "asymmetry.png"), width = 7, height = 3)

### skewness ####
skpf = ggplot(sp4_allpred, aes(x=mean_skew, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Proportion of Coexistence") +
  xlab("") +
  ggtitle("4 Species")

sknd = ggplot(sp4_allpred, aes(x=mean_skew, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Niche Differences") +
  xlab("Skewness") +
  ggtitle("")

skfd = ggplot(sp4_allpred, aes(x=mean_skew, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Fitness Differences") +
  xlab("") +
  ggtitle("")

ggarrange(skpf, sknd, skfd, ncol = 3, nrow = 1, labels = "AUTO")

ggsave(paste0(fig_loc, "skewness.png"), width = 7, height = 3)

## N & F v Feasible ####
ggplot(sp4_allpred, aes(x=mean_niche, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Proportion of Coexistence") +
  xlab("Niche Differences") +
  ggtitle("4 Species")

ggplot(sp4_allpred, aes(x=mean_fitness, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Proportion of Coexistence") +
  xlab("Fitness Differences") +
  ggtitle("4 Species")