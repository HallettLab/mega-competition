## 5 species analyses
## explore how rainfall, origin, functional diversity, indirect interactions, and network metrics influence coexistence, niche differences, and fitness differences

# Set up ####
## load packages
library(ggpubr)
library(tidyverse)

## set file paths
file_path = "analyses/interactions_v_traits/structural_coexistence/"

fig_loc = "analyses/interactions_v_traits/structural_coexistence/prelim_figs/sept_2024/sp5/"

## read in data
### coexistence metrics/indirect interactions
sp5 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp5/5_sp_structural_results_20240904.csv"))

### functional div
fdiv5 = read.csv(paste0(file_path, "calc_comm_attributes/fdiv/sp5_fdiv.csv")) %>%
  select(-X)

### community weighted trait means
cwm5 = read.csv(paste0(file_path, "calc_comm_attributes/cwm/sp5_cwm.csv"))

### network metrics

## set plot theme
theme_set(theme_classic())

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

# Format Data ####
## join fdiv + cwm
traitdat = left_join(cwm5, fdiv5, by = c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL", "richness"))

sp5trait = left_join(sp5, traitdat, by = c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL"))

sp5trait$niche_diff = as.numeric(sp5trait$niche_diff)

sp5sum = sp5trait %>%
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
                         ifelse(num.inv == 5, "Invasive", "Mixed")))

# Explore missing dat####
sp5 = sp5 %>%
  mutate(comp = as.character(paste0(ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL)))

length(unique(sp5$comp))

sp5NA = sp5 %>%
  filter(is.na(feasibility)) %>%
  group_by(comp, rainfall) %>%
  summarise(num = n()) %>%
  ungroup() %>%
  group_by(rainfall) %>%
  summarise(numcomms = n())

# Visualize ####
## RAINFALL ####
pf = ggplot(sp5sum, aes(x=rainfall, y=prop_feasible)) +
  geom_jitter(alpha = 0.15) +
  ylab("Proportion of Coexistence") +
  xlab("") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle("5 Species")

nd = ggplot(sp5sum, aes(x=rainfall, y=mean_niche)) +
  geom_jitter(alpha = 0.15) +
  ylab("Niche Differences") +
  xlab("Rainfall Treatment") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

fd = ggplot(sp5sum, aes(x=rainfall, y=mean_fitness)) +
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
lpfo = ggplot(sp5sum, aes(x=as.factor(num.inv), y=log(prop_feasible))) +
  geom_jitter(alpha = 0.15) +
  ylab("Log(Prop. Coexistence)") +
  xlab("Number of Invasive Species") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)

pfo = ggplot(sp5sum, aes(x=as.factor(num.inv), y=prop_feasible)) +
  geom_jitter(alpha = 0.15) +
  ylab("Prop. Coexistence") +
  xlab("Number of Invasive Species") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)


ndo = ggplot(sp5sum, aes(x=as.factor(num.inv), y=mean_niche)) +
  geom_jitter(alpha = 0.15) +
  ylab("Niche Differences") +
  xlab("Number of Invasive Species") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)


fdo = ggplot(sp5sum, aes(x=as.factor(num.inv), y=mean_fitness)) +
  geom_jitter(alpha = 0.15) +
  ylab("Fitness Differences") +
  xlab("Number of Invasive Species") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)

ggarrange(pfo, lpfo, ndo, fdo, ncol = 1, nrow = 4, common.legend = TRUE, legend = "bottom", labels = "AUTO")

ggsave(paste0(fig_loc, "origin_violin_plots.png"), width = 6.5, height = 10)

## FDIV ####
fdivpf = ggplot(sp5sum, aes(x=fdiv, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Proportion of Coexistence") +
  xlab(" ") +
  ggtitle("5 Species")

fdivnd = ggplot(sp5sum, aes(x=fdiv, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Niche Differences") +
  xlab("Functional Diversity") +
  ggtitle(" ")

fdivfd = ggplot(sp5sum, aes(x=fdiv, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Fitness Differences") +
  xlab("") +
  ggtitle(" ")

ggarrange(fdivpf, fdivnd, fdivfd, ncol = 3, nrow = 1, labels = "AUTO")

ggsave(paste0(fig_loc, "fdiv_overall_patterns.png"), width = 7, height = 3)

## CWMS ####
### Niche diff ####
nh = ggplot(sp5sum, aes(x=cwm.height, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("5 Species") +
  ylab("Niche Differences") +
  xlab("CWM Height")

nl = ggplot(sp5sum, aes(x=cwm.ldmc, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM LDMC")

ns = ggplot(sp5sum, aes(x=cwm.sla, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM SLA")

nr = ggplot(sp5sum, aes(x=cwm.rmf, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM RMF")

nc = ggplot(sp5sum, aes(x=cwm.crsl, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab("Niche Differences") +
  xlab("CWM CRSL")

np = ggplot(sp5sum, aes(x=cwm.pf, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM PF")

nd = ggplot(sp5sum, aes(x=cwm.d, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM Diameter")

ggarrange(nh, nl, ns, nr, nc, np, nd, nrow = 2, ncol = 4)

ggsave(paste0(fig_loc, "cwm_ndiff.png"), width = 10, height = 4.5)

### Fitness diff ####
fh = ggplot(sp5sum, aes(x=cwm.height, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("5 Species") +
  ylab("Fitness Differences") +
  xlab("CWM Height")

fl = ggplot(sp5sum, aes(x=cwm.ldmc, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM LDMC")

fs = ggplot(sp5sum, aes(x=cwm.sla, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM SLA")

fr = ggplot(sp5sum, aes(x=cwm.rmf, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM RMF")

fc = ggplot(sp5sum, aes(x=cwm.crsl, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab("Fitness Differences") +
  xlab("CWM CRSL")

fp = ggplot(sp5sum, aes(x=cwm.pf, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM PF")

fd = ggplot(sp5sum, aes(x=cwm.d, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM Diameter")

ggarrange(fh, fl, fs, fr, fc, fp, fd, nrow = 2, ncol = 4)

ggsave(paste0(fig_loc, "cwm_fdiff.png"), width = 10, height = 4.5)

### Prop Coexist ####
pfh = ggplot(sp5sum, aes(x=cwm.height, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("5 Species") +
  ylab("Prop Feasible") +
  xlab("CWM Height")

pfl = ggplot(sp5sum, aes(x=cwm.ldmc, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM LDMC")

pfs = ggplot(sp5sum, aes(x=cwm.sla, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM SLA")

pfr = ggplot(sp5sum, aes(x=cwm.rmf, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM RMF")

pfc = ggplot(sp5sum, aes(x=cwm.crsl, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab("Prop Feasible") +
  xlab("CWM CRSL")

pfp = ggplot(sp5sum, aes(x=cwm.pf, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM PF")

pfd = ggplot(sp5sum, aes(x=cwm.d, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM Diameter")

ggarrange(pfh, pfl, pfs, pfr, pfc, pfp, pfd, nrow = 2, ncol = 4)

ggsave(paste0(fig_loc, "cwm_propfeas.png"), width = 10, height = 4.5)

## INDIRECT INT ####
cpopf = ggplot(sp5sum, aes(x=mean_cpo, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Proportion of Coexistence") +
  xlab(" ") 

cpdpf = ggplot(sp5sum, aes(x=mean_cpd, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Proportion of Coexistence") +
  xlab(" ") +
  ggtitle("4 Species")

cpond = ggplot(sp5sum, aes(x=mean_cpo, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Niche Differences") +
  xlab("Community Pair Overlap") 

cpdnd = ggplot(sp5sum, aes(x=mean_cpd, y=mean_niche)) +
  geom_point() +
  geom_smooth() +
  ylab("Niche Differences") +
  xlab("Community Pair Differential") +
  ggtitle(" ")

cpofd = ggplot(sp5sum, aes(x=mean_cpo, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Fitness Differences") +
  xlab(" ") 

cpdfd = ggplot(sp5sum, aes(x=mean_cpd, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Fitness Differences") +
  xlab(" ") +
  ggtitle(" ")

ggarrange(cpdpf, cpdnd, cpdfd,
          cpopf, cpond, cpofd,
          nrow = 2, ncol = 3, labels = "AUTO")

ggsave(paste0(fig_loc, "indirect_interactions.png"), width = 8, height = 6)

### Rainfall x Indirect interactions ####
ggplot(sp5sum, aes(x = rainfall, y = mean_cpd)) +
  geom_jitter(alpha = 0.15) +
  xlab("Rainfall Treatment") +
  ylab("Community Pair Differential") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)

ggplot(sp5sum, aes(x = rainfall, y = mean_cpo)) +
  geom_jitter(alpha = 0.15) +
  xlab("Rainfall Treatment") +
  ylab("Community Pair Overlap") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)

## Network Metrics ####
### asymmetry ####


### skewness ####

# Summary ####
sp5fsum = sp5sum %>%
  group_by(prop_feasible) %>%
  summarise(numcomm = n())

sum(sp5fsum[sp5fsum$prop_feasible != 0 & !is.na(sp5fsum$prop_feasible),]$numcomm)
