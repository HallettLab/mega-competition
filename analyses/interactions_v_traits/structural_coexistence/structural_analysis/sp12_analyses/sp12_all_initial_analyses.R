## 12 species analyses
## explore how rainfall, origin, functional diversity, indirect interactions, and network metrics influence coexistence, niche differences, and fitness differences

# Set up ####
## load packages
library(ggpubr)
library(tidyverse)

## set file paths
file_path = "analyses/interactions_v_traits/structural_coexistence/"

fig_loc = "analyses/interactions_v_traits/structural_coexistence/prelim_figs/sept_2024/sp12/"

## read in data
### coexistence metrics/indirect interactions
sp12 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp12/12_sp_structural_results_20240829.csv"))

### functional div
fdiv12 = read.csv(paste0(file_path, "calc_comm_attributes/fdiv/sp12_fdiv.csv")) %>%
  select(-X)

### community weighted trait means
cwm12 = read.csv(paste0(file_path, "calc_comm_attributes/cwm/sp12_cwm.csv"))

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
traitdat = left_join(cwm12, fdiv12, by = c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL", "richness"))

## join struct + trait dat
sp12trait = left_join(sp12, traitdat, by = c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL")) %>%
  filter(!is.na(feasibility),
         niche_diff != '#NAME?')
## 4 rows have values '#NAME?'; need to go back and figure out what these values actually are. Probably need to run the code again for these specific instances to see if it matters...

sp12trait$niche_diff = as.numeric(sp12trait$niche_diff)

sp12sum = sp12trait %>%
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
  mutate(num.inv = sum(ANAR, BRHO, BRNI, CESO, LOMU, TACA, THIR))

# Explore missing dat####
sp12NA = sp12sum %>%
  group_by(rainfall, prop_feasible) %>%
  summarise(num = n())

# Visualize ####
## RAINFALL ####
nd = ggplot(sp12sum, aes(x=rainfall, y=mean_niche)) +
  ylab("Niche Differences") +
  xlab("Rainfall Treatment") +
  geom_violin() +
  ggtitle("12 Species") + 
  theme(text = element_text(size = 15)) +
  geom_jitter(alpha = 0.15) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)

fd = ggplot(sp12sum, aes(x=rainfall, y=mean_fitness)) +
  ylab("Fitness Differences") +
  xlab("Rainfall Treatment") +
  geom_violin() +
  ggtitle(" ") + 
  theme(text = element_text(size = 15)) +
  geom_jitter(alpha = 0.15) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)

ggarrange(nd, fd, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom", labels = "AUTO")

ggsave(paste0(fig_loc, "sp12_rain_violin_plots.png"), width = 6, height = 4)

## ORIGIN ####
pfo = ggplot(sp12sum, aes(x=as.factor(num.inv), y=prop_feasible)) +
  geom_jitter(alpha = 0.15) +
  ylab("Prop. Coexistence") +
  xlab("Number of Invasive Species") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)

ndo = ggplot(sp12sum, aes(x=as.factor(num.inv), y=mean_niche)) +
  geom_jitter(alpha = 0.15) +
  ylab("Niche Differences") +
  xlab("Number of Invasive Species") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)

fdo = ggplot(sp12sum, aes(x=as.factor(num.inv), y=mean_fitness)) +
  geom_jitter(alpha = 0.15) +
  ylab("Fitness Differences") +
  xlab("Number of Invasive Species") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)

ggarrange(pfo, ndo, fdo, ncol = 1, nrow = 3, common.legend = TRUE, legend = "bottom", labels = "AUTO")

ggsave(paste0(fig_loc, "origin_violin_plots.png"), width = 6.5, height = 8)

## FDIV ####
fdivnd = ggplot(sp12sum, aes(x=fdiv, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Niche Differences") +
  xlab("Functional Diversity") +
  ggtitle("12 Species")

fdivfd = ggplot(sp12sum, aes(x=fdiv, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Fitness Differences") +
  xlab("Functional Diversity") +
  ggtitle(" ")

ggarrange(fdivnd, fdivfd, ncol = 2, nrow = 1, labels = "AUTO")

ggsave(paste0(fig_loc, "sp12_fdiv_overall_patterns.png"), width = 5, height = 3)

## CWMS ####
### Niche diff ####
nh = ggplot(sp12sum, aes(x=cwm.height, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("12 Species") +
  ylab("Niche Differences") +
  xlab("CWM Height")

nl = ggplot(sp12sum, aes(x=cwm.ldmc, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM LDMC")

ns = ggplot(sp12sum, aes(x=cwm.sla, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM SLA")

nr = ggplot(sp12sum, aes(x=cwm.rmf, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM RMF")

nc = ggplot(sp12sum, aes(x=cwm.crsl, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab("Niche Differences") +
  xlab("CWM CRSL")

np = ggplot(sp12sum, aes(x=cwm.pf, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM PF")

nd = ggplot(sp12sum, aes(x=cwm.d, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM Diameter")

ggarrange(nh, nl, ns, nr, nc, np, nd, nrow = 2, ncol = 4)

ggsave(paste0(fig_loc, "cwm_ndiff.png"), width = 10, height = 4.5)

### Fitness diff ####
fh = ggplot(sp12sum, aes(x=cwm.height, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("12 Species") +
  ylab("Fitness Differences") +
  xlab("CWM Height")

fl = ggplot(sp12sum, aes(x=cwm.ldmc, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM LDMC")

fs = ggplot(sp12sum, aes(x=cwm.sla, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM SLA")

fr = ggplot(sp12sum, aes(x=cwm.rmf, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM RMF")

fc = ggplot(sp12sum, aes(x=cwm.crsl, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab("Fitness Differences") +
  xlab("CWM CRSL")

fp = ggplot(sp12sum, aes(x=cwm.pf, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM PF")

fd = ggplot(sp12sum, aes(x=cwm.d, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle(" ") +
  ylab(" ") +
  xlab("CWM Diameter")

ggarrange(fh, fl, fs, fr, fc, fp, fd, nrow = 2, ncol = 4)

ggsave(paste0(fig_loc, "cwm_fdiff.png"), width = 10, height = 4.5)


## INDIRECT INT ####
cpond = ggplot(sp12sum, aes(x=mean_cpo, y=mean_niche)) +
  geom_point() +
  geom_smooth() +
  ylab("Niche Differences") +
  xlab("Community Pair Overlap") 

cpdnd = ggplot(sp12sum, aes(x=mean_cpd, y=mean_niche)) +
  geom_point() +
  geom_smooth() +
  ylab("Niche Differences") +
  xlab("Community Pair Differential") +
  ggtitle(" ")

cpofd = ggplot(sp12sum, aes(x=mean_cpo, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Fitness Differences") +
  xlab(" ") 

cpdfd = ggplot(sp12sum, aes(x=mean_cpd, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Fitness Differences") +
  xlab(" ") +
  ggtitle(" ")

ggarrange(cpdnd, cpdfd,
          cpond, cpofd,
          nrow = 2, ncol = 2, labels = "AUTO")

ggsave(paste0(fig_loc, "indirect_interactions.png"), width = 6, height = 4)

### Network Metrics ####
#### asymmetry ####

#### skewness ####

# Summary ####
sp12fsum = sp12sum %>%
  group_by(prop_feasible) %>%
  summarise(numcomm = n())
