## 6 species analyses
## explore how rainfall, origin, functional diversity, indirect interactions, and network metrics influence coexistence, niche differences, and fitness differences

# Set up ####
## load packages
library(ggpubr)

## set file paths
file_path = "analyses/interactions_v_traits/structural_coexistence/"

fig_loc = "analyses/interactions_v_traits/structural_coexistence/prelim_figs/sept_2024/sp6/"

## read in data
### coexistence metrics/indirect interactions
source(paste0(file_path, "structural_analysis/sp6_analyses/sp6_clean_structural.R"))

### functional div
fdiv6 = read.csv(paste0(file_path, "calc_comm_attributes/fdiv/sp6_fdiv.csv")) %>%
  select(-X, -comp)

### community weighted trait means
cwm6 = read.csv(paste0(file_path, "calc_comm_attributes/cwm/sp6_cwm.csv"))

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
traitdat = left_join(cwm6, fdiv6, by = c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL", "richness"))

sp6trait = left_join(sp6_clean, traitdat, by = c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL"))

sp6trait$niche_diff = as.numeric(sp6trait$niche_diff)

sp6sum = sp6trait %>%
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
                         ifelse(num.inv == 6, "Invasive", "Mixed")))

# Visualize ####
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

## ORIGIN ####
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


ndo = ggplot(sp6sum, aes(x=as.factor(num.inv), y=mean_niche)) +
  geom_jitter(alpha = 0.15) +
  ylab("Niche Differences") +
  xlab("Number of Invasive Species") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)


fdo = ggplot(sp6sum, aes(x=as.factor(num.inv), y=mean_fitness)) +
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
### asymmetry ####

### skewness ####

# Summary ####
sp6fsum = sp6sum %>%
  group_by(prop_feasible) %>%
  summarise(numcomm = n())

sum(sp6fsum[sp6fsum$prop_feasible != 0,]$numcomm)

# Interactions ####
## CWMS/Rainfall ####
### Niche diff ####
#66C5CC,#F6CF71,#F89C74,#DCB0F2,#87C55F,#9EB9F3,#FE88B1,#C9DB74,#8BE0A4,#B497E7,#D3B484,#B3B3B3
#### AG traits ####
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

ggarrange(nhr, nlr, nsr, ncol = 3, nrow = 1, common.legend = T)
ggsave(paste0(fig_loc, "cwm_ag_rainfall_ndiff.png"), width = 10, height = 3.5)

#### BG Traits ####
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

### Fitness diff ####
#### AG traits ####
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

ggarrange(fhr, flr, fsr, ncol = 3, nrow = 1, common.legend = T, legend = "bottom")
ggsave(paste0(fig_loc, "cwm_ag_rainfall_fdiff.png"), width = 10, height = 3.5)

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
