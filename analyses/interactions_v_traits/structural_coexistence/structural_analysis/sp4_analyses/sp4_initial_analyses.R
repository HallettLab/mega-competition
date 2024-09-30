## 4 species analyses
## explore how rainfall, origin, functional diversity, indirect interactions, and network metrics influence coexistence, niche differences, and fitness differences

# Set up ####
## load packages
library(ggpubr)
library(tidyverse)

## set file paths
file_path = "analyses/interactions_v_traits/structural_coexistence/"

fig_loc = "analyses/interactions_v_traits/structural_coexistence/prelim_figs/sept_2024/sp4/"

## read in data
sp4 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp4/4_sp_structural_results_20240828.csv"))

fdiv4 = read.csv(paste0(file_path, "calc_comm_attributes/sp4_fdiv.csv")) %>%
  select(-X)

cwm4 = read.csv(paste0(file_path, "calc_comm_attributes/4_sp_cwm_20240916.csv"))

## set plot theme
theme_set(theme_classic())

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

# Format Data ####
sp4fdiv = left_join(sp4, fdiv4, by = c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL"))

sp4fdiv$niche_diff = as.numeric(sp4fdiv$niche_diff)

sp4sum = sp4fdiv %>%
  group_by(comp, rainfall, fdiv, ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL) %>%
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

sp4_allpred = left_join(sp4_sum, netsums, by = c("comp", "rainfall", "ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL"))

sp4_allpred$origin = as.factor(sp4_allpred$origin)

sp4_allpred = sp4_allpred %>%
  mutate(origin = fct_relevel(origin, "Mixed", "Native", "Invasive"))

# Visualize ####
ggplot(sp4_allpred, aes(x=rainfall, y=log(prop_feasible), color = origin)) +
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
fdivpf = ggplot(sp4_allpred, aes(x=fdiv, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Proportion of Coexistence") +
  xlab(" ") +
  ggtitle("4 Species")

fdivnd = ggplot(sp4_allpred, aes(x=fdiv, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Niche Differences") +
  xlab("Functional Diversity") +
  ggtitle(" ")

fdivfd = ggplot(sp4_allpred, aes(x=fdiv, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Fitness Differences") +
  xlab("") +
  ggtitle(" ")

ggarrange(fdivpf, fdivnd, fdivfd, ncol = 3, nrow = 1, labels = "AUTO")

ggsave(paste0(fig_loc, "fdiv_overall_patterns.png"), width = 7, height = 3)

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