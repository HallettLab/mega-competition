## 8 species analyses
## explore how rainfall, origin, functional diversity, indirect interactions, and network metrics influence coexistence, niche differences, and fitness differences

# Set up ####
## load packages
library(ggpubr)

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

## set file paths
file_path = "analyses/interactions_v_traits/structural_coexistence/"

fig_loc = "analyses/interactions_v_traits/structural_coexistence/prelim_figs/sept_2024/sp8/"

## read in data
sp8 = source(paste0(file_path, "structural_analysis/sp8_analyses/sp8_clean_structural.R"))

fdiv8 = read.csv(paste0(file_path, "calc_comm_attributes/sp8_fdiv.csv"))

fdiv8$comp = as.character(fdiv8$comp)

## set plot theme
theme_set(theme_classic())

# Format Data ####
sp8fdiv = left_join(sp8_clean, fdiv8, by = c("comp", "ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL"))

sp8fdiv$niche_diff = as.numeric(sp8fdiv$niche_diff)

check_NAs = sp8fdiv %>%
  filter(is.na(niche_diff))
## need to figure out why there were 161 non-numeric values.

sp8_sum = sp8fdiv %>%
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
  mutate(num.inv = sum(ANAR, BRHO, BRNI, CESO, LOMU, TACA, THIR))

#sp8_allpred = left_join(sp8_sum, netsums, by = c("comp", "rainfall", "ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL"))

# Visualize ####
## RAINFALL ####
pf = ggplot(sp8_sum, aes(x=rainfall, y=prop_feasible)) +
  geom_jitter(alpha = 0.15) +
  ylab("Proportion of Coexistence") +
  xlab("") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle("8 Species")

nd = ggplot(sp8_sum, aes(x=rainfall, y=mean_niche)) +
  geom_jitter(alpha = 0.15) +
  ylab("Niche Differences") +
  xlab("Rainfall Treatment") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

fd = ggplot(sp8_sum, aes(x=rainfall, y=mean_fitness)) +
  geom_jitter(alpha = 0.15) +
  ylab("Fitness Differences") +
  xlab("") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3) +
  ggtitle(" ")

ggarrange(pf, nd, fd, ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom", labels = "AUTO")

ggsave(paste0(fig_loc, "rain_origin_violin_plots_sp8.png"), width = 9, height = 3.5)

## ORIGIN ####
lpfo = ggplot(sp8_sum, aes(x=as.factor(num.inv), y=log(prop_feasible))) +
  geom_jitter(alpha = 0.15) +
  ylab("Log(Prop. Coexistence)") +
  xlab("Number of Invasive Species") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)

pfo = ggplot(sp8_sum, aes(x=as.factor(num.inv), y=prop_feasible)) +
  geom_jitter(alpha = 0.15) +
  ylab("Prop. Coexistence") +
  xlab("Number of Invasive Species") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)


ndo = ggplot(sp8_sum, aes(x=as.factor(num.inv), y=mean_niche)) +
  geom_jitter(alpha = 0.15) +
  ylab("Niche Differences") +
  xlab("Number of Invasive Species") +
  theme(text = element_text(size = 15)) +
  geom_violin(fill = adjustcolor("white", alpha.f = 0.5), size = 0.7) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)


fdo = ggplot(sp8_sum, aes(x=as.factor(num.inv), y=mean_fitness)) +
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
fdivpf = ggplot(sp8_sum, aes(x=fdiv, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Proportion of Coexistence") +
  xlab(" ") +
  ggtitle("8 Species")

fdivnd = ggplot(sp8_sum, aes(x=fdiv, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Niche Differences") +
  xlab("Functional Diversity") +
  ggtitle(" ")

fdivfd = ggplot(sp8_sum, aes(x=fdiv, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Fitness Differences") +
  xlab("") +
  ggtitle(" ")

ggarrange(fdivpf, fdivnd, fdivfd, ncol = 3, nrow = 1, labels = "AUTO")

ggsave(paste0(fig_loc, "fdiv_overall_patterns.png"), width = 7, height = 3)

## INDIRECT INT ####
cpopf = ggplot(sp8_sum, aes(x=mean_cpo, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Proportion of Coexistence") +
  xlab(" ") 

cpdpf = ggplot(sp8_sum, aes(x=mean_cpd, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Proportion of Coexistence") +
  xlab(" ") +
  ggtitle("8 Species")

cpond = ggplot(sp8_sum, aes(x=mean_cpo, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Niche Differences") +
  xlab("Community Pair Overlap") 

cpdnd = ggplot(sp8_sum, aes(x=mean_cpd, y=mean_niche)) +
  geom_point() +
  geom_smooth() +
  ylab("Niche Differences") +
  xlab("Community Pair Differential") +
  ggtitle(" ")

cpofd = ggplot(sp8_sum, aes(x=mean_cpo, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Fitness Differences") +
  xlab(" ") 

cpdfd = ggplot(sp8_sum, aes(x=mean_cpd, y=mean_fitness)) +
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
