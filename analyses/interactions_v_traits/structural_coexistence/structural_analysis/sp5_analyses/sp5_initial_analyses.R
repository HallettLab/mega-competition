## 5 species analyses
## explore how rainfall, origin, functional diversity, indirect interactions, and network metrics influence coexistence, niche differences, and fitness differences

# Set up ####
## load packages
library(ggpubr)

## set file paths
file_path = "analyses/interactions_v_traits/structural_coexistence/"

fig_loc = "analyses/interactions_v_traits/structural_coexistence/prelim_figs/sept_2024/sp5/"

## read in data
sp5 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp5/5_sp_structural_results_20240904.csv"))

fdiv5 = read.csv(paste0(file_path, "calc_comm_attributes/sp5_fdiv.csv")) %>%
  select(-X)

## set plot theme
theme_set(theme_classic())

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

# Format Data ####
sp5fdiv = left_join(sp5, fdiv5, by = c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL"))

sp5fdiv$niche_diff = as.numeric(sp5fdiv$niche_diff)

sp5sum = sp5fdiv %>%
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
                         ifelse(num.inv == 5, "Invasive", "Mixed")))


#sp4_allpred = left_join(sp4_sum, netsums, by = c("comp", "rainfall", "ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL"))

sp4_allpred$origin = as.factor(sp4_allpred$origin)

sp4_allpred = sp4_allpred %>%
  mutate(origin = fct_relevel(origin, "Mixed", "Native", "Invasive"))

# Visualize ####
ggplot(sp5sum, aes(x=rainfall, y=log(prop_feasible))) +
  scale_color_manual(values = c("#fab14f", "#52BCA3", "#5D69B1")) +
  geom_violin() +
  geom_jitter(alpha = 0.15) +
  geom_boxplot(width=0.1) +

  ylab("Log(Proportion of Coexistence)") +
 
  ggtitle("4 Species") +
  stat_summary(fun.y=median, geom="point", size=3)

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