## 12 species analyses
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

fig_loc = "analyses/interactions_v_traits/structural_coexistence/prelim_figs/sept_2024/"

## read in data
sp12 = read.csv(paste0(file_path, "run_structural/structural_results_files/12_sp_structural_results_20240829.csv"))

fdiv12 = read.csv(paste0(file_path, "calc_comm_attributes/sp12_fdiv.csv")) %>%
  select(-X)

## set plot theme
theme_set(theme_classic())

# Format Data ####
## 12 species ####
sp12_fdiv = left_join(sp12, fdiv12, by = c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL")) %>%
  filter(!is.na(feasibility),
         niche_diff != '#NAME?')
## 4 rows have values '#NAME?'; need to go back and figure out what these values actually are. Probably need to run the code again for these specific instances to see if it matters...

sp12_fdiv$niche_diff = as.numeric(sp12_fdiv$niche_diff)

sp12_sum = sp12_fdiv %>%
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

#sp12_allpred = left_join(sp12_sum, netsums, by = c("comp", "rainfall", "ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL"))

# Visualize ####
## COEXISTENCE ####
ggplot(sp12_sum, aes(x=rainfall)) +
  geom_bar() +
  ggtitle("12 Species Comm w/no alpha NAs")

ggsave(paste0(fig_loc, "barplot_comm_nums_rainfall.png"), width= 4, height = 3)

ggplot(sp12_sum, aes(x=as.factor(prop_feasible))) +
  geom_bar() +
  ggtitle("12 Species")

ggsave(paste0(fig_loc, "barplot_feasibility_sp12.png"), width= 3, height = 3)


## RAINFALL ####
nd = ggplot(sp12_sum, aes(x=rainfall, y=mean_niche)) +
  ylab("Niche Differences") +
  xlab("Rainfall Treatment") +
  geom_violin() +
  ggtitle("12 Species") + 
  theme(text = element_text(size = 15)) +
  geom_jitter(alpha = 0.15) +
  geom_boxplot(width = 0.1) +
  stat_summary(fun.y=median, geom="point", size=3)

fd = ggplot(sp12_sum, aes(x=rainfall, y=mean_fitness)) +
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

### FDIV ####
fdivnd = ggplot(sp12_sum, aes(x=fdiv, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Niche Differences") +
  xlab("Functional Diversity") +
  ggtitle("12 Species")

fdivfd = ggplot(sp12_sum, aes(x=fdiv, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Fitness Differences") +
  xlab("Functional Diversity") +
  ggtitle(" ")

ggarrange(fdivnd, fdivfd, ncol = 2, nrow = 1, labels = "AUTO")

ggsave(paste0(fig_loc, "sp12_fdiv_overall_patterns.png"), width = 5, height = 3)

### INDIRECT INT ####
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

### Network Metrics ####
#### asymmetry ####
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

#### skewness ####
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



### N & F v Feasible ####
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























ggplot(sp4_allpred, aes(x=mean_skew, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Proportion of Coexistence") +
  xlab("Skewness") +
  ggtitle("4 Species")

ggplot(sp4_allpred, aes(x=mean_asym, y=prop_feasible)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylab("Proportion of Coexistence") +
  xlab("Asymmetry") +
  ggtitle("4 Species")

ggplot(sp4_allpred, aes(x=fdiv, y=mean_asym)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("4 Species")

ggplot(sp4_allpred, aes(x=fdiv, y=mean_skew)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("4 Species")



## 12 Species ####
ggplot(sp12_sum, aes(x=rainfall, y=prop_feasible, color = as.factor(num.inv))) +
  #scale_color_manual(values = c("#fab14f", "#52BCA3", "#5D69B1")) +
  geom_violin() +
  #geom_jitter(alpha = 0.15) +
  geom_boxplot(width=0.1) +
  facet_wrap(~as.factor(num.inv)) +
  ylab("Log(Proportion of Coexistence)") +
  ggtitle("12 Species") +
  stat_summary(fun.y=median, geom="point", size=3)


ggplot(sp12_sum, aes(x=rainfall, y=mean_niche, color = as.factor(num.inv))) +
  #scale_color_manual(values = c("#fab14f", "#52BCA3", "#5D69B1")) +
  geom_violin() +
  #geom_jitter(alpha = 0.15) +
  geom_boxplot(width=0.1) +
  facet_wrap(~as.factor(num.inv)) +
  ylab("Niche Differences") +
  ggtitle("12 Species") +
  stat_summary(fun.y=median, geom="point", size=3)

ggplot(sp12_sum, aes(x=fdiv, y=mean_cpd)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(sp12_sum, aes(x=fdiv, y=mean_niche)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(sp12_sum, aes(x=fdiv, y=mean_fitness)) +
  geom_point() +
  geom_smooth(method = "lm")

hist(sp12_sum$mean_cpd)

ggplot(sp4_sum, aes(x=as.factor(num.inv), y=log(mean_cpo))) +
  geom_boxplot()


ggplot(sp4_sum, aes(x=rainfall, y=log(mean_cpo))) +
  geom_boxplot()

ggplot(sp4_sum, aes(x=rainfall, y=log(mean_cpd))) +
  geom_boxplot()
ggplot(sp4_sum, aes(x=as.factor(num.inv), y=log(mean_cpd))) +
  geom_boxplot()
ggplot(sp4_sum, aes(x=as.factor(num.inv), y=mean_cpd)) +
  geom_boxplot()

ggplot(sp4_sum, aes(x=fdiv, y=mean_cpd)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(sp4_sum, aes(x=fdiv, y=mean_cpo)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(sp4_sum, aes(x=mean_cpo, y=prop_feasible, color = rainfall))+
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Community Pair Overlap") +
  ylab("Proportion of Feasible Comm") +
  labs(color = "Rainfall")
ggsave(paste0(fig_loc, "cpo_initfig.png"), width= 5, height = 3.5)


ggplot(sp4_sum, aes(x=mean_cpd, y=prop_feasible, color = rainfall))+
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Community Pair Differential") +
  ylab("Proportion of Feasible Comm") +
  labs(color = "Rainfall")

ggsave(paste0(fig_loc, "cpd_initfig.png"), width= 5, height = 3.5)



