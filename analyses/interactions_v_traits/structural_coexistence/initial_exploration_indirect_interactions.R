
# Set up ####
file_path = "analyses/interactions_v_traits/structural_coexistence/"

source(paste0(file_path, "calc_comm_attributes/calc_comm_fdiv.R"))

source(paste0(file_path, "reformat_structural_outputs.R"))

theme_set(theme_classic())


# Format Data ####
fdiv_4 = fdiv_list[[1]]

sp4_fdiv = left_join(sp4_clean, fdiv_4, by = c("comp", "ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL"))

sp4_sum = sp4_fdiv %>%
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

fdiv_12 = fdiv_list[[9]]

sp12_fdiv = left_join(sp12_clean, fdiv_12, by = c("comp", "ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL"))

sp12_sum = sp12_fdiv %>%
  group_by(comp, rainfall, fdiv, ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL) %>%
  filter(!is.na(feasibility)) %>%
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


# Visualize ####
## 4 Species ####
ggplot(sp4_sum, aes(x=rainfall, y=log(prop_feasible), color = origin)) +
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

ggplot(sp4_sum, aes(x=rainfall, y=prop_feasible, color = origin)) +
  scale_color_manual(values = c("#fab14f", "#52BCA3", "#5D69B1")) +
  
  geom_jitter(alpha = 0.15) +
  geom_boxplot(width=0.1) +
  facet_wrap(~origin) +
  ylab("Proportion of Coexistence") +
  xlab("Origin") +
  geom_violin() +
  labs(color = "Origin") +
  ggtitle("4 Species") +
  stat_summary(fun.y=median, geom="point", size=3)



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



