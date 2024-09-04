
file_path = "analyses/interactions_v_traits/structural_coexistence/"

source(paste0(file_path, "calc_comm_attributes/calc_comm_fdiv.R"))

source(paste0(file_path, "reformat_structural_outputs.R"))

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



