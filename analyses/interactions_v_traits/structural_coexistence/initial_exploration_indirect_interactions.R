

ggplot(allcomm_fdiv, aes(x=fdiv, y=comm_pair_overlap))+
  geom_point()


ggplot(allcomm_fdiv, aes(x=fdiv, y=comm_pair_diff))+
  geom_point()

allcomm_sum = allcomm_fdiv %>%
  group_by(comp, rainfall, fdiv, ACAM, AMME, GITR, LENI, MAEL, MICA, PLER, PLNO, TWIL) %>%
  summarise(num_feas = sum(feasibility),
            prop_feasible = num_feas/n(),
            mean_niche = mean(niche_diff),
            mean_fitness = mean(fitness_diff),
            se_niche = calcSE(niche_diff),
            se_fitness = calcSE(fitness_diff),
            mean_cpo = mean(comm_pair_overlap),
            se_cpo = calcSE(comm_pair_overlap),
            mean_cpd = mean(comm_pair_diff),
            se_cpd = calcSE(comm_pair_diff)) #%>%
  #filter(!(origin == "Mixed" & num.inv == 4))

ggplot(allcomm_sum, aes(x=rainfall, y=mean_cpo)) +
  geom_boxplot()

ggplot(allcomm_sum, aes(x=rainfall, y=mean_cpd)) +
  geom_boxplot()


ggplot(allcomm_sum, aes(x=fdiv, y=mean_cpd)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(allcomm_sum, aes(x=fdiv, y=mean_cpo)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(allcomm_sum, aes(x=mean_cpo, y=prop_feasible, color = rainfall))+
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Community Pair Overlap") +
  ylab("Proportion of Feasible Comm") +
  labs(color = "Rainfall")
ggsave(paste0(fig_loc, "cpo_initfig.png"), width= 5, height = 3.5)


ggplot(allcomm_sum, aes(x=mean_cpd, y=prop_feasible, color = rainfall))+
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Community Pair Differential") +
  ylab("Proportion of Feasible Comm") +
  labs(color = "Rainfall")

ggsave(paste0(fig_loc, "cpd_initfig.png"), width= 5, height = 3.5)



