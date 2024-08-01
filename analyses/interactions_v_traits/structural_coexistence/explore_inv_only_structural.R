
## In progress transferring over to explore_nat_only_structural so there is only one script

# Clean data ####



## Explore Raw Data ####
ggplot(allinv, aes(x=feasibility)) +
  geom_bar() +
  facet_wrap(~treatment)

nrow(allinv[allinv$feasibility == 1,])
## 228 feasible comm out of total of 14000

## niche diffs
ggplot(allinv, aes(x=niche_diff)) +
  geom_histogram() +
  facet_wrap(~treatment)

## fitness diffs
ggplot(allinv, aes(x=fitness_diff)) +
  geom_histogram() +
  facet_wrap(~treatment)

# Summarise ####
## Prop Feasible ####





ggplot(invmean_feas, aes(x=treatment, y=mean_prop_feas)) +
  geom_col(color = "black", fill = "lightgray") +
  geom_errorbar(aes(ymin = mean_prop_feas - se_prop_feas, ymax = mean_prop_feas + se_prop_feas), width = 0.25) +
  ylab("Mean Proportion of Feasible Communities")




hist(invdiff_prop_feas$feas_diff)

### visualise ####
ggplot(invdiff_prop_feas, aes(x=feas_diff)) +
  geom_histogram(color = "black", fill = "lightgray", bins = 20) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Difference in Feasibility (D-C)") 



ggplot(prop_feas, aes(x=as.factor(comp), y=prop_feasible, fill = as.factor(w_legume))) +
  geom_bar(stat = 'identity') +
  facet_wrap(~treatment, nrow = 2, ncol = 1)+
  ggtitle("Invasive only 4sp Comm") +
  xlab("Composition") +
  scale_fill_manual(values = c("#A5AA99", "#24796C")) +
  ylab("Prop Feasible Comm (200 draws)")
ggsave(paste0(fig_loc, "inv_only_4spcomm_legume.png"), width = 10, height = 6)

ggplot(prop_feas, aes(x=as.factor(comp), y=prop_feasible)) +
  geom_bar(stat = 'identity') +
  ggtitle("Invasive only 4sp Comm") +
  facet_wrap(~treatment, nrow = 2, ncol = 1)+
  xlab("Composition") +
  ylab("Prop Feasible Comm (200 draws)")
ggsave(paste0(fig_loc, "inv_only_C_4spcomm.png"), width = 7, height = 6)



ggplot(prop_feas, aes(x=mean_niche, y=mean_fitness, color = as.factor(w_legume))) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_fitness - se_fitness, ymax = mean_fitness + se_fitness), width = 0.05)+
  geom_errorbarh(aes(xmax = mean_niche + se_niche, xmin = mean_niche - se_niche), height = 1)
ggsave(paste0(fig_loc, "inv_only_D_niche_fitness_diffs_scatter.png"), width = 6, height = 3)

## Prop Feasible by Sp ####
num_pres = allinv %>%
  pivot_longer(c(1:7), names_to = "species", values_to = "PA") %>%
  filter(PA != 0) %>%
  group_by(species, comp, treatment) %>%
  summarise(num_feas = sum(feasibility)) %>%
  mutate(feas_PA = ifelse(num_feas > 0, 1, 0)) %>%
  ungroup() %>%
  group_by(species, treatment) %>%
  summarise(num_feas_present = sum(feas_PA))

ggplot(num_pres, aes(x=species, y=num_feas_present)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~treatment) +
  ylab("Number of Feasible Communities where Present") +
  xlab("Species")

ggsave(paste0(fig_loc, "inv_num_comm_present.png"), width = 10, height = 4)
