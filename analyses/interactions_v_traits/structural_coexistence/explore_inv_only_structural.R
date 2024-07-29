
# Set up Env ####
library(tidyverse)

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

fig_loc = "analyses/interactions_v_traits/structural_coexistence/prelim_figs/"
theme_set(theme_classic())

# Read in data ####
invcommD = read.csv("analyses/interactions_v_traits/structural_coexistence/run_structural/inv_only_D_structural_results_20240729.csv")

invcommC = read.csv("analyses/interactions_v_traits/structural_coexistence/run_structural/inv_only_C_structural_results_20240729.csv")

# Clean data ####
invcommD_vis = invcommD %>%
  filter(!is.na(ANAR))%>%
  mutate(comp = paste0(ANAR, BRHO, CESO, LOMU, TACA, THIR),
         treatment = "D")

## set up for visualisation
invcommC_vis = invcommC %>%
  filter(!is.na(BRHO))%>%
  mutate(comp = paste0(ANAR, BRHO, CESO, LOMU, TACA, THIR),
         treatment = "C")

## join together
allinv = rbind(invcommC_vis, invcommD_vis) %>%
  select(-X)

## Explore Raw Data ####
ggplot(allinv, aes(x=feasibility)) +
  geom_bar() +
  facet_wrap(~treatment)

nrow(allinv[allinv$feasibility == 1,])
## 107 feasible comm out of total of 6000

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
prop_feas = allinv %>%
  mutate(comp = paste0(ANAR, BRHO, CESO, LOMU, TACA, THIR)) %>%
  group_by(comp, treatment) %>%
  summarise(num_feas = sum(feasibility),
            prop_feasible = num_feas/n(),
            mean_niche = mean(niche_diff),
            mean_fitness = mean(fitness_diff),
            se_niche = calcSE(niche_diff),
            se_fitness = calcSE(fitness_diff)) %>%
  mutate(w_legume = ifelse(substr(comp, start = 6, stop = 6) == 1, 1, 0)) %>%
  filter(!is.na(num_feas))

ggplot(prop_feas, aes(x=as.factor(comp), y=prop_feasible, fill = as.factor(w_legume))) +
  geom_bar(stat = 'identity') +
  facet_wrap(~treatment, nrow = 2, ncol = 1)+
  ggtitle("Invasive only 4sp Comm") +
  xlab("Composition") +
  scale_fill_manual(values = c("#A5AA99", "#24796C")) +
  ylab("Prop Feasible Comm (200 draws)")
ggsave(paste0(fig_loc, "inv_only_C_4spcomm_legume.png"), width = 10, height = 3)

ggplot(prop_feas, aes(x=as.factor(comp), y=prop_feasible)) +
  geom_bar(stat = 'identity') +
  ggtitle("Invasive only 4sp Comm") +
  xlab("Composition") +
  ylab("Prop Feasible Comm (200 draws)")
ggsave(paste0(fig_loc, "inv_only_C_4spcomm.png"), width = 7, height = 3)



ggplot(prop_feas, aes(x=mean_niche, y=mean_fitness, color = as.factor(w_legume))) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_fitness - se_fitness, ymax = mean_fitness + se_fitness), width = 0.05)+
  geom_errorbarh(aes(xmax = mean_niche + se_niche, xmin = mean_niche - se_niche), height = 1)
ggsave(paste0(fig_loc, "inv_only_D_niche_fitness_diffs_scatter.png"), width = 6, height = 3)

## Prop Feasible by Sp ####
num_pres = allinv %>%
  pivot_longer(c(1:6), names_to = "species", values_to = "PA") %>%
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
