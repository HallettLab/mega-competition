
# Set up Env ####
library(tidyverse)

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

fig_loc = "analyses/interactions_v_traits/structural_coexistence/prelim_figs/"
theme_set(theme_classic())

# Read in data ####
natcommD = read.csv("analyses/interactions_v_traits/structural_coexistence/run_structural/nat_only_D_structural_results_20240729.csv")

natcommC = read.csv("analyses/interactions_v_traits/structural_coexistence/run_structural/nat_only_C_structural_results_20240729.csv")

# Clean data ####
natcommD_vis = natcommD %>%
  filter(!is.na(GITR))%>%
  mutate(comp = paste0(ACAM, AMME, GITR, LENI, MAEL, MICA, PLER, PLNO, TWIL),
         treatment = "D")

natcommC_vis = natcommC %>%
  filter(!is.na(GITR))%>%
  mutate(comp = paste0(ACAM, AMME, GITR, LENI, MAEL, MICA, PLER, PLNO, TWIL),
         treatment = "C")

## join together
allnat = rbind(natcommC_vis, natcommD_vis) %>%
  select(-X)

## Explore missing data ####
ggplot(allnat, aes(x=feasibility)) +
  geom_bar()
## 26400 NAs for feasibility, N diff, and Fitness diff
26400/200 ## 132 NAs out of 252
132/252

allnat_filt = allnat %>%
  filter(!is.na(feasibility))
## all sp present in these comms

unique(allnat_filt$comp)
## 23 unique communities; 23x200 = 4600; calculations for these communities worked every time & the ones that did not work didn't work any of the 200 times

# Summarise ####
## Prop Feasible ####
prop_feas = allnat %>%
  group_by(comp, treatment) %>%
  filter(!is.na(feasibility)) %>%
  summarise(num_feas = sum(feasibility),
            prop_feasible = num_feas/n(),
            mean_niche = mean(niche_diff),
            mean_fitness = mean(fitness_diff),
            se_niche = calcSE(niche_diff),
            se_fitness = calcSE(fitness_diff)) %>%
  mutate(w_legume = ifelse(substr(comp, start = 1, stop = 1) == 1, 1, 
                           ifelse(substr(comp, start = 9, stop = 9) == 1, 1, 0)))

ggplot(prop_feas, aes(x=as.factor(comp), y=prop_feasible)) +
  geom_bar(stat = 'identity') +
  ggtitle("Native only 4sp Comm") +
  xlab("Composition") +
  ylab("Prop Feasible Comm (200 draws)") +
  facet_wrap(~treatment, ncol = 1, nrow = 2)

ggplot(prop_feas, aes(x=as.factor(comp), y=prop_feasible, fill = as.factor(w_legume))) +
  geom_bar(stat = 'identity') +
  ggtitle("Native only 4sp Comm") +
  xlab("Composition") +
  ylab("Prop Feasible Comm (200 draws)") +
  scale_fill_manual(values = c("#A5AA99", "#24796C")) +
  facet_wrap(~treatment, ncol = 1, nrow = 2)
ggsave(paste0(fig_loc, "nat_only_4spcomm_legume.png"), width = 10, height = 6)

ggplot(prop_feas, aes(x=mean_niche, y=mean_fitness, color = treatment)) +
  geom_point(aes(size = prop_feasible)) +
  geom_errorbar(aes(ymin = mean_fitness - se_fitness, ymax = mean_fitness + se_fitness), width = 0.05)+
  geom_errorbarh(aes(xmax = mean_niche + se_niche, xmin = mean_niche - se_niche), height = 1)
#ggsave(paste0(fig_loc, "nat_only_D_niche_fitness_diffs_scatter.png"), width = 6, height = 3)

## Prop Feasible by Sp ####
num_pres = allnat %>%
  pivot_longer(c(1:9), names_to = "species", values_to = "PA") %>%
  filter(PA != 0) %>%
  group_by(species, comp, treatment) %>%
  summarise(num_feas = sum(feasibility, na.rm = T)) %>%
  mutate(feas_PA = ifelse(num_feas > 0, 1, 0)) %>%
  ungroup() %>%
  group_by(species, treatment) %>%
  summarise(num_feas_present = sum(feas_PA)) %>%
  mutate(legume = ifelse(species %in% c("ACAM", "TWIL"), 1, 0))
  
ggplot(num_pres, aes(x=species, y=num_feas_present)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~treatment) +
  ylab("Number of Feasible Communities where Present") +
  xlab("Species") +
  scale_fill_manual(values = c("#A5AA99", "#24796C"))

ggsave(paste0(fig_loc, "nat_num_comm_present.png"), width = 10, height = 4)

