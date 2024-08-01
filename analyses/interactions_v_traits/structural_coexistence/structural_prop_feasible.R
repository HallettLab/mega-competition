
# Set up Env ####
library(tidyverse)

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

fig_loc = "analyses/interactions_v_traits/structural_coexistence/prelim_figs/"
theme_set(theme_classic())

# Read in data ####
## native ####
natcommD = read.csv("analyses/interactions_v_traits/structural_coexistence/run_structural/nat_only_D_structural_results_20240729.csv")

natcommC = read.csv("analyses/interactions_v_traits/structural_coexistence/run_structural/nat_only_C_structural_results_20240729.csv")

## invasive ####
invcommD = read.csv("analyses/interactions_v_traits/structural_coexistence/run_structural/inv_only_D_structural_results_20240730.csv")

invcommC = read.csv("analyses/interactions_v_traits/structural_coexistence/run_structural/inv_only_C_structural_results_20240730.csv")

# Clean data ####
## native ####
natcommD_vis = natcommD %>%
  filter(!is.na(GITR), MAEL == 0)%>%
  mutate(comp = paste0(ACAM, AMME, GITR, LENI, MAEL, MICA, PLER, PLNO, TWIL),
         treatment = "D")

natcommC_vis = natcommC %>%
  filter(!is.na(GITR), MAEL == 0)%>%
  mutate(comp = paste0(ACAM, AMME, GITR, LENI, MAEL, MICA, PLER, PLNO, TWIL),
         treatment = "C")

## join together
allnat = rbind(natcommC_vis, natcommD_vis) %>%
  select(-X)

## invasive ####
invcommD_vis = invcommD %>%
  filter(!is.na(ANAR))%>%
  mutate(comp = paste0(ANAR, BRHO, BRNI, CESO, LOMU, TACA, THIR),
         treatment = "D")

invcommC_vis = invcommC %>%
  filter(!is.na(BRHO))%>%
  mutate(comp = paste0(ANAR, BRHO, BRNI, CESO, LOMU, TACA, THIR),
         treatment = "C")

## join together
allinv = rbind(invcommC_vis, invcommD_vis) %>%
  select(-X)

# Summarise ####
## Native ####
### Prop Feasible ####
natprop_feas = allnat %>%
  group_by(comp, treatment) %>%
  filter(!is.na(feasibility)) %>%
  summarise(num_feas = sum(feasibility),
            prop_feasible = num_feas/n(),
            mean_niche = mean(niche_diff),
            mean_fitness = mean(fitness_diff),
            se_niche = calcSE(niche_diff),
            se_fitness = calcSE(fitness_diff)) %>%
  mutate(w_legume = ifelse(substr(comp, start = 1, stop = 1) == 1, 1, 
                           ifelse(substr(comp, start = 9, stop = 9) == 1, 1, 0)),
         origin = "Native")

### Difference in Feasibility ####
diff_prop_feas = prop_feas %>% 
  select(treatment, prop_feasible, comp) %>%
  pivot_wider(names_from = "treatment", values_from = "prop_feasible") %>%
  mutate(feas_diff = D - C)

## Invasive ####
### Prop Feasible ####
invprop_feas = allinv %>%
  group_by(comp, treatment) %>%
  summarise(num_feas = sum(feasibility),
            prop_feasible = num_feas/n(),
            mean_niche = mean(niche_diff),
            mean_fitness = mean(fitness_diff),
            se_niche = calcSE(niche_diff),
            se_fitness = calcSE(fitness_diff)) %>%
  mutate(w_legume = ifelse(substr(comp, start = 7, stop = 7) == 1, 1, 0),
         origin = "Invasive")

### Difference in Feasibility ####
invdiff_prop_feas = invprop_feas %>% 
  select(treatment, prop_feasible, comp) %>%
  pivot_wider(names_from = "treatment", values_from = "prop_feasible") %>%
  mutate(feas_diff = D - C)

# Visualise ####
## prop feasible ####
## diff feasibility ####
## mean feasibility ####

allprop_feas = rbind(invprop_feas, natprop_feas) 

allprop_feas$origin = as.factor(allprop_feas$origin)

allprop_feas = allprop_feas %>%
  mutate(origin = fct_relevel(origin, "Native", "Invasive"))

ggplot(allprop_feas, aes(x=treatment, y=prop_feasible, color = origin)) +
  geom_boxplot() +
  geom_jitter() +
  ylab("Proportion of Feasible Communities") +
  scale_color_manual(values = c("#E58606","#5D69B1")) +
  xlab("Rainfall Treatment") +
  facet_wrap(~origin)
ggsave(paste0(fig_loc, "prop_feas_allcomm_rainfall.png"), width = 6, height = 3)




ggplot(diff_prop_feas, aes(x=feas_diff)) +
  geom_histogram(color = "black", fill = "lightgray", bins = 15) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Difference in Feasibility (D-C)")

sort(unique(diff_prop_feas$feas_diff))

ggplot(test, aes(x=comp)) +
  geom_bar(aes(y=C), stat= 'identity') +
  geom_bar(aes(y=D), stat = 'identity', fill = "red")

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

# Prop Feasible by Sp ####
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

