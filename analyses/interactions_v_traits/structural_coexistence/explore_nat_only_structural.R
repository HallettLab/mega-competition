
# Set up Env ####
library(tidyverse)

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

fig_loc = "analyses/interactions_v_traits/structural_coexistence/prelim_figs/"
theme_set(theme_classic())

# Read in data ####
natcommD = read.csv("analyses/interactions_v_traits/structural_coexistence/nat_only_D_structural_results_20240729.csv")

# Clean data ####
natcommD_vis = natcommD %>%
  filter(!is.na(GITR))%>%
  mutate(comp = paste0(GITR, LENI, MICA, PLER, ACAM, AMME, PLNO, TWIL),
         treatment = "D")

## Explore missing data ####
ggplot(natcommD_vis, aes(x=feasibility)) +
  geom_bar()
## 9400 NAs for feasibility, N diff, and Fitness diff

nrow(natcommD_vis[natcommD_vis$feasibility == 1 & !is.na(natcommD_vis$feasibility),])
## 163 feasible comm 
nrow(natcommD_vis[!is.na(natcommD_vis$feasibility),])
## 4600 rows that are not NAs for feasibility, etc. 

natcommD_filt = natcommD_vis %>%
  filter(!is.na(feasibility))
## all sp present in these comms

unique(natcommD_filt$comp)
## 23 unique communities; 23x200 = 4600; calculations for these communities worked every time & the ones that did not work didn't work any of the 200 times

# Explore Raw Outputs ####
## niche diffs
ggplot(natcommD_filt, aes(x=niche_diff)) +
  geom_histogram()

## fitness diffs
ggplot(natcommD_filt, aes(x=fitness_diff)) +
  geom_histogram()

## by composition & legume presence
ggplot(natcommD_filt, aes(x=niche_diff, fill = as.factor(ACAM))) +
  geom_histogram() +
  facet_wrap(~comp, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed")

## by composition
ggplot(natcommD_filt, aes(x=niche_diff, color = as.factor(comp))) +
  geom_density()   

# Summarise ####
## Prop Feasible ####
prop_feas = natcommD_filt %>%
  group_by(comp) %>%
  summarise(num_feas = sum(feasibility),
            prop_feasible = num_feas/n(),
            mean_niche = mean(niche_diff),
            mean_fitness = mean(fitness_diff),
            se_niche = calcSE(niche_diff),
            se_fitness = calcSE(fitness_diff)) %>%
  mutate(w_legume = ifelse(substr(comp, start = 5, stop = 5) == 1, 1, 
                           ifelse(substr(comp, start = 9, stop = 9) == 1, 1, 0)),
         w_ACAM = ifelse(substr(comp, start = 5, stop = 5) == 1, 1, 0),
         w_TWIL = ifelse(substr(comp, start = 9, stop = 9) == 1, 1, 0)) %>%
  filter(!is.na(num_feas))

ggplot(prop_feas, aes(x=as.factor(comp), y=prop_feasible)) +
  geom_bar(stat = 'identity') +
  ggtitle("Native only 4sp Comm, D") +
  xlab("Composition") +
  ylab("Prop Feasible Comm (200 draws)")
#ggsave(paste0(fig_loc, "nat_only_C_4spcomm.png"), width = 10, height = 3)

ggplot(prop_feas, aes(x=as.factor(comp), y=prop_feasible, fill = as.factor(w_legume))) +
  geom_bar(stat = 'identity') +
  ggtitle("Native only 4sp Comm, D") +
  xlab("Composition") +
  ylab("Prop Feasible Comm (200 draws)") +
  scale_fill_manual(values = c("#A5AA99", "#24796C"))
#ggsave(paste0(fig_loc, "nat_only_C_4spcomm_legume.png"), width = 10, height = 3)

ggplot(prop_feas, aes(x=as.factor(comp), y=prop_feasible, fill = as.factor(w_ACAM))) +
  geom_bar(stat = 'identity') +
  ggtitle("Native only 4sp Comm, C") +
  xlab("Composition") +
  ylab("Prop Feasible Comm (200 draws)") +
  scale_fill_manual(values = c("#A5AA99", "#24796C"))
#ggsave(paste0(fig_loc, "nat_only_C_4spcomm_ACAM.png"), width = 10, height = 3)

ggplot(prop_feas, aes(x=as.factor(comp), y=prop_feasible, fill = as.factor(w_TWIL))) +
  geom_bar(stat = 'identity') +
  ggtitle("Native only 4sp Comm, C") +
  xlab("Composition") +
  ylab("Prop Feasible Comm (200 draws)") +
  scale_fill_manual(values = c("#A5AA99", "#24796C"))
#ggsave(paste0(fig_loc, "nat_only_C_4spcomm_TWIL.png"), width = 10, height = 3)

#E58606,#5D69B1,#52BCA3,#99C945,#CC61B0,#24796C,#DAA51B,#2F8AC4,#764E9F,#ED645A,#CC3A8E,#A5AA99

ggplot(prop_feas, aes(x=mean_niche, y=mean_fitness, color = as.factor(w_legume))) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_fitness - se_fitness, ymax = mean_fitness + se_fitness), width = 0.05)+
  geom_errorbarh(aes(xmax = mean_niche + se_niche, xmin = mean_niche - se_niche), height = 1)
#ggsave(paste0(fig_loc, "nat_only_D_niche_fitness_diffs_scatter.png"), width = 6, height = 3)


## Prop Feasible by Sp ####
num_pres = natcommD_filt %>%
  pivot_longer(c(2:9), names_to = "species", values_to = "PA") %>%
  select(-X) %>%
  filter(PA != 0) %>%
  group_by(species, comp) %>%
  summarise(num_feas = sum(feasibility)) %>%
  mutate(feas_PA = ifelse(num_feas > 0, 1, 0)) %>%
  ungroup() %>%
  group_by(species) %>%
  summarise(num_feas_present = sum(feas_PA))
  
ggplot(num_pres, aes(x=species, y=num_feas_present)) +
  geom_bar(stat = 'identity')

