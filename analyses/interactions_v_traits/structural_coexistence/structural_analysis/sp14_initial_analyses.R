## 12 species analyses
## explore how rainfall, origin, functional diversity, indirect interactions, and network metrics influence coexistence, niche differences, and fitness differences

# Set up ####
## load packages
library(ggpubr)
library(tidyverse)

## set file paths
file_path = "analyses/interactions_v_traits/structural_coexistence/"

fig_loc = "analyses/interactions_v_traits/structural_coexistence/prelim_figs/sept_2024/sp14/"

## read in data
### coexistence metrics/indirect interactions
sp14 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp14/14_sp_structural_results_20240917.csv"))

### functional div
fdiv14 = read.csv(paste0(file_path, "calc_comm_attributes/fdiv/sp14_fdiv.csv")) %>%
  select(-X)

### community weighted trait means
cwm14 = read.csv(paste0(file_path, "calc_comm_attributes/cwm/sp14_cwm.csv"))

### network metrics

## set plot theme
theme_set(theme_classic())

## create standard error function
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

# Format Data ####
## join fdiv + cwm
traitdat = left_join(cwm14, fdiv14, by = c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL", "richness"))

## join struct + trait dat
sp14trait = left_join(sp14, traitdat, by = c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL"))# %>%
#filter(!is.na(feasibility),
# niche_diff != '#NAME?')
## 4 rows have values '#NAME?'; need to go back and figure out what these values actually are. Probably need to run the code again for these specific instances to see if it matters...

sp14trait$niche_diff = as.numeric(sp14trait$niche_diff)

sp14sum = sp14trait %>%
  group_by(comp, rainfall, fdiv, cwm.height, cwm.ldmc, cwm.sla, cwm.rmf, cwm.crsl, cwm.pf, cwm.d,
           ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL) %>%
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

# Explore missing dat####
sp14NA = sp14sum %>%
  group_by(rainfall, prop_feasible) %>%
  summarise(num = n())

## too many missing; only 3 communities could be calculated; not worth doing analyses on
