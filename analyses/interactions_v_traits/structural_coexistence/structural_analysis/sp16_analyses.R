## 16 species analyses
## explore how rainfall, origin, functional diversity, indirect interactions, and network metrics influence coexistence, niche differences, and fitness differences

# Set up ####
## load packages
library(ggpubr)
library(tidyverse)

## set file paths
file_path = "analyses/interactions_v_traits/structural_coexistence/"

fig_loc = "analyses/interactions_v_traits/structural_coexistence/prelim_figs/sept_2024/sp16/"

## read in data
### coexistence metrics/indirect interactions
sp16 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp16/16_sp_structural_results_20240916.csv"))

### functional div
#fdiv15 = read.csv(paste0(file_path, "calc_comm_attributes/fdiv/sp15_fdiv.csv")) %>%
 # select(-X)

### community weighted trait means
#cwm15 = read.csv(paste0(file_path, "calc_comm_attributes/cwm/sp15_cwm.csv"))

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
#traitdat = left_join(cwm15, fdiv15, by = c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL", "richness"))

## join struct + trait dat
#sp15trait = left_join(sp15, traitdat, by = c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL"))# %>%
#filter(!is.na(feasibility),
# niche_diff != '#NAME?')
## 4 rows have values '#NAME?'; need to go back and figure out what these values actually are. Probably need to run the code again for these specific instances to see if it matters...

#sp15trait$niche_diff = as.numeric(sp15trait$niche_diff)

sp16sum = sp16 %>%
  group_by(rainfall,
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
            se_cpd = calcSE(comm_pair_diff))  #%>%
  #mutate(num.inv = sum(ANAR, BRHO, BRNI, CESO, LOMU, TACA, THIR))

# Explore missing dat####
sp16NA = sp16sum %>%
  group_by(rainfall, prop_feasible) %>%
  summarise(num = n())

## no structural metrics could be calculated
