# Set up ####
## load packages
library(ggpubr)

## set file paths
file_path = "analyses/interactions_v_traits/structural_coexistence/"

## read in data
### coexistence metrics/indirect interactions
source(paste0(file_path, "structural_analysis/sp6_analyses/sp6_clean_structural.R"))

### functional div
fdiv6 = read.csv(paste0(file_path, "calc_comm_attributes/fdiv/sp6_fdiv.csv")) %>%
  select(-X, -comp)

### community weighted trait means
cwm6 = read.csv(paste0(file_path, "calc_comm_attributes/cwm/sp6_cwm.csv"))

### network metrics

## set plot theme
theme_set(theme_classic())

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

# Format Data ####
## join fdiv + cwm
traitdat = left_join(cwm6, fdiv6, by = c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL", "richness"))

sp6trait = left_join(sp6_clean, traitdat, by = c("ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL"))

sp6trait$niche_diff = as.numeric(sp6trait$niche_diff)

sp6sum = sp6trait %>%
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
  mutate(num.inv = sum(ANAR, BRHO, BRNI, CESO, LOMU, TACA, THIR),
         num.nat = sum(ACAM, AMME, GITR, LENI, MAEL, MICA, PLER, PLNO, TWIL),
         num.legume = sum(ACAM, THIR, TWIL),
         origin = ifelse(num.inv == 0, "Native",
                         ifelse(num.inv == 6, "Invasive", "Mixed")))
