
# Set up ####
library(tidyverse)


## Read in Data ####
sp4 = read.csv("analyses/interactions_v_traits/structural_coexistence/run_structural/4_sp_structural_results_20240828.csv")

sp12 = read.csv("analyses/interactions_v_traits/structural_coexistence/run_structural/12_sp_structural_results_20240829.csv")


## this should be the correct number of rows!

# 4 Species ####
## extract column names of data frame (this is actually the first row of data)
row1.4 = colnames(sp4)

## rename columns
names(sp4) = c("feasibility", "niche_diff", "fitness_diff", "niche_diff_cpd", "omega_all", "comm_pair_overlap", "comm_pair_diff", "ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL", "rainfall", "draw", "iteration_num")

## change formatting of the first row of data
row1.4
row1.4.correct = data.frame(feasibility = 0, niche_diff = 2.25496903412073, fitness_diff = 46.534126101691, niche_diff_cpd = 0.0011, omega_all = 0.0001, comm_pair_overlap = 0, comm_pair_diff = 0.001, 
                 ACAM = 0, AMME = 0, ANAR = 1, BRHO = 1, BRNI = 0, CESO = 1, GITR = 1, LENI = 0, LOMU = 0, MAEL = 0, MICA = 0, PLER = 0, PLNO = 0, TACA = 0, THIR = 0, TWIL = 0, rainfall = "C", draw = 491, iteration_num = 0)

## add row 1 back into the dataframe
sp4_clean = rbind(row1.4.correct, sp4) %>%
  mutate(comp = paste0(ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL))


# 12 Species ####
## extract column names of data frame (this is actually the first row of data)
row1.12 = colnames(sp12)

## rename columns
names(sp12) = c("feasibility", "niche_diff", "fitness_diff", "niche_diff_cpd", "omega_all", "comm_pair_overlap", "comm_pair_diff", "ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL", "rainfall", "draw", "iteration_num")

## change formatting of the first row of data
row1.12
row1.12.correct = data.frame(feasibility = NA, niche_diff = NA, fitness_diff = NA, niche_diff_cpd = NA, omega_all = NA, comm_pair_overlap = NA, comm_pair_diff = NA, 
                          ACAM = 1, AMME = 1, ANAR = 1, BRHO = 1, BRNI = 1, CESO = 1, GITR = 1, LENI = 1, LOMU = 0, MAEL = 0, MICA = 1, PLER = 1, PLNO = 0, TACA = 1, THIR = 1, TWIL = 0, rainfall = "C", draw = 491, iteration_num = 0)

## add row 1 back into the dataframe
sp12_clean = rbind(row1.12.correct, sp12) %>%
  mutate(comp = paste0(ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL))

unique(sp12_clean$feasibility)

rm(sp4, sp12, row1.4.correct, row1.12.correct)
