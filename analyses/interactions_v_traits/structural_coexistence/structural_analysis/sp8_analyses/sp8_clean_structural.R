# Set up 
library(tidyverse)

file_path = "analyses/interactions_v_traits/structural_coexistence/"

## Read in data files
sp8 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp8/8_sp_structural_results_20240828.csv"))

sp8_1 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp8/8_sp_structural_results_part1_20240910.csv"))

sp8_2 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp8/8_sp_structural_results_part2_20240910.csv"))

sp8_3 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp8/8_sp_structural_results_part3_20240910.csv"))

sp8_4 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp8/8_sp_structural_results_part4_20240910.csv"))

sp8_5 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp8/8_sp_structural_results_part5_20240910.csv"))

sp8_6 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp8/8_sp_structural_results_part6_20240910.csv"))


## Combine
sp8_all = rbind(sp8, sp8_1, sp8_2, sp8_3, sp8_4, sp8_5, sp8_6)

rm(sp8, sp8_1, sp8_2, sp8_3, sp8_4, sp8_5, sp8_6)

## check row number
12870*2*100
## should have 2574000 rows, have 2573997

## make community variable
sp8_all = sp8_all %>%
  mutate(comp = paste0(ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL))

## check number of communities
comm_num = unique(sp8_all$comm)
## 12870, good
rm(comm_num)

## check non-NA runs
sp8_clean = sp8_all %>%
  filter(!is.na(feasibility))

## look for completion
check_complete = no_NAs %>%
  group_by(comp, rainfall, ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL) %>%
  summarise(num_iter = n())

unique(check_complete$num_iter)
## there is one community that has only 97 iterations

## probably need to re-run this particular community to make sure there are the correct number of iterations
## 0011001110010101 D

rm(check_complete, sp8_all)