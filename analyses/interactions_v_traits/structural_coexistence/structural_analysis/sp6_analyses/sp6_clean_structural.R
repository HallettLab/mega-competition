# Set up 
library(tidyverse)

file_path = "analyses/interactions_v_traits/structural_coexistence/"

## Read in data files
sp6_1 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp6/6_sp_structural_results_20240828.csv"))

sp6_2 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp6/6_sp_structural_results_part2_20240910.csv"))

sp6_3 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp6/6_sp_structural_results_part3_20240911.csv"))

sp6_4 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp6/6_sp_structural_results_part4_20240911.csv"))

sp6_5 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp6/6_sp_structural_results_part5_20240916.csv"))

sp6_6 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp6/6_sp_structural_results_part6_20240917.csv"))

sp6_7 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp6/6_sp_structural_results_part7_20240918.csv"))

sp6_8 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp6/6_sp_structural_results_part8_20240918.csv"))

## Combine
sp6_all = rbind(sp6_1, sp6_2, sp6_3, sp6_4, sp6_5, sp6_6, sp6_7, sp6_8)

rm(sp6_1, sp6_2, sp6_3, sp6_4, sp6_5, sp6_6, sp6_7, sp6_8)

## check row number
8008*2*100
## should have 1601600 rows, have 1601661 - some extras...

## make community variable
sp6_all = sp6_all %>%
  mutate(comp = paste0(ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL))

## check number of communities
comm_num = unique(sp6_all$comp)
## 8008, good
rm(comm_num)

## check non-NA runs
sp6_clean = sp6_all %>%
  filter(!is.na(feasibility))

## look for completion
check_complete = sp6_clean %>%
  group_by(comp, rainfall, ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL) %>%
  summarise(num_iter = n())

unique(check_complete$num_iter)
## seems like some doubles / partial doubles and two that were barely started - these must have been the communities that led to trouble during calculations

## probably need to re-run these particular communities to make sure there are the correct number of iterations

rm(check_complete, sp8_all)