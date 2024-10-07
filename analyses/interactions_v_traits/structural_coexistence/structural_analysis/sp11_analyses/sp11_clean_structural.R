# Set up 
library(tidyverse)

file_path = "analyses/interactions_v_traits/structural_coexistence/"

## Read in data files
sp11_1 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp11/11_sp_structural_results_part1_20240929.csv"))

sp11_2 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp11/11_sp_structural_results_part2_20240929.csv"))

sp11_3 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp11/11_sp_structural_results_part3_20240929.csv"))

sp11_4 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp11/11_sp_structural_results_part4_20240929.csv"))

sp11_5 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp11/11_sp_structural_results_part5_20240930.csv"))

sp11_6 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp11/11_sp_structural_results_part6_20240930.csv"))

sp11_7 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp11/11_sp_structural_results_part7_20240930.csv"))

sp11_8 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp11/11_sp_structural_results_part8_20241002.csv"))

sp11_9 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp11/11_sp_structural_results_part9_20241002.csv"))

sp11_10 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp11/11_sp_structural_results_part10_20241002.csv"))


## Combine
sp11_all = rbind(sp11_1, sp11_2, sp11_3, sp11_4, sp11_5, sp11_6, sp11_7, sp11_8, sp11_9, sp11_10)

rm(sp11_1, sp11_2, sp11_3, sp11_4, sp11_5, sp11_6, sp11_7, sp11_8, sp11_9, sp11_10)

## check row number
4368*2*100
## should have 873600 rows; only have 873483; missing some

## make community variable
sp11_all = sp11_all %>%
  mutate(comp = paste0(ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL))

## check number of communities
comm_num = unique(sp11_all$comp)
## 11440, good
rm(comm_num)

## check non-NA runs
sp11_clean = sp11_all %>%
  filter(!is.na(feasibility))

# Explore missing dat####
sp11NA = sp11_all %>%
  filter(is.na(feasibility)) %>%
  group_by(comp, rainfall) %>%
  summarise(num = n()) %>%
  ungroup() %>%
  group_by(rainfall) %>%
  summarise(numcomms = n())

## look for completion
check_complete = sp11_clean %>%
  group_by(comp, rainfall, ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL) %>%
  summarise(num_iter = n())

unique(check_complete$num_iter) ## one community has 40, one other has 140

rm(check_complete, sp11_all, sp11NA)
