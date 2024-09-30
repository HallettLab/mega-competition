# Set up ####
library(tidyverse)

file_path = "analyses/interactions_v_traits/structural_coexistence/"

## Read in Data ####
sp7_1 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp7/7_sp_structural_results_part1_20240917.csv"))

sp7_2 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp7/7_sp_structural_results_part2_20240918.csv"))

sp7_3 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp7/7_sp_structural_results_part3_20240918.csv"))

sp7_4 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp7/7_sp_structural_results_part4_20240919.csv"))

sp7_5 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp7/7_sp_structural_results_part5_20240919.csv"))

sp7_6 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp7/7_sp_structural_results_part6_20240920.csv"))

sp7_7_1 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp7/7_sp_structural_results_part7_1_20240920.csv"))

sp7_7_2 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp7/7_sp_structural_results_part7_2_20240920.csv"))

sp7_8 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp7/7_sp_structural_results_part8_20240920.csv"))

sp7_9 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp7/7_sp_structural_results_part9_20240920.csv"))

sp7_10 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp7/7_sp_structural_results_part10_20240923.csv"))

sp7_11 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp7/7_sp_structural_results_part11_20240923.csv"))

sp7_12 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp7/7_sp_structural_results_part12_20240923.csv"))

## Combine ####
sp7_all = rbind(sp7_1, sp7_2, sp7_3, sp7_4, sp7_5, sp7_6, sp7_7_1, sp7_7_2, sp7_8, sp7_9, sp7_10, sp7_11, sp7_12)

rm(sp7_1, sp7_2, sp7_3, sp7_4, sp7_5, sp7_6, sp7_7_1, sp7_7_2, sp7_8, sp7_9, sp7_10, sp7_11, sp7_12)

# Prep Data ####
## check row number
11440*2*100
## should have 2288000 rows, have 2287634; missing some

## make community variable
sp7_all = sp7_all %>%
  mutate(comp = paste0(ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL))

## check number of communities
comm_num = unique(sp7_all$comp)
## 11439, missing 1
rm(comm_num)

## check non-NA runs
sp7_clean = sp7_all %>%
  filter(!is.na(feasibility))

# Explore missing dat####
sp7NA = sp7_all %>%
  filter(is.na(feasibility)) %>%
  group_by(comp, rainfall) %>%
  summarise(num = n()) %>%
  ungroup() %>%
  group_by(rainfall) %>%
  summarise(numcomms = n())

## look for completion
check_complete = sp7_clean %>%
  group_by(comp, rainfall, ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL) %>%
  summarise(num_iter = n())

unique(check_complete$num_iter)
## seems like some partials - these must have been the communities that led to trouble during calculations

## probably need to re-run these particular communities to make sure there are the correct number of iterations

rm(check_complete, sp7_all, sp7NA)
