# Set up 
library(tidyverse)

file_path = "analyses/interactions_v_traits/structural_coexistence/"

## Read in data files
sp9_1 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp9/9_sp_structural_results_part1_20240923.csv"))

sp9_2 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp9/9_sp_structural_results_part2_20240923.csv"))

sp9_3 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp9/9_sp_structural_results_part3_20240923.csv"))

sp9_4 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp9/9_sp_structural_results_part4_20240923.csv"))

sp9_5 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp9/9_sp_structural_results_part5_20240923.csv"))

sp9_6 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp9/9_sp_structural_results_part6_20240925.csv"))

sp9_7 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp9/9_sp_structural_results_part7_20240925.csv"))

sp9_8 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp9/9_sp_structural_results_part8_20240927.csv"))

sp9_9 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp9/9_sp_structural_results_part9_20240927.csv"))

sp9_10 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp9/9_sp_structural_results_part10_20240929.csv"))

sp9_11 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp9/9_sp_structural_results_part11_20240929.csv"))

sp9_12 = read.csv(paste0(file_path, "run_structural/structural_results_files/sp9/9_sp_structural_results_part12_20240929.csv"))


## Combine
sp9_all = rbind(sp9_1, sp9_2, sp9_3, sp9_4, sp9_5, sp9_6, sp9_7, sp9_8, sp9_9, sp9_10, sp9_11, sp9_12)

rm(sp9_1, sp9_2, sp9_3, sp9_4, sp9_5, sp9_6, sp9_7, sp9_8, sp9_9, sp9_10, sp9_11, sp9_12)

## check row number
11440*2*100
## should have 2288000 rows; yep this matches

## make community variable
sp9_all = sp9_all %>%
  mutate(comp = paste0(ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL))

## check number of communities
comm_num = unique(sp9_all$comp)
## 11440, good
rm(comm_num)

## check non-NA runs
sp9_clean = sp9_all %>%
  filter(!is.na(feasibility))

# Explore missing dat####
sp9NA = sp9_all %>%
  filter(is.na(feasibility)) %>%
  group_by(comp, rainfall) %>%
  summarise(num = n()) %>%
  ungroup() %>%
  group_by(rainfall) %>%
  summarise(numcomms = n())

## look for completion
check_complete = sp9_clean %>%
  group_by(comp, rainfall, ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL) %>%
  summarise(num_iter = n())

unique(check_complete$num_iter)
## all good

rm(check_complete, sp9_all, sp9NA)
