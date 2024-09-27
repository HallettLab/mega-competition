# Set up 
library(tidyverse)

dat_path = "analyses/interactions_v_traits/structural_coexistence/run_structural/structural_results_files/sp10/"

## Read in data files
sp10_1 = read.csv(paste0(dat_path, "10_sp_structural_results_20240829.csv"))

sp10_2 = read.csv(paste0(dat_path, "10_sp_structural_results_part2_20240916.csv"))

sp10_3 = read.csv(paste0(dat_path, "10_sp_structural_results_part3_20240916.csv"))

sp10_4 = read.csv(paste0(dat_path, "10_sp_structural_results_part4_20240918.csv"))

sp10_5 = read.csv(paste0(dat_path, "10_sp_structural_results_part5_20240918.csv"))

## Combine
sp10_all = rbind(sp10_1, sp10_2, sp10_3, sp10_4, sp10_5)

rm(sp10_1, sp10_2, sp10_3, sp10_4, sp10_5)

## check row number
8008*2*100
## should have 1601600 rows, have 1601658 - some extras...

## make community variable
sp10_all = sp10_all %>%
  mutate(comp = paste0(ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL))

## check number of communities
comm_num = unique(sp10_all$comp)
## 8008, good
rm(comm_num)

## check non-NA runs
sp10_clean = sp10_all %>%
  filter(!is.na(feasibility))

## look for completion
check_complete = sp10_clean %>%
  group_by(comp, rainfall, ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL) %>%
  summarise(num_iter = n())

unique(check_complete$num_iter)
## two comms have over 100 iterations

## probably need to re-run these particular communities to make sure there are the correct number of iterations

rm(check_complete, sp10_all)
