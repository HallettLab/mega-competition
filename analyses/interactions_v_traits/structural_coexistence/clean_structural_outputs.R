## Clean & Combine structural coexistence outputs

# Set up ####
library(tidyverse)

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

# Read in data ####
## native ####
natcommD = read.csv("analyses/interactions_v_traits/structural_coexistence/run_structural/nat_only_D_structural_results_20240729.csv")

natcommC = read.csv("analyses/interactions_v_traits/structural_coexistence/run_structural/nat_only_C_structural_results_20240729.csv")

## invasive ####
invcommD = read.csv("analyses/interactions_v_traits/structural_coexistence/run_structural/inv_only_D_structural_results_20240730.csv")

invcommC = read.csv("analyses/interactions_v_traits/structural_coexistence/run_structural/inv_only_C_structural_results_20240730.csv")

## mixed ####

mixcommC = read.csv("analyses/interactions_v_traits/structural_coexistence/run_structural/mix_C_structural_results_20240801.csv")

mixcommD = read.csv("analyses/interactions_v_traits/structural_coexistence/run_structural/mix_D_structural_results_20240812.csv")

# Clean data ####
## native ####
natcommD_vis = natcommD %>%
  filter(!is.na(GITR), MAEL == 0)%>%
  mutate(comp = paste0(ACAM, AMME, GITR, LENI, MAEL, MICA, PLER, PLNO, TWIL),
         treatment = "D")

natcommC_vis = natcommC %>%
  filter(!is.na(GITR), MAEL == 0)%>%
  mutate(comp = paste0(ACAM, AMME, GITR, LENI, MAEL, MICA, PLER, PLNO, TWIL),
         treatment = "C")

## join together
allnat = rbind(natcommC_vis, natcommD_vis) %>%
  select(-X)

## invasive ####
invcommD_vis = invcommD %>%
  filter(!is.na(ANAR))%>%
  mutate(comp = paste0(ANAR, BRHO, BRNI, CESO, LOMU, TACA, THIR),
         treatment = "D")

invcommC_vis = invcommC %>%
  filter(!is.na(BRHO))%>%
  mutate(comp = paste0(ANAR, BRHO, BRNI, CESO, LOMU, TACA, THIR),
         treatment = "C")

## join together
allinv = rbind(invcommC_vis, invcommD_vis) %>%
  select(-X)

## mixed ####
mixcommC_vis = mixcommC %>%
  filter(!is.na(GITR))%>%
  mutate(comp = paste0(ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL),
         treatment = "C")

mixcommD_vis = mixcommD %>%
  filter(!is.na(GITR))%>%
  mutate(comp = paste0(ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL),
         treatment = "D")

## join together
allmix = rbind(mixcommC_vis, mixcommD_vis) %>%
  select(-X)

# Clean Env ####
rm(invcommC, invcommC_vis, invcommD, invcommD_vis, natcommC, natcommC_vis, natcommD, natcommD_vis, mixcommC, mixcommD, mixcommC_vis, mixcommD_vis)
