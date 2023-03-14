## Set up Env
library(tidyverse)
library(openxlsx)

# read in data ####
## file path
lead <- "/Users/carme/Dropbox (University of Oregon)/Greenhouse_Traits/Data/"

fall_leaf_scans <- read.xlsx(paste0(lead, "Raw_Data/Leaf Scans/September traits gh project/sept_leaf_area_image J.xlsx"), sheet = 1) %>%
  mutate(Notes = X6) %>%
  select(-X6)
## what are the mean, min, and max of leaf scans? Why are they only on the fall leaf scans?

spring_leaf_scans <- read.xlsx(paste0(lead, "Raw_Data/Leaf Scans/January traits gh project/jan_leaf_area_image J.xlsx"), sheet = 1) %>%
  mutate(Mean = NA, 
         Min = NA, 
         Max = NA, 
         Notes = NA)

leaf_scans_all <- rbind(fall_leaf_scans, spring_leaf_scans)


