## Set up Env
library(tidyverse)
library(openxlsx)

# read in data ####
## file path
lead <- "/Users/carme/Dropbox (University of Oregon)/Greenhouse_Traits/Data/"

root_scans1 <- read.xlsx(paste0(lead, "Raw_Data/Root Scans/Roots_1/Root_data.xlsx"), sheet = 1) 
root_scans2 <- read.xlsx(paste0(lead, "Raw_Data/Root Scans/Roots_2/Root_data_2.xlsx"), sheet = 1)
lina_root_scans <- read.xlsx(paste0(lead, "Raw_Data/Root Scans/Roots_2/Lina_roots.xlsx"), sheet = 1)

