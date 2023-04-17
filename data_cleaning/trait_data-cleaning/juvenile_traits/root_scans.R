## Set up Env
library(tidyverse)
library(openxlsx)
library(readxl)

# read in data ####
## file path
lead <- "/Users/carme/Dropbox (University of Oregon)/Greenhouse_Traits/Data/"

root_scans1 <- read.xlsx(paste0(lead, "Raw_Data/Root Scans/Roots_1/Root_data.xlsx"), sheet = 1)
root_scans2 <- read.xlsx(paste0(lead, "Raw_Data/Root Scans/Roots_2/Root_data_2.xlsx"), sheet = 1)
lina_root_scans <- read.xlsx(paste0(lead, "Raw_Data/Root Scans/Roots_2/Lina_roots.xlsx"), sheet = 1)



test <- root_scans1[-(1:4),] %>%
  select(1:31)

colnames(test)

str(test)

unique(test$`AnalysedRegionArea(cm2)`)
unique(test$`AnalysedRegionHeight(cm)`)
unique(test$`AnalysedRegionWidth(cm)`)
## these columns seem like they have to do with tray size rather than a measuremnet of interest about the roots

unique(test$`SoilVol(m3)`)
## only 1, not an informative column

unique(test$Left.Top.Right.Bottom.NExclusions)
## don't know what this column is

unique(test$`Length(cm)`)



