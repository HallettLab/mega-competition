# Load packages ####
library(openxlsx)
library(tidyverse)

# Read in Data ####

# specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/"
  
} else {
  # Marina
  lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/"
} 


date_collections <- 20220927

collections <- read.xlsx(paste0(lead, "Collections/Collections_merged/", date_collections, "_MASTER_Collections_2-in-progress.xlsx"), sheet = 2)

## make sure the dates read in correctly
collections$phyto.date.collect <- as.Date(collections$phyto.date.collect, origin = "1899-12-30")
collections$phyto.date.census <- as.Date(collections$phyto.date.census, origin = "1899-12-30")
collections$bg.date.census <- as.Date(collections$bg.date.census, origin = "1899-12-30")
collections$phyto.unique <- as.character(collections$phyto.unique)

# Make Modifications ####
## all of the vectors to eventually filter by
summer_phytos <- c("ACAM", "ANAR", "AVBA", "BRHO", "GITR", "MICA", "LENI", "PLER", "THIR-I", "TWIL-I", "LOMU")
nhood10 <- c("MICA", "PLER", "BRHO", "ANAR", "GITR", "ACAM") ## neighborhood size
nhood18 <- c("LENI", "TWIL-I", "AVBA", "THIR-I") ## neighborhood size

#unique(test$phyto.unique)

collectionsC <- collections %>%
  mutate(across(where(is.character), str_trim)) %>% ## remove leading & trailing whitespace!!
  filter(plot < 43, bkgrd != "VIVI", phyto.n.indiv > 0) %>%
  mutate(phyto.unique = ifelse(phyto.unique == "a", "A", ## change all values to caps
                               ifelse(phyto.unique == "b","B", 
                                      ifelse(phyto.unique == "c", "C", 
                                             ifelse(phyto.unique == "", NA, phyto.unique))))) %>%
  mutate(unique.ID = unique) %>% ## standardize column name
  mutate(bkgrd.n.indiv = ifelse(bkgrd == "Control", NA, bkgrd.n.indiv)) %>% ## change # of background indiv in controls to NA
  mutate(Nbrhood.size = ifelse(phyto %in% nhood10, 10, ## fill in all vals of neighborhood size
                               ifelse(phyto %in% nhood18, 18, Nbrhood.size))) %>%
  filter(phyto %in% summer_phytos)
