## the purpose of this script is to create a key of unique.ID to treatment, block, plot, sub info for easy storage and use later on.

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


date_collections <- 20230407

collections.fk <- read.xlsx(paste0(lead, "Collections/Collections_merged/", date_collections, "_MASTER_Collections.xlsx"), sheet = 2)

## make sure the dates read in correctly
collections.fk$phyto.date.collect <- as.Date(collections.fk$phyto.date.collect, origin = "1899-12-30")
collections.fk$phyto.date.census <- as.Date(collections.fk$phyto.date.census, origin = "1899-12-30")
collections.fk$bg.date.census <- as.Date(collections.fk$bg.date.census, origin = "1899-12-30")
collections.fk$phyto.unique <- as.character(collections.fk$phyto.unique)

# Make Modifications ####
collections.fkC <- collections.fk %>%
  mutate(across(where(is.character), str_trim)) %>% ## remove leading & trailing whitespace!!
  filter(plot < 43, bkgrd != "VIVI") %>% ## take out trifolium sub-exp & unsampled VIVI backgrounds
  mutate(phyto.unique = toupper(phyto.unique), ## capitalize all values
         unique.ID = unique) %>% ## standardize column name
  mutate_all(na_if,"") ## make blank values NAs

drought <- c(1, 3, 4, 6, 12, 14) ## create a treatment vector

## create a unique-ID key
unique.key <- collections.fkC %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C")) %>% ## add a treatment column
  mutate(phyto = ifelse(phyto == "THIR-I", "THIR", phyto),
         phyto = ifelse(phyto == "TWIL-I", "TWIL", phyto), 
         bkgrd = ifelse(bkgrd == "THIR-I", "THIR", bkgrd),
         bkgrd = ifelse(bkgrd == "TWIL-I", "TWIL", bkgrd)) %>%
  select(unique.ID, treatment, block, plot, sub, bkgrd, dens, phyto, phyto.unique) ## retain only necessary columns


## clean up env
rm(list = c("date_collections", "collections.fkC", "collections.fk"))
