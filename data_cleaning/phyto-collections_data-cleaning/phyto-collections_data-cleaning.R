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
summer_phytos <- c("ACAM", "ANAR", "AVBA", "BRHO", "GITR", "MICA", "LENI", "PLER", "THIR-I", "TWIL-I", "LOMU", "TACA", "MAEL")
nhood10 <- c("MICA", "PLER", "BRHO", "ANAR", "GITR", "ACAM", "TACA") ## neighborhood size
nhood18 <- c("LENI", "TWIL-I", "AVBA", "THIR-I", "MAEL") ## neighborhood size

#unique(test$phyto.unique)

collectionsC <- collections %>%
  mutate(across(where(is.character), str_trim)) %>% ## remove leading & trailing whitespace!!
  filter(plot < 43, bkgrd != "VIVI", phyto.n.indiv > 0) %>%
  mutate(phyto.unique = toupper(phyto.unique),
         unique.ID = unique, ## standardize column name
         bkgrd.n.indiv = ifelse(bkgrd == "Control", NA, bkgrd.n.indiv), 
         Nbrhood.size = ifelse(phyto %in% nhood10, 10, ## fill in all vals of neighborhood size
                               ifelse(phyto %in% nhood18, 18, Nbrhood.size))) %>% ## change # of background indiv in controls to NA
  mutate_all(na_if,"") %>%
  filter(phyto %in% summer_phytos)



# Check Notes ####
## create empty data frame
df <- data.frame()

## loop through all notes searching for "die" or "chang"
for(i in colnames(collectionsC)[20:26]) {
  tmp <- dplyr::filter(collectionsC, grepl("die", collectionsC[,i]))
  df <- rbind(df, tmp)
}

for(i in colnames(collectionsC)[20:26]) {
  tmp <- dplyr::filter(collectionsC, grepl("chang", collectionsC[,i]))
  df <- rbind(df, tmp)
}

subnotes <- collectionsC %>%
  filter(!is.na(sub.notes))
## unique 3934 double planted
## unique 4027 may have 4 seeds planted
## unique 8138 4 ANAR seeds planted
## unique 8347 might be double planted
## unique 825 might be double planted
## unique 10038 might be double planted


plotnotes <- collectionsC %>%
  filter(!is.na(plot.notes))
## no useful notes here.

# Add in intra-phyto column ####




# Separate Cen Dat ####
# Pull out census data
phyto.census <- collectionsC %>%
  select(unique.ID, phyto.n.indiv, Nbrhood.size, bkgrd.n.indiv, CRCO, ERBO, FIGA, GAMU, HYGL, SIGA, other)




rm(list = c("date_collections"))