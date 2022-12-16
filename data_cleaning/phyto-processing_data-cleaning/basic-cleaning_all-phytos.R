# Set up env
library(tidyverse)

# Read in Data ####
## specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/"
  
} else {
  # Marina
  lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/"
} 

date_processing <- 20221018

## Processing data
acam <- read.csv(paste0(lead, "ACAM_phyto-processing-redo_20221209.csv"))
anar <- read.csv(paste0(lead, "ANAR_phyto-processing_", date_processing, ".csv"))
avba <- read.csv(paste0(lead, "AVBA_phyto-processing_", date_processing, ".csv")) %>%
  mutate(scale.ID = NA)
brho <- read.csv(paste0(lead, "BRHO_phyto-processing_20221201", ".csv"))
gitr <- read.csv(paste0(lead, "GITR_phyto-processing_", date_processing, ".csv"))
leni <- read.csv(paste0(lead, "LENI_phyto-processing_", date_processing, ".csv"))
lomu <- read.csv(paste0(lead, "LOMU_phyto-processing_", date_processing, ".csv"))
mica <- read.csv(paste0(lead, "MICA_phyto-processing_", date_processing, ".csv"))
pler <- read.csv(paste0(lead, "PLER_phyto-processing-redo_20221209.csv"))
thir <- read.csv(paste0(lead, "THIR_phyto-processing_", date_processing, ".csv"))
twil <- read.csv(paste0(lead, "TWIL_phyto-processing_", date_processing, ".csv"))
taca <- read.csv(paste0(lead, "TACA_phyto-processing_", "20221108", ".csv"))
mael <- read.csv(paste0(lead, "MAEL_phyto-processing_", "20221122", ".csv")) %>%
  mutate(scale.ID = NA)

# Define Cleaning Function ####
basic_cleaning_func <- function(phyto_data, ...) {
  
  drought <- c(1, 3, 4, 6, 12, 14) ## create treatment vector
  
  temp <- phyto_data %>%
    mutate(complete.sample = complete., ## change column names
           unique.ID = unique,
           treatment = ifelse(block %in% drought, "D", "C")) %>%  ## add a treatment column
    
    mutate(across(where(is.character), str_trim)) %>% ## remove leading & trailing whitespace!!
    
    mutate(across(c(phyto.unique,scale.ID,complete.sample), toupper)) %>% ## capitalize all vals 
    
    mutate_all(na_if,"") %>% ## make blank values NAs
    
    filter(plot < 43, bkgrd != "VIVI")
  
  return(temp)
  
}

# Clean DFs ####
## create vector of species to clean
sp.processed <- c("acam", "anar", "avba", "brho", "gitr", "leni", "lomu", "mica", "pler", "thir", "twil", "taca", "mael")

## clean each species and save output separately since colnames do not match between dataframes
for(i in 1:length(sp.processed)) {
  
  sp <- get(sp.processed[i]) ## fetch dataframe
  temp <- basic_cleaning_func(sp) ## apply cleaning function
  assign(paste0(sp.processed[i], "C"), temp) ## rename the output, keeping species separate
  
}

# Clean Env ####
rm(list = c("acam", "anar", "avba", "brho", "gitr", "leni", "lomu", "mica", "pler", "thir", "twil", "taca", "mael", "temp", "sp"))