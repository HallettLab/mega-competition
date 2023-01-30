# Load packages ####
library(tidyverse)

# Read in Data ####
## phyto-processing data
## specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/"
  
} else {
  # Marina
  lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/"
} 

lomu <- read.csv(paste0(lead, "LOMU_phyto-processing-redo_20230124.csv"))

## basic cleaning function
source("data_cleaning/phyto-processing_data-cleaning/basic_cleaning_function.R")


# Check Redo ####
#colnames(lomu)
#str(lomu)
#unique(lomu$redo.total.biomass) ## someone wrote missing in this column


#lomu.redo <- lomu %>%
 # filter(redo.total.biomass != "", redo.total.biomass != "missing")

#lomu.redo$redo.total.biomass <- as.numeric(lomu.redo$redo.total.biomass)
  
#lomu.redo <- lomu.redo %>%
 # mutate(redo.diff = redo.total.biomass - total.biomass.g)

#ggplot(lomu.redo, aes(x=redo.diff)) +
 # geom_histogram() +
  #geom_vline(xintercept = 0)

#concerns <- lomu.redo %>%
 # filter(redo.diff > 0)



#MWblocks <- c(6, 7, 12, 14, 15)

#lomu.needs.redo <- lomu %>%
 # filter(block %in% MWblocks, redo.total.biomass == "")
  




