## Set up env
library(tidyverse)

## Read in data
source("data_cleaning/phyto-processing_data-cleaning/BRHO_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/GITR_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/AVBA_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/PLER_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/ACAM_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/ANAR_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/MAEL_phyto.R")

## the final data frames are brho.phyto and gitr.phyto
all.phytos <- do.call("rbind", list(brho.phyto, gitr.phyto, pler.phyto, avba.phyto, acam.phyto, anar.phyto, mael.phyto))
    ## add more phytos in as they are ready here.


## clean env 
rm(list = c("acam.phyto", "anar.phyto", "avba.phyto", "basic_cleaning_func", "brho.phyto", "gitr.phyto", "mael.phyto", "pler.phyto" ))
  