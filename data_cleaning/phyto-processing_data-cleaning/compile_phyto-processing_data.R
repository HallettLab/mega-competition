## the purpose of this script is to combine all clean phyto seeds in/out data and create a df that is ready to be combined with bg seeds in/out

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

## merge all final phyto seeds in/seeds out dfs together
all.phytos <- do.call("rbind", list(brho.phyto, gitr.phyto, pler.phyto, avba.phyto, acam.phyto, anar.phyto, mael.phyto))
    ## add more phytos in as they are ready here.


## clean env 
rm(list = c("acam.phyto", "anar.phyto", "avba.phyto", "basic_cleaning_func", "brho.phyto", "gitr.phyto", "mael.phyto", "pler.phyto" ))
  