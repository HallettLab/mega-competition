## Set up env
library(tidyverse)

## Read in data
source("data_cleaning/phyto-processing_data-cleaning/BRHO_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/GITR_phyto.R")


## the final data frames are brho.phyto and gitr.phyto
all.phytos <- do.call("rbind", list(brho.phyto, gitr.phyto))
    ## add more phytos in as they are ready here.
  