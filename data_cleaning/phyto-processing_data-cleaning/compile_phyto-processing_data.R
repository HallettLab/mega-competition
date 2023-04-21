## the purpose of this script is to combine all clean phyto seeds in/out data and create a df that is ready to be combined with bg seeds in/out

## Set up env
library(tidyverse)

## Read in data
source("data_cleaning/phyto-processing_data-cleaning/ACAM_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/ANAR_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/AVBA_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/AMME_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/BRHO_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/BRNI_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/CESO_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/CLPU_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/GITR_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/LENI_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/LOMU_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/MAEL_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/MICA_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/PLER_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/PLNO_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/TACA_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/THIR_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/TWIL_phyto.R")

## merge all final phyto seeds in/seeds out dfs together
all.phytos <- do.call("rbind", list(acam.phyto, anar.phyto, amme.phyto, avba.phyto, brho.phyto, brni.phyto, ceso.phyto, clpu.phyto, gitr.phyto, leni.phyto, lomu.phyto, mael.phyto, mica.phyto, pler.phyto, plno.phyto, taca.phyto, thir.phyto, twil.phyto))
    ## add more phytos in as they are ready here.


#ggplot(all.phytos, aes(x=phyto.seed.out)) +
  #geom_histogram() +
  #facet_wrap(~phyto, scales = "free")

#ggsave("models/CW/preliminary_figures/phyto.seed.output.png", height = 8, width = 12)

## clean env 
rm(list = c("acam.phyto", "anar.phyto", "amme.phyto", "avba.phyto", "basic_cleaning_func", "brho.phyto", "brni.phyto", "ceso.phyto",  "clpu.phyto", "gitr.phyto", "leni.phyto", "lomu.phyto", "mael.phyto", "mica.phyto", "pler.phyto", "plno.phyto", "taca.phyto", "thir.phyto", "twil.phyto", "not_planted", "i", "drought", "acam_final", "anar_final", "brho_final", "clpu_final", "gitr_final", "leni_final", "lomu_final", "mael_final", "pler_final2", "thir_final", "twil_final"))
