### Merged BROHOR QAQC ##

rm(list=ls())
source("data_cleaning/merge_processing_collections_data.R")

all_dat_final_BRHO <- filter(all_dat_final, phyto == "BRHO")

df <- data.frame()

for(i in colnames(all_dat_final_BRHO)[45:48]) {
  tmp <- dplyr::filter(all_dat_final_BRHO, grepl("die", all_dat_final_BRHO[,i]))
  df <- rbind(df, tmp)
}

for(i in colnames(all_dat_final_BRHO)[45:48]) {
  tmp <- dplyr::filter(all_dat_final_BRHO, grepl("chang", all_dat_final_BRHO[,i]))
  df <- rbind(df, tmp)
}

all_dat_final$intraphyto <- 0 

