# Read in Data ####
rm(list=ls())
source("data_cleaning/merge_processing_collections_data.R")

## for spot checks, perhaps select 8 blocks and 5 samples from each?
brho <- all_dat_final %>%
  filter(phyto == "BRHO")

## create empty data frame
brho_spotcheck <- data.frame()
blocks <- unique(brho$block)
blocksamp <- sample_n(blocks, 8)


## search for "die"
for(i in 1:length(blocks)) {
  
  ## subset block
  tmp <- brho %>%
    filter(block == blocks[i])
  
  ## randomly sample
  rsamp <- sample_n(tmp, 5)
  
  ## append
  brho_spotcheck <- rbind(brho_spotcheck, rsamp)
}
