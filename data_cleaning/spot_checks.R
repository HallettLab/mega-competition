# Read in Data ####
rm(list=ls())
source("data_cleaning/merge_processing_collections_data.R")

# BRHO ####
## for spot checks, perhaps select 8 blocks and 5 samples from each?
brho <- all_dat_final %>%
  filter(phyto == "BRHO")

## create empty data frame
brho_spotcheck <- data.frame()
blocks <- unique(brho$block)
blocksamp <- sample(blocks, 8, replace = F)

## select 5 samples from each of 8 blocks
for(i in 1:length(blocksamp)) {
  
  ## subset block
  tmp <- brho %>%
    filter(block == blocksamp[i])
  
  ## randomly sample
  rsamp <- sample_n(tmp, 5)
  
  ## append
  brho_spotcheck <- rbind(brho_spotcheck, rsamp)
}

brhoSC <- brho_spotcheck %>%
  select(block, plot, sub, dens, bkgrd, phyto, phyto.n.indiv, inflor.g, seed.num, total.biomass.g, unique.ID)
write.csv(brhoSC, "brho_spotcheck_list.csv")




# GITR ####
gitr <- all_dat_final %>%
  filter(phyto == "GITR")

## create empty data frame
gitr_spotcheck <- data.frame()
blocks <- unique(brho$block)
blocksamp <- sample(blocks, 8, replace = F)

## select 5 samples from each of 8 blocks
for(i in 1:length(blocksamp)) {
  
  ## subset block
  tmp <- gitr %>%
    filter(block == blocksamp[i])
  
  ## randomly sample
  rsamp <- sample_n(tmp, 5)
  
  ## append
  gitr_spotcheck <- rbind(gitr_spotcheck, rsamp)
}

gitrSC <- gitr_spotcheck %>%
  select(block, plot, sub, dens, bkgrd, phyto, phyto.n.indiv, flower.num, total.biomass.g, unique.ID)
write.csv(gitrSC, "gitr_spotcheck_list.csv")













