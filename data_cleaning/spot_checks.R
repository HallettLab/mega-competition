# Read in Data ####
rm(list=ls())
source("data_cleaning/initial_data_prep/merge_processing_collections_data.R")

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
  select(block, plot, sub, dens, bkgrd, phyto, phyto.n.indiv, phyto.unique, complete.sample, inflor.g, seed.num, total.biomass.g, scale.ID, unique.ID) %>%
  mutate(spot.check.complete.sample= NA, spot.check.inflor.g = NA, spot.check.scale = NA, spot.check.notes = NA)


write.csv(brhoSC, "brho_spotcheck_list.csv")




# GITR ####
gitr <- all_dat_final %>%
  filter(phyto == "GITR")

## create empty data frame
gitr_spotcheck <- data.frame()
blocks <- unique(gitr$block)
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


#gitrSC <- gitr_spotcheck %>%
 # select(block, plot, sub, dens, bkgrd, phyto, phyto.n.indiv, phyto.unique, complete.sample, inflor.g, seed.num, total.biomass.g, scale.ID, unique.ID) %>%
 # mutate(spot.check.complete.sample= NA, spot.check.inflor.g = NA, spot.check.scale = NA, spot.check.notes = NA)


gitrSC <- gitr_spotcheck %>%
  select(block, plot, sub, dens, bkgrd, phyto, phyto.n.indiv, phyto.unique, complete.sample, flower.num, total.biomass.g, scale.ID, unique.ID) %>%
  mutate(spot.check.complete.sample= NA, spot.check.total.biomass.g = NA, spot.check.scale = NA, spot.check.notes = NA)

write.csv(gitrSC, "gitr_spotcheck_list.csv")


# MICA ####
mica <- all_dat_final %>%
  filter(phyto == "MICA")

## create empty data frame
mica_spotcheck <- data.frame()
blocks <- unique(mica$block)
blocksamp <- sample(blocks, 8, replace = F)

## select 5 samples from each of 8 blocks
for(i in 1:length(blocksamp)) {
  
  ## subset block
  tmp <- mica %>%
    filter(block == blocksamp[i])
  
  ## randomly sample
  rsamp <- sample_n(tmp, 5)
  
  ## append
  mica_spotcheck <- rbind(mica_spotcheck, rsamp)
}

micaSC <- mica_spotcheck %>%
  select(block, plot, sub, dens, bkgrd, phyto, phyto.n.indiv, phyto.unique, complete.sample, seed.num, total.biomass.g, scale.ID, unique.ID) %>%
  mutate(spot.check.complete.sample= NA, spot.check.total.biomass.g = NA, spot.check.scale = NA, spot.check.notes = NA)

write.csv(micaSC, "mica_spotcheck_list.csv")


# THIR ####
thir <- all_dat_final %>%
  filter(phyto == "THIR-I")

## create empty data frame
thir_spotcheck <- data.frame()
blocks <- unique(thir$block)
blocksamp <- sample(blocks, 8, replace = F)

## select 5 samples from each of 8 blocks
for(i in 1:length(blocksamp)) {
  
  ## subset block
  tmp <- thir %>%
    filter(block == blocksamp[i])
  
  ## randomly sample
  rsamp <- sample_n(tmp, 5)
  
  ## append
  thir_spotcheck <- rbind(thir_spotcheck, rsamp)
}

thirSC <- thir_spotcheck %>%
  select(block, plot, sub, dens, bkgrd, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, scale.ID, unique.ID) %>%
  mutate(spot.check.complete.sample= NA, spot.check.total.biomass.g = NA, spot.check.scale = NA, spot.check.notes = NA)

write.csv(thirSC, "thir_spotcheck_list.csv")


# LOMU ####
lomu <- all_dat_final %>%
  filter(phyto == "LOMU")

## create empty data frame
lomu_spotcheck <- data.frame()
blocks <- unique(lomu$block)
blocksamp <- sample(blocks, 8, replace = F)

## select 5 samples from each of 8 blocks
for(i in 1:length(blocksamp)) {
  
  ## subset block
  tmp <- lomu %>%
    filter(block == blocksamp[i])
  
  ## randomly sample
  rsamp <- sample_n(tmp, 5)
  
  ## append
  lomu_spotcheck <- rbind(lomu_spotcheck, rsamp)
}

lomuSC <- lomu_spotcheck %>%
  select(block, plot, sub, dens, bkgrd, phyto, phyto.n.indiv, phyto.unique, complete.sample, flower.num, total.biomass.g, scale.ID, unique.ID) %>%
  mutate(spot.check.complete.sample= NA, spot.check.total.biomass.g = NA, spot.check.scale = NA, spot.check.notes = NA)

write.csv(lomuSC, "lomu_spotcheck_list.csv")


# LENI ####
leni <- all_dat_final %>%
  filter(phyto == "LENI")

## create empty data frame
leni_spotcheck <- data.frame()
blocks <- unique(leni$block)
blocksamp <- sample(blocks, 8, replace = F)

## select 5 samples from each of 8 blocks
for(i in 1:length(blocksamp)) {
  
  ## subset block
  tmp <- leni %>%
    filter(block == blocksamp[i])
  
  ## randomly sample
  rsamp <- sample_n(tmp, 5)
  
  ## append
  leni_spotcheck <- rbind(leni_spotcheck, rsamp)
}

leniSC <- leni_spotcheck %>%
  select(block, plot, sub, dens, bkgrd, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.stem.length.mm, scale.ID, unique.ID) %>%
  mutate(spot.check.complete.sample= NA, spot.check.total.biomass.g = NA, spot.check.scale = NA, spot.check.notes = NA)

write.csv(leniSC, "leni_spotcheck_list.csv")









