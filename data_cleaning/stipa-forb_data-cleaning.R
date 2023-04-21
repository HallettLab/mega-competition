## Stipa Forb Script

## Read in data
source("data_cleaning/phyto-processing_data-cleaning/ACAM_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/ANAR_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/BRHO_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/CLPU_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/GITR_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/LENI_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/LOMU_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/MAEL_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/PLER_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/THIR_phyto.R")
source("data_cleaning/phyto-processing_data-cleaning/TWIL_phyto.R")


acamMC2 <- acam_final %>%
  mutate(inflor.g = NA, 
         seed.num = NA, 
         empty.flower.num = NA,
         
         total.biomass.g = total.biomass.g.rounded) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique,  total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, scale.ID, process.notes, census.notes, unique.ID)

anarMC2 <- anar_final %>%
  mutate(inflor.g = NA, 
         seed.num = NA, 
         empty.flower.num = NA) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, scale.ID, process.notes, census.notes, unique.ID)


gitrMC2 <- gitr_final %>%
  mutate(inflor.g = NA, 
         seed.num = NA, 
         empty.flower.num = NA,
         
         total.biomass.g = total.biomass.g.rounded) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, scale.ID, process.notes, census.notes, unique.ID)


thirMC2 <- thir_final %>%
  mutate(empty.flower.num = NA,
         flower.num = NA,
         total.biomass.g = total.biomass.g.rounded,
         inflor.g = NA, 
         seed.num = NA) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, scale.ID, process.notes, census.notes, unique.ID)


twilMC2 <- twil_final %>%
  filter(majority.seeds.present == "Y", redo.complete == "Y") %>%
  mutate(inflor.g = NA, 
         empty.flower.num = NA,
         
         flower.num = NA, 
         total.biomass.g = total.biomass.g.rounded,
         seed.num = NA) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, scale.ID, process.notes, census.notes, unique.ID)

lomuMC2 <- lomu_final %>%
  mutate(inflor.g = NA, 
         empty.flower.num = NA,
         
         flower.num = NA,
         total.biomass.g = final.total.biomass.g,
         seed.num = NA) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, scale.ID, process.notes, census.notes, unique.ID)

brhoMC2 <- brho_final %>%
  mutate(empty.flower.num = NA,
         
         flower.num = NA, 
         inflor.g = inflor.g.rounded) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, scale.ID, process.notes, census.notes, unique.ID)

plerMC2 <- pler_final2 %>%
  mutate(total.stem.length.mm = NA,
         pod.num = NA,
         inflor.g = inflor.g.rounded, 
         total.biomass.g = total.biomass.g.rounded) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num,  scale.ID, process.notes, census.notes, unique.ID)

leniMC2 <- leni_final %>%
  mutate(empty.flower.num = NA,
         inflor.g = NA, 
         flower.num = NA) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, scale.ID, process.notes, census.notes, unique.ID)

maelMC2 <- mael_final %>%
  mutate(empty.flower.num = NA,
         inflor.g = NA,
         seed.num = NA) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, scale.ID, process.notes, census.notes, unique.ID)



BC.background.merged <- do.call("rbind", list(acamMC2, anarMC2, brhoMC2, gitrMC2, lomuMC2, leniMC2, maelMC2, plerMC2, thirMC2, twilMC2)) %>%
  mutate(total.biomass.rounded.percap = total.biomass.g/phyto.n.indiv,
         empty.flower.num.percap = empty.flower.num/phyto.n.indiv,
         flower.num.percap = flower.num/phyto.n.indiv,
         inflor.g.rounded.percap =  inflor.g/phyto.n.indiv,
         seed.num.percap = seed.num/phyto.n.indiv
  ) %>%
  mutate(phyto = ifelse(phyto == "THIR-I", "THIR", 
                        ifelse(phyto == "TWIL-I", "TWIL", phyto))) %>%
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, total.biomass.rounded.percap, empty.flower.num.percap, flower.num.percap, scale.ID, inflor.g.rounded.percap, seed.num.percap, process.notes, census.notes, unique.ID) %>%
  filter(bkgrd == "BRHO" | bkgrd == "Control")


write.csv(BC.background.merged, "brho_control_bkgrd.csv")






