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

## read in census data
source("data_cleaning/phyto-collections_data-cleaning/phyto-collections_data-cleaning.R")

# ACAM ####
acamMC2 <- acam_final %>%
  mutate(inflor.g = NA, 
         seed.num = NA, 
         empty.flower.num = NA,
         
         total.biomass.g = total.biomass.g.rounded) %>%
  filter(bkgrd %in% c("BRHO", "Control", "AVBA", "ERBO")) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique,  total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, scale.ID, process.notes, census.notes, unique.ID)

acam_bg = left_join(acamMC2, phyto.census, by = c("unique.ID", "phyto.n.indiv")) %>%
  filter(!(bkgrd == "AVBA" & bkgrd.n.indiv != 0),
         (bkgrd %in% c("Control", "BRHO") | unique.ID == 6982)) ## filter to keep only BRHO, Control, & select AVBA backgrounds

#acam_bg[acam_bg$block == 8 & acam_bg$plot == 31,]$unique.ID
## 6982 

## several AVBA in neighboring subs
## 1-40-22 has no AVBA in sub but several AVBA in neighboring subs; don't use
## 8-22-4: several AVBA around, don't use
## 12-4-1: no
## 12-42-1 
## 14-31-5
## 16-5-20

## one AVBA in neighboring subs
## 1-35-3 one AVBA in neighboring sub; potentially could use? 

## no AVBA in neighboring subs
## 8-31-4 OK to use, no AVBA in directly touching subs

# ANAR ####
anarMC2 <- anar_final %>%
  mutate(inflor.g = NA, 
         seed.num = NA, 
         empty.flower.num = NA) %>%
  filter(bkgrd %in% c("BRHO", "Control", "AVBA")) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, scale.ID, process.notes, census.notes, unique.ID)


anar_bg = left_join(anarMC2, phyto.census, by = c("unique.ID", "phyto.n.indiv")) %>%
  filter(!(bkgrd == "AVBA" & bkgrd.n.indiv != 0),
         bkgrd %in% c("BRHO", "Control"))

## multiple AVBA near
## 3-20-14
## 4-35-2
## 4-36-2
## 6-7-2
## 8-22-19
## 15-34-11
## 16-5-17
## 16-36-17

## 1 AVBA near
## 5-24-7
## 6-40-2

## probably don't want to keep any of these
## 3-41-10 seems ok to keep, don't know why it didn't show up on this search?

gitrMC2 <- gitr_final %>%
  mutate(inflor.g = NA, 
         seed.num = NA, 
         empty.flower.num = NA,
         
         total.biomass.g = total.biomass.g.rounded) %>%
  filter(bkgrd %in% c("BRHO", "Control", "AVBA")) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, scale.ID, process.notes, census.notes, unique.ID)

gitr_bg = left_join(gitrMC2, phyto.census, by = c("unique.ID", "phyto.n.indiv")) %>%
  filter(!(bkgrd == "AVBA" & bkgrd.n.indiv != 0),
         (bkgrd %in% c("Control", "BRHO") | unique.ID %in% c(2025,6990)))

## multiple AVBA near
## 1-40-16
## 3-20-19
## 5-18-11
## 14-7-9
## 16-5-24

## one AVBA near
## 4-35-21
## 6-40-21
## 7-22-4

## no AVBA near - these two are useable
## 3-41-7
## 8-31-12

#gitr_bg[gitr_bg$block %in% c(3,8) & gitr_bg$plot %in% c(41,31),]$unique.ID
#2025 6990

thirMC2 <- thir_final %>%
  mutate(empty.flower.num = NA,
         flower.num = NA,
         total.biomass.g = total.biomass.g.rounded,
         inflor.g = NA, 
         seed.num = NA) %>%
  filter(bkgrd %in% c("BRHO", "Control", "AVBA")) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, scale.ID, process.notes, census.notes, unique.ID)

thir_bg = left_join(thirMC2, phyto.census, by = c("unique.ID", "phyto.n.indiv")) %>%
  filter(!(bkgrd == "AVBA" & bkgrd.n.indiv != 0),
         (bkgrd %in% c("Control", "BRHO") | unique.ID %in% c(8501)))

## multiple AVBA near
## 1-40-11
## 3-41-9
## 12-42-20

## one nearby
## 16-5-4

## no AVBA near 
## 14-7-15

#thir_bg[thir_bg$block %in% c(14) & thir_bg$plot %in% c(7),]$unique.ID
#8501

twilMC2 <- twil_final %>%
  filter(## majority.seeds.present == "Y", ## these are complete except for seeds present; a lot of the samples are this way...
    redo.complete == "Y") %>%
  
  ## making change on 12/10/24 - keep ones without majority seeds present if they are otherwise complete
  
  mutate(inflor.g = NA, 
         empty.flower.num = NA,
         
         flower.num = NA, 
         total.biomass.g = total.biomass.g.rounded,
         seed.num = NA) %>%
  filter(bkgrd %in% c("BRHO", "Control", "AVBA")) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, scale.ID, process.notes, census.notes, unique.ID)

twil_bg = left_join(twilMC2, phyto.census, by = c("unique.ID", "phyto.n.indiv")) %>%
  filter(!(bkgrd == "AVBA" & bkgrd.n.indiv != 0),
         bkgrd %in% c("Control", "BRHO"))

## multiple AVBA near 
## 3-41-13
## 7-36-5

## one AVBA nearby
## 5-24-19

# LOMU ####
lomuMC2 <- lomu_final %>%
  mutate(inflor.g = NA, 
         empty.flower.num = NA,
         
         flower.num = NA,
         total.biomass.g = final.total.biomass.g,
         seed.num = NA) %>%
  filter(bkgrd %in% c("BRHO", "Control")) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, scale.ID, process.notes, census.notes, unique.ID)

lomu_bg = left_join(lomuMC2, phyto.census, by = c("unique.ID", "phyto.n.indiv"))

# BRHO ####
brhoMC2 <- brho_final %>%
  mutate(empty.flower.num = NA,
         
         flower.num = NA, 
         inflor.g = inflor.g.rounded) %>%
  filter(bkgrd %in% c("BRHO", "Control")) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, scale.ID, process.notes, census.notes, unique.ID)

brho_bg = left_join(brhoMC2, phyto.census, by = c("unique.ID", "phyto.n.indiv")) %>%
  filter(bkgrd %in% c("BRHO", "Control"))

# PLER ####
plerMC2 <- pler_final2 %>%
  mutate(total.stem.length.mm = NA,
         pod.num = NA,
         inflor.g = inflor.g.rounded, 
         total.biomass.g = total.biomass.g.rounded) %>%
  filter(bkgrd %in% c("BRHO", "Control", "AVBA")) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num,  scale.ID, process.notes, census.notes, unique.ID)

pler_bg = left_join(plerMC2, phyto.census, by = c("unique.ID", "phyto.n.indiv")) %>%
  filter(!(bkgrd == "AVBA" & bkgrd.n.indiv != 0),
         (bkgrd %in% c("Control", "BRHO") | unique.ID %in% c(2019, 6198, 6073, 6986)))

## multiple AVBA near
## 1-40-7
## 4-35-11
## 4-36-11
## 6-40-12
## 7-22-12
## 12-42-13
## 15-36-15

## one AVBA near
## 1-42-12

## no AVBA near
## 3-41-1
## 7-36-12
## 8-31-8
## 14-31-21

#pler_bg[pler_bg$block %in% c(3, 7, 8, 12) & pler_bg$plot %in% c(41,36,31),]$unique.ID
# 2019 6198 6073 6986 

# LENI ####
leniMC2 <- leni_final %>%
  mutate(empty.flower.num = NA,
         inflor.g = NA, 
         flower.num = NA) %>%
  filter(bkgrd %in% c("BRHO", "Control", "AVBA")) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, scale.ID, process.notes, census.notes, unique.ID)

leni_bg = left_join(leniMC2, phyto.census, by = c("unique.ID", "phyto.n.indiv")) %>%
  filter(!(bkgrd == "AVBA" & bkgrd.n.indiv != 0),
         (bkgrd %in% c("Control", "BRHO") | unique.ID %in% c(5142, 6067)))

## multiple AVBA near
## 8-22-9
## 12-42-12

## one AVBA near
## 7-22-6
## 8-31-9

## no AVBA near
## 6-40-23
## 7-36-6

## leni_bg[leni_bg$block %in% c(6,7) & leni_bg$plot %in% c(40,36),]$unique.ID
## 5142 6067

# MAEL ####
maelMC2 <- mael_final %>%
  mutate(empty.flower.num = NA,
         inflor.g = NA,
         seed.num = NA) %>%
  filter(bkgrd %in% c("BRHO", "Control", "AVBA")) %>%
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, total.biomass.g, inflor.g, flower.num, empty.flower.num, seed.num, scale.ID, process.notes, census.notes, unique.ID)

mael_bg = left_join(maelMC2, phyto.census, by = c("unique.ID", "phyto.n.indiv")) %>%
  filter(!(bkgrd == "AVBA" & bkgrd.n.indiv != 0),
         bkgrd %in% c("BRHO", "Control"))

## multiple AVBA near 
## 12-42-25
## 3-20-23

## one AVBA near
## 1-42-11

# Merge ####
BC.background.merged <- do.call("rbind", list(acam_bg, anar_bg, brho_bg, gitr_bg, lomu_bg, leni_bg, mael_bg, pler_bg, thir_bg, twil_bg)) %>%
  mutate(total.biomass.rounded.percap = total.biomass.g/phyto.n.indiv,
         empty.flower.num.percap = empty.flower.num/phyto.n.indiv,
         flower.num.percap = flower.num/phyto.n.indiv,
         inflor.g.rounded.percap =  inflor.g/phyto.n.indiv,
         seed.num.percap = seed.num/phyto.n.indiv
  ) %>%
  mutate(phyto = ifelse(phyto == "THIR-I", "THIR", 
                        ifelse(phyto == "TWIL-I", "TWIL", phyto))) #%>%
  

brho_bg_checks = BC.background.merged %>%
  filter(bkgrd == "BRHO")

ggplot(brho_bg_checks, aes(x=bkgrd.n.indiv)) +
  geom_histogram() +
  facet_grid(dens~Nbrhood.size) +
  xlab("Number of Background Individuals") +
  ylab("Count")

ggsave("stipa-forb_brho_dens_fig.png", width = 7.5, height = 5)

BC_final = BC.background.merged %>%
  mutate(bkgrd = ifelse(bkgrd == "AVBA", "Control", bkgrd)) %>%
select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, total.biomass.rounded.percap, empty.flower.num.percap, flower.num.percap, scale.ID, inflor.g.rounded.percap, seed.num.percap, process.notes, census.notes, unique.ID) 

write.csv(BC_final, "brho_control_bkgrd_20241210.csv")
