
# Set up Env ####
library(tidyverse)

## read in posterior data
posts <- read.csv("data/posteriors_20231218_models.csv")

## read in replicate info to filter out params that were not estimated using enough data
reps <- read.csv("data/replicate-info.csv")

# Clean Posteriors ####
bad.reps <- reps %>%
  filter(true.reps < 4) %>%
  filter(!bkgrd %in% c("CLPU", "AVBA", "ERBO"), 
         !phyto %in% c("CLPU", "AVBA", "ERBO"))

## could filter out the phytos that have some bad alphas, fill bad alpha cols with NAs, then rbind back to the rest of the data

unique(bad.reps$phyto)
## "ACAM" "AMME" "BRNI" "MAEL" "PLNO" "TWIL" "LOMU"

## ACAM ####
ACAM_reps <- bad.reps %>%
  filter(phyto == "ACAM")

unique(ACAM_reps$combos)

ACAM_filt <- posts %>%
  filter(species == "ACAM") %>%
  mutate(alpha_brho_c = NA, 
         alpha_ceso_c = NA, 
         alpha_thir_c = NA, 
         alpha_mael_d = NA,
         alpha_plno_d = NA)

## AMME ####
AMME_reps <- bad.reps %>%
  filter(phyto == "AMME")

unique(AMME_reps$combos)

AMME_filt <- posts %>%
  filter(species == "AMME") %>%
  mutate(alpha_acam_c = NA, 
         alpha_acam_d = NA,
         alpha_ceso_c = NA,
         alpha_gitr_c = NA,
         alpha_mael_c = NA,
         alpha_pler_c = NA,
         alpha_plno_c = NA,
         alpha_taca_c = NA, 
         alpha_thir_c = NA, 
         alpha_twil_c = NA, 
         alpha_lomu_d = NA,
         alpha_mica_d = NA,
         alpha_plno_d = NA)

## BRNI ####
BRNI_reps <- bad.reps %>%
  filter(phyto == "BRNI")

unique(BRNI_reps$combos)

BRNI_filt <- posts %>%
  filter(species == "BRNI") %>%
  mutate(alpha_brni_c = NA, 
         alpha_ceso_c = NA,
         alpha_plno_d = NA,
         alpha_brni_d = NA)

## MAEL ####
MAEL_reps <- bad.reps %>%
  filter(phyto == "MAEL")

unique(MAEL_reps$combos)

MAEL_filt <- posts %>%
  filter(species == "MAEL") %>%
  mutate(alpha_acam_c = NA, 
         alpha_amme_c = NA,
         alpha_plno_c = NA, 
         alpha_acam_d = NA,
         alpha_amme_d = NA,
         alpha_pler_d = NA,
         alpha_plno_d = NA,
         alpha_twil_d = NA)

## PLNO ####
PLNO_reps <- bad.reps %>%
  filter(phyto == "PLNO")

unique(PLNO_reps$combos)

PLNO_filt <- posts %>%
  filter(species == "PLNO") %>%
  mutate(alpha_amme_c = NA,
         alpha_anar_c = NA, 
         alpha_brho_c = NA, 
         alpha_ceso_c = NA, 
         alpha_mael_c = NA, 
         alpha_pler_c = NA, 
         alpha_taca_c = NA, 
         alpha_amme_d = NA,
         alpha_anar_d = NA, 
         alpha_brni_d = NA, 
         alpha_ceso_d = NA, 
         alpha_leni_d = NA, 
         alpha_lomu_d = NA)

## TWIL ####
TWIL_reps <- bad.reps %>%
  filter(phyto == "TWIL")

unique(TWIL_reps$combos)

TWIL_filt <- posts %>%
  filter(species == "TWIL") %>%
  mutate(alpha_acam_c = NA,
         alpha_brho_c = NA, 
         alpha_gitr_c = NA, 
         alpha_leni_c = NA,
         alpha_mael_c = NA, 
         alpha_pler_c = NA,
         alpha_plno_c = NA, 
         alpha_ceso_d = NA, 
         alpha_mica_d = NA,
         alpha_pler_d = NA,
         alpha_plno_d = NA)

## LOMU ####
LOMU_reps <- bad.reps %>%
  filter(phyto == "LOMU")

unique(LOMU_reps$combos)

LOMU_filt <- posts %>%
  filter(species == "LOMU") %>%
  mutate(alpha_amme_d = NA)

## remove the edited phytos from the main df
posts_filt <- posts %>%
  filter(!species %in% c("ACAM", "AMME", "BRNI", "MAEL", "PLNO", "TWIL", "LOMU"))

## Join together ####
posts_clean <- rbind(posts_filt, ACAM_filt, AMME_filt, BRNI_filt, LOMU_filt, MAEL_filt, PLNO_filt, TWIL_filt)

# Clean Env ####
rm(ACAM_filt, AMME_filt, BRNI_filt, LOMU_filt, MAEL_filt, PLNO_filt, TWIL_filt, posts, posts_filt, ACAM_reps, AMME_reps, BRNI_reps, LOMU_reps, MAEL_reps, PLNO_reps, TWIL_reps)
