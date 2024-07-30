
# Set up Env ####
library(tidyverse)
library(bayestestR)

date = 20240714

## read in posterior data
posts <- read.csv(paste0("data/posteriors_", date, "_models.csv"))

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
  filter(species == "ACAM")

## explore
ggplot(ACAM_filt, aes(x=alpha_brho_c))+
  geom_histogram()
hdi(ACAM_filt$alpha_brho_c)
## GOOD

ggplot(ACAM_filt, aes(x=alpha_ceso_c))+
  geom_histogram()
hdi(ACAM_filt$alpha_ceso_c)
1.08-0.21 ##0.87

ggplot(ACAM_filt, aes(x=alpha_thir_c))+
  geom_histogram()
hdi(ACAM_filt$alpha_thir_c)
0.5- (-.11)

ggplot(ACAM_filt, aes(x=alpha_taca_c))+
  geom_histogram()
hdi(ACAM_filt$alpha_taca_c)
0.69-0.08
## OKAY

ggplot(ACAM_filt, aes(x=alpha_mael_d))+
  geom_histogram()
hdi(ACAM_filt$alpha_mael_d)
0.77-(-1.31)
## REMOVE

ggplot(ACAM_filt, aes(x=alpha_plno_d))+
  geom_histogram()
hdi(ACAM_filt$alpha_plno_d)
2.23 - (-0.83)
## REMOVE

## change vals to NA
ACAM_filt2 = ACAM_filt %>%
  mutate(alpha_mael_d = NA,
         alpha_plno_d = NA)


## AMME ####
AMME_reps <- bad.reps %>%
  filter(phyto == "AMME")

unique(AMME_reps$combos)
#"AMME_ACAM_C" "AMME_CESO_C" "AMME_GITR_C" "AMME_MAEL_C" "AMME_PLER_C" "AMME_PLNO_C" "AMME_TACA_C"
#[8] "AMME_THIR_C" "AMME_TWIL_C" "AMME_ACAM_D" "AMME_LOMU_D" "AMME_MICA_D" "AMME_PLNO_D"

AMME_filt <- posts %>%
  filter(species == "AMME") 

hdi(AMME_filt$alpha_acam_c) ## IFFY
0.40 - (-0.83)
hdi(AMME_filt$alpha_ceso_c) ## GOOD
0.19-(-0.31) 
hdi(AMME_filt$alpha_gitr_c) ## GOOD
hdi(AMME_filt$alpha_mael_c) ## GOOD
hdi(AMME_filt$alpha_pler_c) ## GOOD
hdi(AMME_filt$alpha_plno_c) ## GOOD
hdi(AMME_filt$alpha_taca_c) ## GOOD
hdi(AMME_filt$alpha_thir_c) ## GOOD
hdi(AMME_filt$alpha_twil_c) ## GOOD
hdi(AMME_filt$alpha_acam_d) ## IFFY
0.31-(-0.62)
hdi(AMME_filt$alpha_lomu_d) ## GOOD
hdi(AMME_filt$alpha_mica_d) ## GOOD
hdi(AMME_filt$alpha_plno_d) ## GOOD

AMME_filt2 = AMME_filt %>%
  mutate(alpha_acam_c = NA, 
         alpha_acam_d = NA)

## BRNI ####
BRNI_reps <- bad.reps %>%
  filter(phyto == "BRNI")

unique(BRNI_reps$combos)

BRNI_filt <- posts %>%
  filter(species == "BRNI")

hdi(BRNI_filt$alpha_brni_c) ## GOOD
hdi(BRNI_filt$alpha_ceso_c) ## GOOD
hdi(BRNI_filt$alpha_brni_d) ## GOOD
hdi(BRNI_filt$alpha_plno_d) ## BAD
1.34-0.26
hdi(BRNI_filt$alpha_plno_c) ## OK
0.51 - -0.34
hdi(BRNI_filt$alpha_mael_c) ## OK
0.17 - -0.69

BRNI_filt2 <- posts %>%
  filter(species == "BRNI") %>%
  mutate(alpha_plno_d = NA)

## MAEL ####
MAEL_reps <- bad.reps %>%
  filter(phyto == "MAEL")

unique(MAEL_reps$combos)
#"MAEL_ACAM_C" "MAEL_AMME_C" "MAEL_PLNO_C" "MAEL_ACAM_D" "MAEL_AMME_D" "MAEL_PLER_D" "MAEL_PLNO_D"
#[8] "MAEL_TWIL_D"

MAEL_filt <- posts %>%
  filter(species == "MAEL")

hdi(MAEL_filt$alpha_acam_c) ## BAD
0.68 - -1.47
hdi(MAEL_filt$alpha_amme_c) ## OK
hdi(MAEL_filt$alpha_plno_c) ## OK
hdi(MAEL_filt$alpha_acam_d) ## BAD
hdi(MAEL_filt$alpha_amme_d) ## BAD
0.61- -0.83
hdi(MAEL_filt$alpha_pler_d) ## GOOD
hdi(MAEL_filt$alpha_plno_d) ## BAD
0.5- -1.78
hdi(MAEL_filt$alpha_twil_d) ## OK

MAEL_filt2 <- posts %>%
  filter(species == "MAEL") %>%
  mutate(alpha_acam_c = NA,
         alpha_acam_d = NA,
         alpha_amme_d = NA,
         alpha_plno_d = NA)

## PLNO ####
PLNO_reps <- bad.reps %>%
  filter(phyto == "PLNO")

unique(PLNO_reps$combos)

PLNO_filt <- posts %>%
  filter(species == "PLNO")

hdi(PLNO_filt$alpha_amme_c) ## GOOD
hdi(PLNO_filt$alpha_anar_c) ## GOOD
hdi(PLNO_filt$alpha_brho_c) ## GOOD
hdi(PLNO_filt$alpha_ceso_c) ## GOOD
hdi(PLNO_filt$alpha_mael_c) ## OK
hdi(PLNO_filt$alpha_pler_c) ## GOOD
hdi(PLNO_filt$alpha_taca_c) ## GOOD
hdi(PLNO_filt$alpha_amme_d) ## BAD
0.88--1.42
hdi(PLNO_filt$alpha_anar_d) ## GOOD
hdi(PLNO_filt$alpha_brni_d) ## GOOD
hdi(PLNO_filt$alpha_ceso_d) ## GOOD
hdi(PLNO_filt$alpha_leni_d) ## GOOD
hdi(PLNO_filt$alpha_lomu_d) ## GOOD

PLNO_filt2 <- posts %>%
  filter(species == "PLNO") %>%
  mutate(alpha_amme_d = NA)

## TWIL ####
TWIL_reps <- bad.reps %>%
  filter(phyto == "TWIL")

unique(TWIL_reps$combos)
#"TWIL_ACAM_C" "TWIL_BRHO_C" "TWIL_GITR_C" "TWIL_LENI_C" "TWIL_MAEL_C" "TWIL_PLER_C" "TWIL_PLNO_C"
#[8] "TWIL_CESO_D" "TWIL_MICA_D" "TWIL_PLER_D" "TWIL_PLNO_D"

TWIL_filt <- posts %>%
  filter(species == "TWIL")

hdi(TWIL_filt$alpha_acam_c) ## GOOD
hdi(TWIL_filt$alpha_brho_c) ## GOOD
hdi(TWIL_filt$alpha_gitr_c) ## GOOD
hdi(TWIL_filt$alpha_leni_c) ## GOOD
hdi(TWIL_filt$alpha_mael_c) ## BAD
2.9--0.55
hdi(TWIL_filt$alpha_pler_c) ## GOOD
hdi(TWIL_filt$alpha_plno_c) ## GOOD
hdi(TWIL_filt$alpha_ceso_d) ## GOOD
hdi(TWIL_filt$alpha_mica_d) ## GOOD
hdi(TWIL_filt$alpha_pler_d) ## GOOD
hdi(TWIL_filt$alpha_plno_d) ## OK

TWIL_filt2 <- posts %>%
  filter(species == "TWIL") %>%
  mutate(alpha_mael_c = NA)

## LOMU ####
LOMU_reps <- bad.reps %>%
  filter(phyto == "LOMU")

LOMU_filt <- posts %>%
  filter(species == "LOMU")

unique(LOMU_reps$combos)

hdi(LOMU_filt$alpha_amme_d) ## OK

LOMU_filt2 <- posts %>%
  filter(species == "LOMU")

## remove the edited phytos from the main df
posts_filt <- posts %>%
  filter(!species %in% c("ACAM", "AMME", "BRNI", "MAEL", "PLNO", "TWIL", "LOMU"))

## Join together ####
posts_clean <- rbind(posts_filt, ACAM_filt2, AMME_filt2, BRNI_filt2, LOMU_filt2, MAEL_filt2, PLNO_filt2, TWIL_filt2)

# Clean Env ####
rm(ACAM_filt, AMME_filt, BRNI_filt, LOMU_filt, MAEL_filt, PLNO_filt, TWIL_filt, posts, posts_filt, ACAM_reps, AMME_reps, BRNI_reps, LOMU_reps, MAEL_reps, PLNO_reps, TWIL_reps, ACAM_filt2, AMME_filt2, BRNI_filt2, LOMU_filt2, MAEL_filt2, PLNO_filt2, TWIL_filt2)
