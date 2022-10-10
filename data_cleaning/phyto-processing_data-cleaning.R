## Cleaning all processing data together

## load packages
library(tidyverse)


# Read in Data ####
lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/" # Carmen's file path
date <- 20220927
date_collections <- 20220825

## Processing data
acam <- read.csv(paste0(lead, "ACAM_phyto-processing_", date, ".csv"))
anar <- read.csv(paste0(lead, "ANAR_phyto-processing_", date, ".csv"))
avba <- read.csv(paste0(lead, "AVBA_phyto-processing_", date, ".csv"))
brho <- read.csv(paste0(lead, "BRHO_phyto-processing_", date, ".csv"))
gitr <- read.csv(paste0(lead, "GITR_phyto-processing_", date, ".csv"))
leni <- read.csv(paste0(lead, "LENI_phyto-processing_", date, ".csv"))
mica <- read.csv(paste0(lead, "MICA_phyto-processing_", date, ".csv"))
pler <- read.csv(paste0(lead, "PLER_phyto-processing_", date, ".csv"))
thir <- read.csv(paste0(lead, "THIR_phyto-processing_", date, ".csv"))
twil <- read.csv(paste0(lead, "TWIL_phyto-processing_", date, ".csv"))


drought <- c(1, 3, 4, 6, 12, 14) ## create treatment vector

# Round 1 Cleaning ####
## Standardize ACAM ####
acamC <- acam %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C"), ## make sure all of the columns match!!
         phyto.unique = ifelse(phyto.unique == "a", "A", ## change all values to caps
                                      ifelse(phyto.unique == "b","B",
                                             ifelse(phyto.unique == "c", "C", phyto.unique))),
         complete.sample = ifelse(complete. == "y", "Y", ## change all values to caps
                                  ifelse(complete. == " Y","Y", 
                                         ifelse(complete. == "n", "N", complete.))),
         scale.ID = ifelse(scale.ID == "e", "E", ## change all values to caps
                           ifelse(scale.ID == "a","A", scale.ID)),
         unique.ID = unique,
         biomass.no.seed.g = NA, 
         #total.biomass.g = NA,
         #flower.num = NA,
         inflor.g = NA, 
         seed.num = NA,
         pod.num = NA,
         total.stem.length.mm = NA, 
         empty.flower.num = NA,
         seeds.present = NA,
         glume.num = NA) %>%
  filter(plot < 43, bkgrd != "VIVI") %>% 
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, pod.num, glume.num, seeds.present, scale.ID, process.notes, census.notes,unique.ID) ## make sure column ordering works


## Standardize ANAR ####
anarC <- anar %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C"), ## make sure all of the columns match!!
         phyto.unique = ifelse(phyto.unique == "a", "A", ## change all values to caps
                               ifelse(phyto.unique == "b","B",
                                      ifelse(phyto.unique == "c", "C", phyto.unique))),
         complete.sample = ifelse(complete. == "y", "Y", ## change all values to caps
                                  ifelse(complete. == " Y","Y", 
                                         ifelse(complete. == "n", "N", complete.))),
         scale.ID = ifelse(scale.ID == "e", "E", ## change all values to caps
                           ifelse(scale.ID == "a","A", scale.ID)),
         unique.ID = unique,
         biomass.no.seed.g = NA, 
         #total.biomass.g = NA,
         #flower.num = NA,
         inflor.g = NA, 
         seed.num = NA,
         pod.num = NA,
         total.stem.length.mm = NA, 
         empty.flower.num = NA,
         glume.num = NA,
         seeds.present = NA,
         census.notes = notes) %>% 
  filter(plot < 43, bkgrd != "VIVI") %>% 
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, pod.num, glume.num, seeds.present, scale.ID, process.notes, census.notes, unique.ID) ## make sure column ordering works


### add in anar data ####
tempanar <- rbind(acamC, anarC)

## Standardize AVBA ####
avbaC <- avba %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C"), ## make sure all of the columns match!!
         phyto.unique = ifelse(phyto.unique == "a", "A", ## change all values to caps
                               ifelse(phyto.unique == "b","B",
                                      ifelse(phyto.unique == "c", "C", phyto.unique))),
         complete.sample = ifelse(complete. == "y", "Y", ## change all values to caps
                                  ifelse(complete. == " Y","Y", 
                                         ifelse(complete. == "n", "N", complete.))),
         #scale.ID = ifelse(scale.ID == "e", "E", ## change all values to caps
                           #ifelse(scale.ID == "a","A", scale.ID)),
         unique.ID = unique,
         biomass.no.seed.g = NA, 
         total.biomass.g = NA,
         flower.num = NA,
         inflor.g = NA, 
         #seed.num = NA,
         pod.num = NA,
         total.stem.length.mm = NA, 
         empty.flower.num = NA,
         #glume.num = NA,
         seeds.present = NA,
         scale.ID = NA
         ) %>% 
  filter(plot < 43, bkgrd != "VIVI") %>% 
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, pod.num, glume.num, seeds.present, scale.ID, process.notes, census.notes, unique.ID) ## make sure column ordering works

### add in avba data ####
tempavba <- rbind(tempanar, avbaC)


## Standardize BRHO ####
brhoC <- brho %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C"), ## make sure all of the columns match!!
         phyto.unique = ifelse(phyto.unique == "a", "A", ## change all values to caps
                               ifelse(phyto.unique == "b","B",
                                      ifelse(phyto.unique == "c", "C", phyto.unique))),
         complete.sample = ifelse(complete. == "y", "Y", ## change all values to caps
                                  ifelse(complete. == " Y","Y", 
                                         ifelse(complete. == "n", "N", complete.))),
         scale.ID = ifelse(scale.ID == "e", "E", ## change all values to caps
                           ifelse(scale.ID == "a","A", scale.ID)),
         unique.ID = unique,
         #biomass.no.seed.g = NA, 
         #total.biomass.g = NA,
         flower.num = NA,
         #inflor.g = NA, 
         #seed.num = NA,
         pod.num = NA,
         total.stem.length.mm = NA, 
         empty.flower.num = NA,
         seeds.present = NA,
         glume.num = NA) %>% 
  filter(plot < 43, bkgrd != "VIVI") %>% 
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, pod.num, glume.num, seeds.present, scale.ID, process.notes, census.notes, unique.ID) ## make sure column ordering works


### add in brho data ####
tempbrho <- rbind(tempavba, brhoC)

## Standardize GITR ####
gitrC <- gitr %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C"), ## make sure all of the columns match!!
         phyto.unique = ifelse(phyto.unique == "a", "A", ## change all values to caps
                               ifelse(phyto.unique == "b","B",
                                      ifelse(phyto.unique == "c", "C", phyto.unique))),
         complete.sample = ifelse(complete. == "y", "Y", ## change all values to caps
                                  ifelse(complete. == " Y","Y", 
                                         ifelse(complete. == "n", "N", complete.))),
         scale.ID = ifelse(scale.ID == "e", "E", ## change all values to caps
                           ifelse(scale.ID == "a","A", scale.ID)),
         unique.ID = unique,
         biomass.no.seed.g = NA, 
         #total.biomass.g = NA,
         #flower.num = NA,
         inflor.g = NA, 
         seed.num = NA,
         pod.num = NA,
         total.stem.length.mm = NA, 
         empty.flower.num = NA,
         seeds.present = NA,
         glume.num = NA) %>% 
  filter(plot < 43, bkgrd != "VIVI") %>% 
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, pod.num, glume.num, seeds.present, scale.ID, process.notes, census.notes, unique.ID) ## make sure column ordering works

### add in gitr data ####
tempgitr <- rbind(tempbrho, gitrC)

## Standardize MICA ####
micaC <- mica %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C"), ## make sure all of the columns match!!
         phyto.unique = ifelse(phyto.unique == "a", "A", ## change all values to caps
                               ifelse(phyto.unique == "b","B",
                                      ifelse(phyto.unique == "c", "C", phyto.unique))),
         complete.sample = ifelse(complete. == "y", "Y", ## change all values to caps
                                  ifelse(complete. == " Y","Y", 
                                         ifelse(complete. == "n", "N", complete.))),
         scale.ID = ifelse(scale.ID == "e", "E", ## change all values to caps
                           ifelse(scale.ID == "a","A", scale.ID)),
         unique.ID = unique,
         #biomass.no.seed.g = NA, 
         #total.biomass.g = NA,
         flower.num = NA,
         inflor.g = NA, 
         #seed.num = NA,
         pod.num = NA,
         total.stem.length.mm = NA, 
         empty.flower.num = NA,
         seeds.present = NA,
         glume.num = NA) %>% 
  filter(plot < 43, bkgrd != "VIVI") %>% 
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, pod.num, glume.num, seeds.present, scale.ID, process.notes, census.notes, unique.ID) ## make sure column ordering works

### add in mica data ####
tempmica <- rbind(tempgitr, micaC)


## Standardize LENI ####
leniC <- leni %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C"), ## make sure all of the columns match!!
         phyto.unique = ifelse(phyto.unique == "a", "A", ## change all values to caps
                               ifelse(phyto.unique == "b","B",
                                      ifelse(phyto.unique == "c", "C", phyto.unique))),
         complete.sample = ifelse(complete. == "y", "Y", ## change all values to caps
                                  ifelse(complete. == " Y","Y", 
                                         ifelse(complete. == "n", "N", complete.))),
         scale.ID = ifelse(scale.ID == "e", "E", ## change all values to caps
                           ifelse(scale.ID == "a","A", scale.ID)),
         unique.ID = unique,
         biomass.no.seed.g = NA, 
         total.biomass.g = total.biomass,
         flower.num = NA,
         inflor.g = NA, 
         #seed.num = NA,
         #pod.num = NA,
         #total.stem.length.mm = NA, 
         empty.flower.num = NA,
         seeds.present = NA,
         glume.num = NA) %>% 
  filter(plot < 43, bkgrd != "VIVI") %>% 
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, pod.num, glume.num, seeds.present, scale.ID, process.notes, census.notes, unique.ID) ## make sure column ordering works



### add in leni data ####
templeni <- rbind(tempmica, leniC)


## Standardize PLER ####
plerC <- pler %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C"), ## make sure all of the columns match!!
         phyto.unique = ifelse(phyto.unique == "a", "A", ## change all values to caps
                               ifelse(phyto.unique == "b","B",
                                      ifelse(phyto.unique == "c", "C", phyto.unique))),
         complete.sample = ifelse(complete. == "y", "Y", ## change all values to caps
                                  ifelse(complete. == " Y","Y", 
                                         ifelse(complete. == "n", "N", complete.))),
         scale.ID = ifelse(scale.ID == "e", "E", ## change all values to caps
                           ifelse(scale.ID == "a","A", scale.ID)),
         unique.ID = unique,
         biomass.no.seed.g = NA, 
         #total.biomass.g = NA,
         #flower.num = NA,
         #inflor.g = NA, 
         #seed.num = NA,
         pod.num = NA,
         total.stem.length.mm = NA, 
         #empty.flower.num = NA,
         glume.num = NA,
         seeds.present = NA
         ) %>% 
  filter(plot < 43, bkgrd != "VIVI") %>% 
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, pod.num, glume.num, seeds.present, scale.ID, process.notes, census.notes, unique.ID) ## make sure column ordering works


### add in pler data ####
temppler <- rbind(templeni, plerC)


## Standardize THIR ####
thirC <- thir %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C"), ## make sure all of the columns match!!
                phyto.unique = ifelse(phyto.unique == "a", "A", ## change all values to caps
                                      ifelse(phyto.unique == "b","B",
                                             ifelse(phyto.unique == "c", "C", phyto.unique))),
                complete.sample = ifelse(complete. == "y", "Y", ## change all values to caps
                                         ifelse(complete. == " Y","Y", 
                                                ifelse(complete. == "n", "N", complete.))),
                scale.ID = ifelse(scale.ID == "e", "E", ## change all values to caps
                                  ifelse(scale.ID == "a","A", scale.ID)),
                unique.ID = unique,
                #biomass.no.seed.g = NA, 
                #total.biomass.g = NA,
                flower.num = NA,
                #inflor.g = NA, 
                #seed.num = NA,
                pod.num = NA,
                total.stem.length.mm = NA, 
                empty.flower.num = NA,
         seeds.present = NA,
         glume.num = NA) %>% 
  filter(plot < 43, bkgrd != "VIVI") %>% 
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, pod.num, glume.num, seeds.present, scale.ID, process.notes, census.notes, unique.ID) ## make sure column ordering works

### add in thir data ####
tempthir <- rbind(temppler, thirC)


## Standardize TWIL ####
twilC <- twil %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C"), ## make sure all of the columns match!!
         phyto.unique = ifelse(phyto.unique == "a", "A", ## change all values to caps
                               ifelse(phyto.unique == "b","B",
                                      ifelse(phyto.unique == "c", "C", phyto.unique))),
         complete.sample = ifelse(complete. == "y", "Y", ## change all values to caps
                                  ifelse(complete. == " Y","Y", 
                                         ifelse(complete. == "n", "N", complete.))),
         scale.ID = ifelse(scale.ID == "e", "E", ## change all values to caps
                           ifelse(scale.ID == "a","A", scale.ID)),
         unique.ID = unique,
         biomass.no.seed.g = NA, 
         #total.biomass.g = NA,
         flower.num = NA,
         inflor.g = NA, 
         #seed.num = NA,
         pod.num = NA,
         total.stem.length.mm = NA, 
         empty.flower.num = NA,
         seeds.present = NA,
         glume.num = NA) %>% 
  filter(plot < 43, bkgrd != "VIVI") %>% 
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, pod.num, glume.num, seeds.present, scale.ID, process.notes, census.notes, unique.ID) ## make sure column ordering works

### add in twil data ####
temptwil <- rbind(tempthir, twilC)




# Round 1 Checks ####
ggplot(temptwil, aes(x=phyto.unique)) +
  geom_bar()
unique(temptwil$phyto.unique)
## Need to standardize, further

ggplot(temptwil, aes(x=scale.ID)) +
  geom_bar()
## Need to standardize, further

ggplot(temptwil, aes(x=complete.sample)) +
  geom_bar()
## Need to standardize, further


ggplot(temptwil, aes(x=bkgrd)) +
  geom_bar()
## Not all the background names are standardized
unique(temptwil$bkgrd)

round1 <- temptwil %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C"), ## make sure all of the columns match!!
         phyto.unique = ifelse(phyto.unique == "A ", "A", ## change all values to caps
                               ifelse(phyto.unique == "c ", "C", 
                                      ifelse(phyto.unique == "C ", "C", phyto.unique))),
         complete.sample = ifelse(complete.sample == " N", "N", complete.sample),
         scale.ID = ifelse(scale.ID == "d", "D", scale.ID),
         phyto = ifelse(phyto == "THIR-I ", "THIR-I", phyto),
         bkgrd = ifelse(bkgrd == "BRNI ", "BRNI",
                        ifelse(bkgrd == "THIR-I ", "THIR-I",
                               ifelse(bkgrd == "TWIL-I ", "TWIL-I", bkgrd)))) ## change all values to caps
                  
ggplot(round1, aes(x=phyto.unique)) +
  geom_bar()

ggplot(round1, aes(x=scale.ID)) +
  geom_bar()
## Need to standardize, further

ggplot(round1, aes(x=complete.sample)) +
  geom_bar()

ggplot(round1, aes(x=bkgrd)) +
  geom_bar()



# Round 2 Cleaning ####

## Fix non-numeric cols ####
## Initial issues that arose during this step: 
    ## inflor.g is a character
    ## flower.num is a character

## filter out the non-numeric observations that are messing up inflor.g and flower.num
non_nums_inflor <- round1 %>%
  filter(!is.na(inflor.g)) %>%
  filter(!(!is.na(as.numeric(inflor.g)))) ## lol I don't quite know why this works.... but it did separate out the problematic observation!
## 7-15-8; BRHO phyto with inflor.g "1.6847g"

round1[round1$block == 7 & round1$plot == 15 & round1$sub == 8, ]$inflor.g <- 1.6847

## change the column to numeric
round1$inflor.g <- as.numeric(round1$inflor.g)

non_nums_flower <- round1 %>%
  filter(!is.na(flower.num)) %>%
  filter(!(!is.na(as.numeric(flower.num))))
## 1-30-23; PLER phyto has a space in flower.num

round1[round1$block == 1 & round1$plot == 30 & round1$sub == 23,]$flower.num <- NA

## change the column to numeric
round1$flower.num <- as.numeric(round1$flower.num)


## Filter, Round, & PerCap ####
med_scales <- c("A", "E", "F") ## scales that need to be rounded

## round weights that were taken using the medium scales as scale A goes to 4 decimal places while scales E and F go to 3
## we are rounding to 3 decimal places overall 
round2 <- round1 %>%
  filter(complete.sample == "Y") %>% ## remove incomplete samples
  mutate(total.biomass.g.rounded = ifelse(scale.ID %in% med_scales, round(total.biomass.g, digits = 3), total.biomass.g),
         inflor.g.rounded = ifelse(scale.ID %in% med_scales, round(inflor.g, digits = 3), inflor.g)) %>%
  
  mutate(total.biomass.rounded.percap = total.biomass.g.rounded/phyto.n.indiv,
         
         inflor.g.rounded.percap = inflor.g.rounded/phyto.n.indiv,
         
         seed.num.percap = seed.num/phyto.n.indiv,
         flower.num.percap = flower.num/phyto.n.indiv,
         total.stem.length.mm.percap = total.stem.length.mm/phyto.n.indiv,
         empty.flower.num.percap = empty.flower.num/phyto.n.indiv,
         pod.num.percap = pod.num/phyto.n.indiv,
         glume.num.percap = glume.num/phyto.n.indiv)

## Samples to Check ####        
incompletes_to_check <- round1 %>%
  filter(complete.sample != "Y")
nrow(incompletes_to_check)

nas_to_check <- round1 %>%
  filter(is.na(complete.sample))
nrow(nas_to_check)

## 350 samples removed for completion issues. Will need to check through these!!

proc_dat_clean <- round2

## clean up environment
rm(list = c("acam", "acamC", "anar", "anarC", "avba", "avbaC", "brho", "brhoC", "date", "date_collections", "drought", "gitr", "gitrC", "lead", "leni", "leniC", "med_scales",  "mica", "micaC", "non_nums_flower", "non_nums_inflor", "pler", "plerC", "round1", "round2", "tempanar", "tempavba", "tempbrho", "tempgitr", "templeni", "tempmica", "temppler", "tempthir", "temptwil", "thir", "thirC", "twil", "twilC"))

