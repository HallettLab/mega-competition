## Cleaning all processing data together

## load packages
library(tidyverse)

# Read in Data ####
lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/" # Carmen's file path

lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/" #Marina's file path

date_processing <- 20221018
date_collections <- 20220825

## Processing data
acam <- read.csv(paste0(lead, "ACAM_phyto-processing_", date_processing, ".csv"))
anar <- read.csv(paste0(lead, "ANAR_phyto-processing_", date_processing, ".csv"))
avba <- read.csv(paste0(lead, "AVBA_phyto-processing_", date_processing, ".csv"))
brho <- read.csv(paste0(lead, "BRHO_phyto-processing_", date_processing, ".csv"))
gitr <- read.csv(paste0(lead, "GITR_phyto-processing_", date_processing, ".csv"))
leni <- read.csv(paste0(lead, "LENI_phyto-processing_", date_processing, ".csv"))
lomu <- read.csv(paste0(lead, "LOMU_phyto-processing_", date_processing, ".csv"))
mica <- read.csv(paste0(lead, "MICA_phyto-processing_", date_processing, ".csv"))
pler <- read.csv(paste0(lead, "PLER_phyto-processing_", date_processing, ".csv"))
thir <- read.csv(paste0(lead, "THIR_phyto-processing_", date_processing, ".csv"))
twil <- read.csv(paste0(lead, "TWIL_phyto-processing_", date_processing, ".csv"))


drought <- c(1, 3, 4, 6, 12, 14) ## create treatment vector

# Round 1 ####
## Standardize column names

## Standardize ACAM ####
colnames(acam)
acamC <- acam %>%
  mutate(complete.sample = complete.,
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
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, pod.num, glume.num, seeds.present, scale.ID, process.notes, census.notes,unique.ID) ## make sure column ordering works

## Standardize ANAR ####
colnames(anar)
anarC <- anar %>%
  mutate(complete.sample = complete.,
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
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, pod.num, glume.num, seeds.present, scale.ID, process.notes, census.notes,unique.ID) ## make sure column ordering works


### add in anar data ####
tempanar <- rbind(acamC, anarC)

## Standardize AVBA ####
colnames(avba)
avbaC <- avba %>%
  mutate(complete.sample = complete.,
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
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, pod.num, glume.num, seeds.present, scale.ID, process.notes, census.notes,unique.ID) ## make sure column ordering works

### add in avba data ####
tempavba <- rbind(tempanar, avbaC)


## Standardize BRHO ####
colnames(brho)
brhoC <- brho %>%
  mutate(complete.sample = complete.,
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
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, pod.num, glume.num, seeds.present, scale.ID, process.notes, census.notes,unique.ID) ## make sure column ordering works


### add in brho data ####
tempbrho <- rbind(tempavba, brhoC)

## Standardize GITR ####
colnames(gitr)
gitrC <- gitr %>%
  mutate(complete.sample = complete.,
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
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, pod.num, glume.num, seeds.present, scale.ID, process.notes, census.notes,unique.ID) ## make sure column ordering works

### add in gitr data ####
tempgitr <- rbind(tempbrho, gitrC)

## Standardize MICA ####
colnames(mica)
micaC <- mica %>%
  mutate(complete.sample = complete.,
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
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, pod.num, glume.num, seeds.present, scale.ID, process.notes, census.notes,unique.ID) ## make sure column ordering works

### add in mica data ####
tempmica <- rbind(tempgitr, micaC)


## Standardize LENI ####
colnames(leni)
leniC <- leni %>%
  mutate(complete.sample = complete.,
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
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, pod.num, glume.num, seeds.present, scale.ID, process.notes, census.notes,unique.ID) ## make sure column ordering works



### add in leni data ####
templeni <- rbind(tempmica, leniC)


## Standardize PLER ####
colnames(pler)
plerC <- pler %>%
  mutate(complete.sample = complete.,
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
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, pod.num, glume.num, seeds.present, scale.ID, process.notes, census.notes,unique.ID) ## make sure column ordering works


### add in pler data ####
temppler <- rbind(templeni, plerC)


## Standardize THIR ####
colnames(thir)
thirC <- thir %>%
  mutate(complete.sample = complete.,
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
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, pod.num, glume.num, seeds.present, scale.ID, process.notes, census.notes,unique.ID) ## make sure column ordering works

### add in thir data ####
tempthir <- rbind(temppler, thirC)


## Standardize TWIL ####
colnames(twil)
twilC <- twil %>%
  mutate(complete.sample = complete.,
         unique.ID = unique,
         biomass.no.seed.g = NA, 
         #total.biomass.g = NA,
         flower.num = NA,
         inflor.g = NA, 
         #seed.num = NA,
         pod.num = NA,
         total.stem.length.mm = NA, 
         empty.flower.num = NA,
         #seeds.present = NA,
         glume.num = NA) %>% 
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, pod.num, glume.num, seeds.present, scale.ID, process.notes, census.notes,unique.ID) ## make sure column ordering works

### add in twil data ####
temptwil <- rbind(tempthir, twilC)

## Standardize LOMU ####
colnames(lomu)
lomuC <- lomu %>%
  mutate(complete.sample = complete.,
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
  select(block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, biomass.no.seed.g, inflor.g, seed.num, flower.num, total.stem.length.mm, empty.flower.num, pod.num, glume.num, seeds.present, scale.ID, process.notes, census.notes,unique.ID) ## make sure column ordering works

### add in lomu data ####
round1_final <- rbind(temptwil, lomuC)

# Round 2 ####
## Mods ####
round2 <- round1_final %>%
  mutate(across(where(is.character), str_trim)) %>% ## remove leading & trailing whitespace!!
  mutate(treatment = ifelse(block %in% drought, "D", "C"), ## add a treatment column
         phyto.unique = ifelse(phyto.unique == "a", "A", ## change all values to caps
                               ifelse(phyto.unique == "b","B",
                                      ifelse(phyto.unique == "c", "C", phyto.unique))),
         complete.sample = ifelse(complete.sample == "y", "Y", ## change all values to caps
                                  ifelse(complete.sample == "n", "N", complete.sample)),
         scale.ID = ifelse(scale.ID == "e", "E", ## change all values to caps
                           ifelse(scale.ID == "a","A", 
                                  ifelse(scale.ID == "d", "D", scale.ID)))) %>%
  filter(plot < 43, bkgrd != "VIVI")
           

## Checks ####
ggplot(round2, aes(x=phyto.unique)) +
  geom_bar()
unique(round2$phyto.unique)
## Need to standardize, further, lots of values of "" that should be changed to NA

ggplot(round2, aes(x=scale.ID)) +
  geom_bar()
## Need to standardize, further, lots of values of "" that should be changed to NA
## values of N/A that need to change to NA
unique(round2$scale.ID)

ggplot(round2, aes(x=complete.sample)) +
  geom_bar()
## there's one that's not filled in still? Yep a "" value
unique(round2$complete.sample)

ggplot(round2, aes(x=bkgrd)) +
  geom_bar()

## More Mods ####
round2_final <- round2 %>%
  mutate(phyto.unique = ifelse(phyto.unique == "", NA, phyto.unique),
         scale.ID = ifelse(scale.ID == "", NA, 
                           ifelse(scale.ID == "N/A", NA, scale.ID)), 
         dens = ifelse(dens == "", NA, dens), 
         complete.sample = ifelse(complete.sample == "", NA, complete.sample))
                  
ggplot(round2_final, aes(x=phyto.unique)) +
  geom_bar()

ggplot(round2_final, aes(x=scale.ID)) +
  geom_bar()

ggplot(round2_final, aes(x=complete.sample)) +
  geom_bar()
## fixed!


# Round 3 ####
str(round2_final)
## Fix non-numeric cols ####
## Initial issues that arose during this step: 
    ## inflor.g is a character
    ## flower.num is a character
    ## update, flower.num is now numeric, so leave this column alone now

## filter out the non-numeric observations that are messing up inflor.g and flower.num
non_nums_inflor <- round2_final %>%
  filter(!is.na(inflor.g)) %>%
  filter(!(!is.na(as.numeric(inflor.g)))) ## lol I don't quite know why this works.... but it did separate out the problematic observation!
## 7-15-8; BRHO phyto with inflor.g "1.6847g"
## also lots of BRHO missing vals

round2_final[round2_final$block == 7 & round2_final$plot == 15 & round2_final$sub == 8, ]$inflor.g <- 1.6847

## change the column to numeric
round2_final$inflor.g <- as.numeric(round2_final$inflor.g)

#non_nums_flower <- round2_final %>%
  #filter(!is.na(flower.num)) %>%
  #filter(!(!is.na(as.numeric(flower.num))))
## nothing shows up, think there used to be a space here, change back to numeric

## change the column to numeric
#round2_final$flower.num <- as.numeric(round2_final$flower.num)



## Filter, Round, & PerCap ####
med_scales <- c("A", "E", "F", "G") ## scales that need to be rounded

## round weights that were taken using the medium scales as scale A goes to 4 decimal places while scales E, F, and G go to 3
## we are rounding to 3 decimal places overall 
round3 <- round2_final %>%
  filter(complete.sample == "Y") %>% ## remove incomplete samples
  mutate(total.biomass.g.rounded = ifelse(scale.ID %in% med_scales, round(total.biomass.g, digits = 3), total.biomass.g),
         inflor.g.rounded = ifelse(scale.ID %in% med_scales, round(inflor.g, digits = 3), inflor.g)) %>% ## round to 3 decimal places
  mutate(total.biomass.rounded.percap = total.biomass.g.rounded/phyto.n.indiv,
         inflor.g.rounded.percap = inflor.g.rounded/phyto.n.indiv,
         seed.num.percap = seed.num/phyto.n.indiv,
         flower.num.percap = flower.num/phyto.n.indiv,
         total.stem.length.mm.percap = total.stem.length.mm/phyto.n.indiv,
         empty.flower.num.percap = empty.flower.num/phyto.n.indiv,
         pod.num.percap = pod.num/phyto.n.indiv,
         glume.num.percap = glume.num/phyto.n.indiv) ## put everything in per-capita

## Samples to Check ####        
incompletes_to_check <- round2_final %>%
  filter(complete.sample == "N")
nrow(incompletes_to_check)

nas_to_check <- round2_final %>%
  filter(is.na(complete.sample))
nrow(nas_to_check)
## seems like many of these are missing samples.

## 386 samples removed for completion issues. 

proc_dat_clean <- round3

## clean up environment
rm(list = c("acam", "acamC", "anar", "anarC", "avba", "avbaC", "brho", "brhoC",  "date_collections", "drought", "gitr", "gitrC", "lead", "leni", "leniC", "med_scales",  "mica", "micaC", "non_nums_flower", "non_nums_inflor", "pler", "plerC", "round2", "tempanar", "tempavba", "tempbrho", "tempgitr", "templeni", "tempmica", "temppler", "tempthir", "thir", "thirC", "twil", "twilC", "temptwil", "lomu", "lomuC", "round1_final", "round2_final", "round3"))
