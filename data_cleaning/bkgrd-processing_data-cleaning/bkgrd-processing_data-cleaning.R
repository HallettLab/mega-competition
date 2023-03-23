## Background Data Cleaning

## The purpose of this script is to: 
## 1. clean background data up
## 2. calculate avg. background individual
## 3. calculate avg background indiv seed output using allometric relationships

## Relevant Outputs of this script: 
## bg.seeds

# set up env
library(tidyverse)

# Read in Data ####
# specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Background-Processing/Background-Processing_entered/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Background-Processing/Background-Processing_entered/"
  
} else {
  # Marina
  lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Processing/Background-Processing/Background-Processing_entered/"
} 

## Background Data
bg_indiv <- read.csv(paste0(lead, "bkgrd-processing_20230315.csv"))
  
## WUE data for AMME & PLNO
wue <- read.csv(paste0(lead, "WUE_samples.csv"))

## Allometric Relationships
source("allometry/merge_allometric_relationships.R")


# Allo Types ####
## separate by types of allometric relationships
totbio.to.something <- c("ACAM", "AMME", "ANAR", "BRNI", "CLPU", "GITR", "LENI", "LOMU", "MICA", "PLNO", "TACA", "THIR", "TWIL")
inflor.bio.to.seeds <- c("BRHO", "PLER")
seeds.per.flower <- c("MAEL", "CESO")
seeds <- c("AVBA")

## make a vector of drought blocks
drought <- c(1, 3, 4, 6, 12, 14)


# Clean Data ####
## Basic cleaning ####
bg_indivC <- bg_indiv %>%
  mutate(bkgrd = ifelse(bkgrd == "THIR-I", "THIR", bkgrd),
         bkgrd = ifelse(bkgrd == "TWIL-I", "TWIL", bkgrd)) %>%
  ## fix species abbreviations to remove inoc sub-experiment
  filter(bkgrd != "THIR-U", bkgrd != "TWIL-U", bkgrd != "") %>%
  filter(plot < 43) %>% 
  ## remove inoc sub-experiment phytos and blank rows
  mutate(across(where(is.character), str_trim)) %>% ## remove leading & trailing whitespace!!
  mutate(across(c(scale.ID), toupper)) %>% ## capitalize scale ID
  mutate_all(na_if,"") %>% ## fill blanks with NAs
  select(-date.collect, -initials) %>%
  mutate(treatment = ifelse(block %in% drought, "D", "C")) ## add treatment column


unique(bg_indivC$bkgrd)
ggplot(bg_indivC, aes(x=bkgrd)) +
  geom_bar()
## CLPU has over 20 samples?
  
## Check structure ####
str(bg_indivC)
    ## flower.num is a chr
unique(bg_indivC$flower.num) ## one value is "F"

### Change Values ####
bg_indivC[bg_indivC$block == 16 & bg_indivC$plot == 17,]$flower.num <- NA
bg_indivC[bg_indivC$block == 7 & bg_indivC$plot == 11,]$flower.num <- NA
## change this flower.num val from "F" to NA

bg_indivC[bg_indivC$block == 7 & bg_indivC$plot == 11,]$process.notes <- "missing"

bg_indivC$flower.num <- as.numeric(bg_indivC$flower.num)

## Check completion ####
bio.rel <- bg_indivC %>% 
  filter(bkgrd %in% totbio.to.something)

ggplot(bio.rel, aes(x=total.biomass.g)) +
  geom_histogram() +
  facet_wrap(~bkgrd)

## 1 row missing - CLPU bkgrd 7-17
missing.check <- bio.rel %>%
  filter(is.na(total.biomass.g))

bio.rel[bio.rel$bkgrd == "CLPU",]
## OK, looks like there are 2 potential block 7 CLPU L dens plots - so it's not an issue that we are missing 7-17.
## should remove this in case this is what is causing some of the CLPU issues

#bg_indivC[bg_indivC$bkgrd == "CLPU" & bg_indivC$block == 7 & bg_indivC$plot == 17,]
#bg_indivC[bg_indivC$unique == 243,]

inflor.rel <- bg_indivC %>%
  filter(bkgrd %in% inflor.bio.to.seeds)

ggplot(inflor.rel, aes(x=inflor.g)) +
  geom_histogram() +
  facet_wrap(~bkgrd)
## no missing vals

seeds.rel <- bg_indivC %>%
  filter(bkgrd %in% seeds.per.flower)

ggplot(seeds.rel, aes(x=flower.num)) +
  geom_histogram() +
  facet_wrap(~bkgrd)
## missing 1 and this must be the CESO bkgrd

## Check notes ####
unique(bg_indivC$process.notes)
unique(bg_indivC$census.notes)
## THIR - need to take into account the TINC indiv present... somehow...
## WUE leaf weights...
## Vegetative vs flowering indiv for several species
    ## GITR
    ## ANAR - but these notes didn't get carried over


with.notes <- bg_indivC %>%
  filter(!is.na(census.notes))

### Calc Prop THIR ####
THIR.temp <- bg_indivC %>%
  filter(bkgrd == "THIR") %>%
  mutate(THIR.cov.temp = substr(census.notes, 1, 13),
         THIR.cov = as.numeric(gsub("([0-9]+).*$", "\\1", THIR.cov.temp)), 
         TINC.cov.temp = substr(census.notes, 14, 27),
         TINC.cov = as.numeric(gsub("([0-9]+).*$", "\\1", TINC.cov.temp))) %>%
  mutate(THIR.cov2 = ifelse(str_detect(THIR.cov.temp, "TINC") == TRUE, TINC.cov, THIR.cov),
         TINC.cov2 = ifelse(str_detect(TINC.cov.temp, "TH") == TRUE, THIR.cov, TINC.cov))
## extract info in notes to get %cov of THIR and TINC in separate cols

## fix a few that don't follow the same pattern
THIR.temp[THIR.temp$unique == 137,]$THIR.cov2 <- 75
THIR.temp[THIR.temp$unique == 101,]$THIR.cov2 <- 55

## finish calcs
THIR.temp <- THIR.temp %>%
  mutate(tot.thir = THIR.cov2 + TINC.cov2,
         THIR.prop = THIR.cov2/tot.thir) ## calc the prop THIR

### Add in WUE weights ####
#### clean wue data 
wueC <- wue %>%
  mutate(bkgrd = species,
         block = Block,
         wue.g = total.mg/1000)


AMME_PLNO <- left_join(bg_indivC, wueC[,c(3,11:14)], by = c("block", "plot", "bkgrd")) %>%
  filter(bkgrd == "AMME" | bkgrd == "PLNO", !is.na(wue.g)) %>%
  mutate(tot.bio = ifelse(str_detect(census.notes, "WUE")== TRUE, total.biomass.g + wue.g, total.biomass.g))

bg_indivC2 <- left_join(bg_indivC, AMME_PLNO[,c(1:3,18)], by = c("block", "plot", "bkgrd")) %>%
  mutate(total.biomass.g = ifelse(!is.na(tot.bio), tot.bio, total.biomass.g))



# Calc Avg Indiv ####
bg.ind <- bg_indivC2 %>%
  filter(unique != 243) %>% ## remove missing extra CLPU bkgrd
  mutate(avg.ind = ifelse(bkgrd %in% totbio.to.something, total.biomass.g/n.indiv, NA),
         avg.ind = ifelse(bkgrd %in% inflor.bio.to.seeds, inflor.g/n.indiv, avg.ind),
         avg.ind = ifelse(bkgrd %in% seeds.per.flower, flower.num/n.indiv, avg.ind),
         avg.ind = ifelse(bkgrd %in% seeds, glume.num/n.indiv, avg.ind))
## Calc the avg bg indiv, use the most appropriate measurement for each species
  
  
# Calc Avg Seed Output ####
## make a vector of species
bg.sp <- unique(bg.ind$bkgrd)


## separate totbio to something relationships
totbio.to.flowers.to.seeds <- c("ACAM", "AMME", "ANAR", "BRNI", "GITR", "LENI", "PLNO")
totbio.to.seeds <- c("CLPU", "LOMU", "MICA", "TACA")
totbio.to.flowers.to.viability <- c("THIR", "TWIL")


## make an empty dataframe for the output
bg.ind.avg <- data.frame(treatment = c(), block = c(), plot = c(), bkgrd = c(), dens = c(), bg.avg.seed.num = c())

## for each unique background species
for (i in 1:length(bg.sp)){
  
  ## filter species
  tmp.sp <- bg.ind[bg.ind$bkgrd == bg.sp[i],]
  
  ## filter model results
  tmp.model <- allo.df[allo.df$Species == bg.sp[i],]
  
  ## separate by allo-rel type and calc avg seeds out per indiv
  if (bg.sp[i] %in% totbio.to.seeds | bg.sp[i] %in% inflor.bio.to.seeds) {
    
    tmp.ind <- tmp.sp %>%
      mutate(bg.avg.seed.num = (tmp.model$intercept + ((tmp.model$slope)*avg.ind + (tmp.model$poly*(avg.ind^2))))) %>%
      select(treatment, block, plot, bkgrd, dens, bg.avg.seed.num)
    
  }
  
  else if (bg.sp[i] %in% totbio.to.flowers.to.seeds) {
    
    tmp.ind <- tmp.sp %>%
      mutate(avg.flower.num = (tmp.model$intercept + ((tmp.model$slope)*avg.ind) + (tmp.model$poly*(avg.ind^2))),
             bg.avg.seed.num = ifelse(treatment == "D", avg.flower.num*tmp.model$seeds_D, avg.flower.num*tmp.model$seeds_C)) %>%
      select(treatment, block, plot, bkgrd, dens, bg.avg.seed.num)

  }
  
  else if (bg.sp[i] %in% totbio.to.flowers.to.viability) {
    
    tmp.ind <- tmp.sp %>%
      mutate(avg.flower.num = (tmp.model$intercept + ((tmp.model$slope)*avg.ind) + (tmp.model$poly*(avg.ind^2))),
             avg.via.num = ifelse(treatment == "D", avg.flower.num*tmp.model$viability_D, avg.flower.num*tmp.model$viability_C),
             bg.avg.seed.num = ifelse(treatment == "D", avg.via.num*tmp.model$seeds_D, avg.via.num*tmp.model$seeds_C)) %>%
      select(treatment, block, plot, bkgrd, dens, bg.avg.seed.num)
    
  }
  
  else if (bg.sp[i] %in% seeds.per.flower) {
    
    tmp.ind <- tmp.sp %>%
      mutate(bg.avg.seed.num = ifelse(treatment == "D", avg.ind*tmp.model$seeds_D, avg.ind*tmp.model$seeds_C)) %>%
      select(treatment, block, plot, bkgrd, dens, bg.avg.seed.num)
    
  }
  
  else {
    
    tmp.ind <- tmp.sp %>%
      mutate(bg.avg.seed.num = avg.ind*2) %>%
      select(treatment, block, plot, bkgrd, dens, bg.avg.seed.num)
    ## currently specific to AVBA, which has counted glume #
    
  }
  
  bg.ind.avg <- rbind(bg.ind.avg, tmp.ind)
  
}

bg.seeds <- left_join(bg.ind.avg, THIR.temp[, c(1:4, 23)], by = c("block", "plot", "bkgrd", "dens"))
## merge THIR prop calcs back in so that it carries through to the next script

## Test CESO Bgs ####
ceso.bgs <- bg.seeds %>%
  filter(bkgrd == "CESO")

ceso.block.model <- aov(bg.avg.seed.num~block, data = ceso.bgs)
summary(ceso.block.model)
## block has a marginally signif effect on avg seeds (p=0.0858)
ceso.dens.model <- aov(bg.avg.seed.num~dens, data = ceso.bgs)
summary(ceso.dens.model)
## no signif effect of density

## therefore, will substitute block 7 plot 26 avg indiv seeds out for the missing block 7 plot 11

bg.seeds[bg.seeds$block == 7 & bg.seeds$bkgrd == "CESO", ]

### Fix CESO 7-11 ####
bg.seeds[bg.seeds$block == 7 & bg.seeds$plot == 11 & bg.seeds$bkgrd == "CESO", ]$bg.avg.seed.num <- 695.175

# Clean Env ####
rm(bg_indiv, bg.ind, bg.sp, bio.rel, inflor.rel, seeds.rel, missing.check, with.notes, seeds, tmp.ind, tmp.sp, tmp.model, totbio.to.seeds, inflor.bio.to.seeds, bg.ind.avg, bg_indivC, drought, i, seeds.per.flower, totbio.to.flowers.to.seeds, totbio.to.flowers.to.viability, totbio.to.something, basic_cleaning_func, THIR.temp, bg_indivC2, wue, wueC, AMME_PLNO, ceso.bgs, ceso.block.model, ceso.dens.model)

