# Load packages ####
library(tidyverse)

# Read in Data ####
## phyto-processing data
## specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/"
  
} else {
  # Marina
  lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_entered/"
} 

brni <- read.csv(paste0(lead, "BRNI_phyto-processing_20230315.csv"))

## basic cleaning function
source("data_cleaning/phyto-processing_data-cleaning/basic_cleaning_function.R")

## allometry data
source("allometry/merge_allometric_relationships.R")

## uniqueID key
source("data_cleaning/unique_key.R")


# Clean Data ####

## fix the lost sample before cleaning the data...
brni[brni$block == 6 & brni$plot == 9 & brni$sub == 7,]$bkgrd <- "GITR"
brni[brni$block == 6 & brni$plot == 9 & brni$sub == 7,]$unique <- 4375
  
  
brniC <- basic_cleaning_func(brni)
## FIXED 
  ## we lost one sample here.
  ## this is cutting out a row that was added in later on and does not have a bkgrd or a phyto unique. It shouldn't get cut out, but it is... 

## Explore Complete Column
## during allo rel I noticed that one complete.sample was incorrect based on the phenology and leaves data collected - sort through these before assuming these are correct

ggplot(brniC, aes(x=complete.sample, fill = seeds.missing)) +
  geom_bar() +
  facet_wrap(~leaves.present)

## temp mods
brniC2 <- brniC %>%
  
  ## change N/A to NA
  mutate(seeds.missing = ifelse(seeds.missing == "N/A", NA, seeds.missing)) %>%
  
  ## fix veg & flowers rows of pheno column - should have seeds.missing = NA
  mutate(seeds.missing = ifelse(phenology == "Veg", NA, seeds.missing),
         seeds.missing= ifelse(phenology == "Flowers", NA, seeds.missing)) %>%
  
  ## fix complete.sample
  mutate(complete.sample2 = ifelse(seeds.missing == "Y", "N", complete.sample))
    ## somehow this column is causing NAs for Flower samples...
         
         #, ## if seeds missing, it is incomplete
         
         #complete.sample2 = ifelse(phenology == "Veg" & leaves.present != "None", "Y", complete.sample), ## if veg and has few to many leaves, it is complete
         
       #  complete.sample2 = ifelse(phenology == "Flowers" & leaves.present != "None", "Y", complete.sample) ## if flowers only and has few to many leaves, it is complete
         
         
 # )

seeds <- brniC2 %>%
  filter(phenology == "Seeds")

ggplot(seeds, aes(x=complete.sample2, fill = seeds.missing)) +
  geom_bar() +
  facet_wrap(~leaves.present)
## for the seeds pheno
    ## if seeds missing - should be incomplete

#seeds.missing.but.complete <- brniC %>%
  #filter(phenology == "Seeds", seeds.missing == "Y", complete.sample == "Y")
## added these 3 to data cleaning checks on 3/17/23 - waiting for follow up


flowers <- brniC2 %>%
  filter(phenology == "Flowers")
## added to TBD samples to data cleaning checks on 3/17/23 - waiting for follow-up
ggplot(flowers, aes(x=complete.sample)) +
  geom_bar() +
  facet_wrap(~leaves.present)


veg <- brniC2 %>%
  filter(phenology == "Veg")
ggplot(veg, aes(x=complete.sample)) +
  geom_bar() +
  facet_wrap(~leaves.present)


na.pheno <- brniC %>%
  filter(is.na(phenology))
## the 2 phytos missing phenology are definitely incomplete


incompletes <- brniC %>%
  filter(complete.sample != "Y")


brni_final <- brniC %>%
  
  ## change N/A to NA
  mutate(seeds.missing = ifelse(seeds.missing == "N/A", NA, seeds.missing)) %>%
  
  ## fix veg rows of pheno column - should have seeds.missing = NA
  mutate(seeds.missing = ifelse(phenology == "Veg" | phenology == "Flowers", NA, seeds.missing)) %>%
  
  ## fix complete.sample
  #mutate(complete.sample2 = ifelse(seeds.missing == "Y", "N", complete.sample), ## if seeds missing, it is incomplete
         
         #complete.sample2 = ifelse(phenology == "Veg" & leaves.present != "None", "Y", complete.sample), ## if veg and has few to many leaves, it is complete
         
        # complete.sample2 = ifelse(phenology == "Flowers" & leaves.present != "None", "Y", complete.sample) ## if flowers only and has few to many leaves, it is complete
 
  filter(complete.sample == "Y") %>% ## remove incompletes
  
  left_join(unique.key, by = c("treatment", "block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique")) %>% 
  ## add in unique.ID nums
  
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, phenology, seeds.missing, leaves.present, total.biomass.g, scale.ID, process.notes, collect.notes, unique.ID) 



  
  
  
ggplot(brni_final, aes(x=total.biomass.g)) +
    geom_histogram()
  
## check process notes
unique(brni_final$process.notes)


## create empty data frame
df <- data.frame()

## loop through all notes searching for "die" or "chang"
for(i in colnames(brni_final)[16:17]) {
  tmp <- dplyr::filter(brni_final, grepl("die", brni_final[,i]))
  df <- rbind(df, tmp)
}

for(i in colnames(brni_final)[16:17]) {
  tmp <- dplyr::filter(brni_final, grepl("chang", brni_final[,i]))
  df <- rbind(df, tmp)
}
## looks good

# Make Phyto DF ####
brni.phyto <- brni_final %>%
  mutate(total.viable.flowers.out = (allo.df[allo.df$Species == "BRNI",5]*total.biomass.g), ## slope
         ## calc total viable flowers out from biomass
         
         phyto.seed.out = ifelse(treatment == "D",  
                                 allo.df[allo.df$Species == "BRNI",10]*total.viable.flowers.out,  
                                 allo.df[allo.df$Species == "BRNI",8]*total.viable.flowers.out),
         
         phyto.seed.in = ifelse(!is.na(phyto.unique), phyto.n.indiv, 3),
         ## for phyto uniques, use the # indiv as the seeds.in, otherwise put 3 as the default
         
         phyto.seed.in = ifelse(phyto.n.indiv > 3, phyto.n.indiv, phyto.seed.in)) %>%
  ## then, check for # indiv > 3, use # indiv as seeds.in here also
  
  select(unique.ID, phyto, phyto.n.indiv, phyto.seed.in, phyto.seed.out)



rm(list = c("brni", "brni_final", "brniC", "brniC2", "df", "drought", "flowers", "i", "incompletes", "lead", "na.pheno", "seeds", "tmp", "veg"))
