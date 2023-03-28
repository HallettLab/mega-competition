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

ceso <- read.csv(paste0(lead, "CESO_phyto-processing_20230118.csv")) %>%
  mutate(scale.ID = NA,
         `complete.` = NA)

## basic cleaning function
source("data_cleaning/phyto-processing_data-cleaning/basic_cleaning_function.R")

## allometry data
source("allometry/merge_allometric_relationships.R")

## unique ID key 
source("data_cleaning/unique_key.R")

#unique(ceso$flower.num)

# Final Cleaning ####
cesoC <- basic_cleaning_func(ceso)
str(cesoC)

#unique(cesoC$flower.num)

ceso_final <- cesoC %>%
  filter(!is.na(flower.num)) %>% ## get rid of missing sample
  ## we weren't able to assess whether a sample was complete, so we can't filter by this
  
  left_join(unique.key, by = c("treatment", "block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique")) %>% 
  
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, flower.num, process.notes, collect.notes, unique.ID)
## select only needed columns


# Check Data ####
ggplot(ceso_final, aes(x=flower.num)) +
  geom_histogram()
## one sample is missing a flower number, removed above.

ggplot(ceso_final, aes(x=phyto.n.indiv)) +
  geom_histogram()

# Check Notes ####
unique(ceso_final$process.notes)
## one sample missing

## create empty data frame
df <- data.frame()

## loop through all notes searching for "die" or "chang"
for(i in colnames(ceso_final)[11:12]) {
  tmp <- dplyr::filter(ceso_final, grepl("die", ceso_final[,i]))
  df <- rbind(df, tmp)
}

for(i in colnames(ceso_final)[11:12]) {
  tmp <- dplyr::filter(ceso_final, grepl("chang", ceso_final[,i]))
  df <- rbind(df, tmp)
}

unique(df$collect.notes)
unique(df$unique.ID)
## 2314 OK


# Make Final DF ####
ceso.phyto <- ceso_final %>%
  mutate(phyto.seed.out = ifelse(treatment == "D",  allo.df[allo.df$Species == "CESO",13]*flower.num,  allo.df[allo.df$Species == "CESO",11]*flower.num),
         
         phyto.seed.in = ifelse(!is.na(phyto.unique), phyto.n.indiv, 3),
         ## for phyto uniques, use the # indiv as the seeds.in, otherwise put 3 as the default
         
         phyto.seed.in = ifelse(phyto.n.indiv > 3, phyto.n.indiv, phyto.seed.in)) %>%
        ## then, check for # indiv > 3, use # indiv as seeds.in here also
  
  select(unique.ID, phyto, phyto.n.indiv, phyto.seed.in, phyto.seed.out)

ggplot(ceso.phyto, aes(x=phyto.seed.out)) +
  geom_histogram()
## nothing missing here!

ggplot(ceso.phyto, aes(x=phyto.seed.in)) +
  geom_histogram()

## clean up env
rm(list = c("ceso", "cesoC", "ceso_final", "df", "tmp"))
