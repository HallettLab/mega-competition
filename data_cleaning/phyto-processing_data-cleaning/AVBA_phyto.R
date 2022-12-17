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

avba <- read.csv(paste0(lead, "AVBA_phyto-processing_20221018.csv")) %>%
  mutate(scale.ID = NA)

## basic cleaning function
source("data_cleaning/phyto-processing_data-cleaning/basic_cleaning_function.R")

## uniqueID key
source("data_cleaning/unique_key.R")

# Initial Clean ####
avbaC <- basic_cleaning_func(avba)

# Initial Exploration ####
ggplot(avbaC, aes(x=glume.num)) +
  geom_histogram()

ggplot(avbaC, aes(x=seed.num)) +
  geom_histogram()

ggplot(avbaC, aes(x=complete.sample)) +
  geom_bar()

avbaC[is.na(avbaC$complete.sample),]
## missing 1 sample

unique(avbaC$census.notes)
unique(avbaC$process.notes)

# avbaC[avbaC$process.notes == "phyto diseaed",] ## this is still marked complete, looks OK


# Final Cleaning ####
## need to add in unique.IDs here
avba_int <- left_join(avbaC, unique.key, by = c("treatment", "block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique")) %>%
  mutate(unique.ID = unique.ID.y)


avba_final <- avba_int %>%
  filter(complete.sample == "Y") %>%
  ## remove incompletes
  
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, glume.num, seed.num, scale.ID, process.notes, census.notes, unique.ID)
  

# Check Notes ####
unique(avba_final$process.notes)
unique(avba_final$census.notes)

## create empty data frame
df <- data.frame()

## loop through all notes searching for "die" or "chang"
for(i in colnames(avba_final)[14:15]) {
  tmp <- dplyr::filter(avba_final, grepl("die", avba_final[,i]))
  df <- rbind(df, tmp)
}

for(i in colnames(avba_final)[14:15]) {
  tmp <- dplyr::filter(avba_final, grepl("chang", avba_final[,i]))
  df <- rbind(df, tmp)
}


# Check for Outliers ####
ggplot(avba_final, aes(x=glume.num, y=seed.num)) +
  geom_point()
ggplot(avba_final, aes(x=seed.num)) +
  geom_histogram()
## 1 very large sample relative to the others, maybe check this?

ggplot(avba_final, aes(x=phyto.unique)) +
  geom_bar()

## check the # of phyto uniques to make sure it aligns with the # of seeds.in less than 3
avbaC[!is.na(avbaC$phyto.unique),]


# Make Phyto DF ####
avba.phyto <- avba_final %>%
  mutate(phyto.seed.out = seed.num, 
         phyto.seed.in = ifelse(!is.na(phyto.unique), phyto.n.indiv, 3),
         phyto.seed.in = ifelse(phyto.n.indiv > 3, phyto.n.indiv, phyto.seed.in)) %>%
  select(unique.ID, phyto, phyto.n.indiv, phyto.seed.in, phyto.seed.out)


## check the seed.in numbers
ggplot(avba.phyto, aes(x=phyto.n.indiv, y=phyto.seed.in)) +
  geom_point()
## looks good!

## clean up env
rm(list = c("avba", "avbaC", "avba_final", "avba_int", "df", "tmp"))