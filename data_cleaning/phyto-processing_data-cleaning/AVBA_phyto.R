# Load packages ####
library(tidyverse)

# Read in Data ####
## phyto-processing data
source("data_cleaning/phyto-processing_data-cleaning/basic-cleaning_all-phytos.R")

## allometry data
source("allometry/merge_allometric_relationships.R")

# Initial Exploration ####
ggplot(avbaC, aes(x=glume.num)) +
  geom_histogram()

ggplot(avbaC, aes(x=seed.num)) +
  geom_histogram()

ggplot(avbaC, aes(x=complete.sample)) +
  geom_bar()

avbaC[is.na(avbaC$complete.sample),]
## missing 1 sample


# Final Cleaning ####
avba_final <- avbaC %>%
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


# Make Phyto DF ####
avba.phyto <- avba_final %>%
  mutate(AVBA.seed.out = seed.num, 
         AVBA.seed.in = ifelse(phyto.n.indiv > 3, phyto.n.indiv, 3)) %>%
  select(unique.ID, phyto.n.indiv, AVBA.seed.in, AVBA.seed.out)


## check the seed.in numbers
ggplot(avba.phyto, aes(x=phyto.n.indiv, y=AVBA.seed.in)) +
  geom_point()
## looks good!