## removed the 5g sample from the allometric relationship as this was a big outlier and made the relatinoship fit worse for all other points
## when calculating seeds out, use the counted flowers for this particular sample


## Still need to adjust seeds in based on whether a sample was planted or a recruit

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

anar <- read.csv(paste0(lead, "ANAR_phyto-processing-redo_20221213.csv"))

## basic cleaning function
source("data_cleaning/phyto-processing_data-cleaning/basic_cleaning_function.R")

## allometry data
source("allometry/merge_allometric_relationships.R")

## uniqueID key
source("data_cleaning/unique_key.R")


# Clean Data ####
## Basic cleaning ####
anarC <- basic_cleaning_func(anar)

## need to add in unique.IDs here
anar_int <- left_join(anarC, unique.key, by = c("treatment", "block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique")) %>%
  mutate(unique.ID = unique.ID.y)

## 4-6-2 does not have a unique ID
#unique.key[unique.key$block == 4 & unique.key$plot == 6 & unique.key$sub == 2,]
  ## 1/10/23 downloaded the newest version of collections data onto dropbox. This should fix the issue.

#anar_int[anar_int$unique.ID == 3934, ]

## Final mods ####
anar_final <- anar_int %>%
  filter(complete.sample == "Y") %>%
  mutate(census.notes = notes) %>%
  
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, total.biomass.g, flower.num, scale.ID, process.notes, census.notes, unique.ID)
## select only needed columns


# Check Outliers ####
ggplot(anar_final, aes(x=total.biomass.g)) +
  geom_histogram()

ggplot(anar_final, aes(x=phyto.n.indiv)) +
  geom_histogram()

# Check Notes ####
unique(anar_final$process.notes)
## 2 notes that indicate a phyto might be missing but they made it through the data cleaning
## these are added to the data_cleaning_checks sheet now
#t <- anar_final[anar_final$process.notes == "missing, could be mislabeled", ]
#t2 <- anar_final[anar_final$process.notes == "Phyto missing 8/26", ]


## create empty data frame
df <- data.frame()

## loop through all notes searching for "die" or "chang"
for(i in colnames(anar_final)[13:14]) {
  tmp <- dplyr::filter(anar_final, grepl("die", anar_final[,i]))
  df <- rbind(df, tmp)
}

for(i in colnames(anar_final)[13:14]) {
  tmp <- dplyr::filter(anar_final, grepl("chang", anar_final[,i]))
  df <- rbind(df, tmp)
}
unique(df$census.notes)

## 12-23-18 is showing up twice on the notes df. Not sure why. Oh probably shows up in both for loops
## lots of notes but all thankfully show phyto # changes!! 

# Seeds in checks ####
#anar_final[anar_final$unique.ID == 3934, ] ## this sample not complete
anar_final[anar_final$unique.ID == 8138, ] ## this sample had 4 seeds in. If the planted phytos grew, need to replace the seeds in # for this.


## Not Planted ####
## these are indiv identified as not planted based on collection notes. Full list available in data-cleaning_checks spreadsheet
not_planted <- c(1069, 1369, 1618, 1803, 1828, 2086, 2311, 2336, 2361, 2561, 2586, 2811, 2836, 2986, 3684, 3734, 4084, 4195, 4470, 4545, 4996, 5071, 5298, 5323, 5398, 5473, 5548, 5699, 5824, 7337, 8113, 10113, 10188, 10611, 10711, 11581, 11711, 11788, 11789, 11801, 11819, 11840, 11842, 11848, 11854, 11856, 11857, 11865, 11869, 11871, 11872, 11873, 11874, 11880, 11881, 11882, 11884, 11890, 11895, 11896, 11897, 11898, 11899, 11900, 11901, 11902, 11904, 11905, 11906, 11907, 11908, 11909, 11911, 11912, 11913, 11915, 11916)

# Make Phyto DF ####
anar.phyto <- anar_final %>%
  mutate(ANAR.flowers.out = (allo.df[allo.df$Species == "ANAR",2] + ## intercept
                               (allo.df[allo.df$Species == "ANAR",5]*total.biomass.g) + ## slope
                               (allo.df[allo.df$Species == "ANAR",8]*total.biomass.g^2)), ## poly
         ## use tot.bio to flower.num to get flowers out
         
         ANAR.flowers.out = ifelse(total.biomass.g > 5, 419, ANAR.flowers.out), ## for the largest sample, put in flower # manually as this didn't work well with the allometric relationship
         
         phyto.seed.out = ifelse(treatment == "D",  allo.df[allo.df$Species == "ANAR",13]*ANAR.flowers.out,  allo.df[allo.df$Species == "ANAR",11]*ANAR.flowers.out),
         ## use avg seed num per trt to calculate seeds out
         
         phyto.seed.in = ifelse(!is.na(phyto.unique), phyto.n.indiv, 3),
         ## for phyto uniques, use the # indiv as the seeds.in, otherwise put 3 as the default
         
         phyto.seed.in = ifelse(phyto.n.indiv > 3, phyto.n.indiv, phyto.seed.in),
         ## then, check for # indiv > 3, use # indiv as seeds.in here also
         
         phyto.seed.in = ifelse(phyto.unique %in% not_planted, phyto.n.indiv/0.3, phyto.seed.in),
         ## if the phyto was not a planted indiv, calculate seeds in by: phyto.n.indiv/germ.rate
         ## choosing to use the warm temp, wet germ rate here as weedy ANAR seeds were present in the seedbank and must have experienced the warm, wet germination conditions that they preferentially germinate at
         
         phyto.seed.in = ifelse(unique.ID == 8138, 4, phyto.seed.in)
         ## 4 seeds were planted for this specific phyto
         ) %>%
  
  
  select(unique.ID, phyto, phyto.n.indiv, phyto.seed.in, phyto.seed.out)


ggplot(anar.phyto, aes(x=phyto.seed.out)) +
  geom_histogram()

rm(list = c("anar", "anar_final", "anar_int", "anarC", "df", "tmp"))