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

mael <- read.csv(paste0(lead, "MAEL_phyto-processing_", "20221122", ".csv")) %>%
  mutate(scale.ID = NA)

## basic cleaning function
source("data_cleaning/phyto-processing_data-cleaning/basic_cleaning_function.R")

## allometry data
source("allometry/merge_allometric_relationships.R")

## background seeding dates
source("data_cleaning/bkgrd-processing_data-cleaning/bkgrd_seeding_dates.R")

# Final Cleaning ####
maelC <- basic_cleaning_func(mael)

mael_final <- maelC %>%
  filter(complete.sample == "Y") %>%
  mutate(census.notes = notes) %>%
  
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, total.biomass.g, flower.num, scale.ID, process.notes, census.notes, collect.notes, background.notes, unique.ID)
## select only needed columns

ggplot(mael_final, aes(x=flower.num)) +
  geom_histogram()
## one sample is quite a bit larger than the rest.

ggplot(mael_final, aes(x=phyto.n.indiv)) +
  geom_histogram()
## there are 2 samples with 4 phytos and another with 6?
## check these samples to see if this is the case (assuming we can tell. If only flowers were collected we won't be able to)
## we planted 10 seeds in most subplots, so it is possible.

unique(mael_final$process.notes)
## looks okay

## create empty data frame
df <- data.frame()

## loop through all notes searching for "die" or "chang"
for(i in colnames(mael_final)[13:16]) {
  tmp <- dplyr::filter(mael_final, grepl("die", mael_final[,i]))
  df <- rbind(df, tmp)
}

for(i in colnames(mael_final)[13:16]) {
  tmp <- dplyr::filter(mael_final, grepl("chang", mael_final[,i]))
  df <- rbind(df, tmp)
}

unique(df$collect.notes)

## Check planting dates to determine MAEL seeds in
    ## From wiki notes: "Block 14, plots 29-42 have 10 Madia seeds. Everything seeded after this point also has 10 Madia seeds/subplot."

plot.dates[plot.dates$block == 14,]
## anything seeded 11/10 or later has 10 MAEL seeds in and anything before should be 3.


## join planting dates DF with mael_final
mael_final_dates <- left_join(mael_final, plot.dates, by = c("block", "plot", "bkgrd"))


# Make Final DF ####
mael.phyto <- mael_final_dates %>%
  mutate(phyto.seed.out = ifelse(treatment == "D",  allo.df[allo.df$Species == "MAEL",13]*flower.num,  allo.df[allo.df$Species == "MAEL",11]*flower.num),
         ## use avg seed num per trt to calculate seeds out

         
         phyto.seed.in = ifelse(date > "2021-11-09", 10, 3),
         ## if date is later than 11/9 use 10 seeds in, if before 3 seeds in
         
         phyto.seed.in = ifelse(!is.na(phyto.unique), phyto.n.indiv, phyto.seed.in),
         ## for phyto uniques, use the # indiv as the seeds.in - not perfect where there were 10 seeds in...
         
         phyto.seed.in = ifelse(date < "2021-11-09" & phyto.n.indiv > 3, phyto.n.indiv, phyto.seed.in)) %>%
  ## then, check for # indiv > 3, use # indiv as seeds.in here also
  
  
  select(unique.ID, phyto, phyto.n.indiv, phyto.seed.in, phyto.seed.out)


#phyto.uniques.mael <- mael.phyto %>%
 # filter(!is.na(phyto.unique))

# NOT DONE ####
## still not sure what to do for phyto.uniques when 10 seeds were put in a sub plot.

ggplot(mael.phyto, aes(x=phyto.seed.out)) +
  geom_histogram()


## clean env
rm(list = c("mael", "maelC", "plot.dates", "tmp", "df", "mael_final", "mael_final_dates"))