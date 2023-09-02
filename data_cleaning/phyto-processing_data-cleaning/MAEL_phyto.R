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

## uniqueID key
source("data_cleaning/unique_key.R")

# Final Cleaning ####
maelC <- basic_cleaning_func(mael)
#maelC[maelC$unique.ID == 3928,]


mael_final <- maelC %>%
  filter(complete.sample == "Y") %>%
  
  left_join(unique.key, by = c("treatment", "block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique")) %>% 
  
  mutate(census.notes = notes) %>%
  
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, total.biomass.g, flower.num, scale.ID, process.notes, census.notes, collect.notes, background.notes, unique.ID)
## select only needed columns

# Check outliers ####
ggplot(mael_final, aes(x=flower.num)) +
  geom_histogram()
## one sample is quite a bit larger than the rest.

ggplot(mael_final, aes(x=phyto.n.indiv)) +
  geom_histogram()
## there are 2 samples with 4 phytos and another with 6?
## check these samples to see if this is the case (assuming we can tell. If only flowers were collected we won't be able to)
## we planted 10 seeds in most subplots, so it is possible.

# Check Notes ####
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


# Add planting dates ####
## join planting dates DF with mael_final
mael_final_dates <- left_join(mael_final, plot.dates, by = c("block", "plot", "bkgrd"))
    ## one caveat about dates - Control plots don't have a bg planting date as there is no planted background. The way we have done things below means that they don't get a phyto seeds in value

## 2817 planted 11-14
## 5574 & 12005 planted 11-11
## 7519 planted 11-10

## Fix Control planting dates ####
mael_final_dates[mael_final_dates$unique.ID == 2817,]$date <- "2021-11-14"
mael_final_dates[mael_final_dates$unique.ID == 5574,]$date <- "2021-11-11"
mael_final_dates[mael_final_dates$unique.ID == 12005,]$date <- "2021-11-11"
mael_final_dates[mael_final_dates$unique.ID == 7519,]$date <- "2021-11-10"
#mael_final_dates[mael_final_dates$unique.ID == 3928,]



# Make Final DF ####
mael.phyto <- mael_final_dates %>%
  mutate(phyto.seed.out = ifelse(treatment == "D",  
                                 allo.df[allo.df$Species == "MAEL",10]*flower.num,  
                                 allo.df[allo.df$Species == "MAEL",8]*flower.num),
         ## use avg seed num per trt to calculate seeds out

         
         phyto.seed.in = ifelse(date > "2021-11-09", 10, 3),
         ## if date is later than 11/9 use 10 seeds in, if before 3 seeds in
         
         phyto.seed.in = ifelse(!is.na(phyto.unique) & date > "2021-11-09", 5, phyto.seed.in),
         ## for phyto uniques planted with 10 seeds (after 11/9), split the difference and say 5 seeds in for each
         
         phyto.seed.in = ifelse(!is.na(phyto.unique) & date < "2021-11-09", phyto.n.indiv, phyto.seed.in),
        # for phyto uniques planted with 3 seeds (before 11/9), use the # indiv as the seeds.in
         
         phyto.seed.in = ifelse(date < "2021-11-09" & phyto.n.indiv > 3, phyto.n.indiv, phyto.seed.in)) %>%
  ## then, check for # indiv > 3, use # indiv as seeds.in here also
  
  
  select(unique.ID, treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.seed.in, phyto.seed.out)


#phyto.uniques.mael <- mael.phyto %>%
 # filter(!is.na(phyto.unique))

mael.check <- mael.phyto %>%
  filter(is.na(phyto.seed.in))
mael.controls <- mael_final_dates %>%
  filter(bkgrd == "Control")

mael.phyto[mael.phyto$unique.ID == 11367,]
mael_final_dates[mael_final_dates$unique.ID==11367,]
mael_final_dates[mael_final_dates$block == 16 & mael_final_dates$plot == 37 & mael_final_dates$sub == 22,]


mael.phyto[mael.phyto$unique.ID == 4346,]
mael.phyto[mael.phyto$unique.ID == 2942,]
## Make Mods ####
## Change seeds in for a pair of phyto.uniques
mael.phyto[mael.phyto$unique.ID == 11367,]$phyto.seed.in <- 6
mael.phyto[mael.phyto$unique.ID == 11922,]$phyto.seed.in <- 4

ggplot(mael.phyto, aes(x=phyto.seed.out)) +
  geom_histogram()

## clean env
rm(list = c("mael", "maelC", "plot.dates", "tmp", "df", "mael_final_dates", "mael.check", "mael.controls"))
