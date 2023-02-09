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

clpu <- read.csv(paste0(lead, "CLPU_phyto-processing_20230206.csv"))

## basic cleaning function
source("data_cleaning/phyto-processing_data-cleaning/basic_cleaning_function.R")

## allometry data
source("allometry/merge_allometric_relationships.R")

## uniqueID key
source("data_cleaning/unique_key.R")


# Data Cleaning ####
clpuC <- basic_cleaning_func(clpu)


## Final Mods ####
clpu_final <- clpuC %>%
  
  ## add unique IDs in
  left_join(unique.key, by = c("treatment", "block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique")) %>% 
  
  filter(complete.sample == "Y") %>% ## remove incompletes
  
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g, scale.ID, process.notes, census.notes, unique.ID) ## select only needed columns



# Check Data ####
## look at total biomass
ggplot(clpu_final, aes(x=total.biomass.g)) +
  geom_histogram()
## no missing values

## look at phyto.n.indiv
ggplot(clpu_final, aes(x=phyto.n.indiv)) +
  geom_histogram()

# Check Notes ####
unique(clpu_final$process.notes)
## all seem good. There are likely phyto # changes that have not carried over to the phyto collections data sheet and will need to be made there.
    ## "changed to two phytos, few leaves"   
    ## "added row to spreadsheet (not previously listed); few leaves"

## create empty data frame
df <- data.frame()

## loop through all notes searching for "die" or "chang"
for(i in colnames(clpu_final)[13:14]) {
  tmp <- dplyr::filter(clpu_final, grepl("die", clpu_final[,i]))
  df <- rbind(df, tmp)
}

for(i in colnames(clpu_final)[13:14]) {
  tmp <- dplyr::filter(clpu_final, grepl("chang", clpu_final[,i]))
  df <- rbind(df, tmp)
}

# Check Uniques ####
unique(clpu_final$unique.ID)
na.check <- clpu_final %>%
  filter(is.na(unique.ID))
## 7 samples do NOT have phyto.uniques. Only one says it was added to the spreadsheet during processing.
    ## added one is 14-36-16 and in collections we have a note saying it died before collection, but someone must have collected it and forgotten to record this.

## ohh, these are ones that had their subplot changed during processing

## Add uniques back in ####
## fix sub plot so it matches collections data again
clpu_final[clpu_final$block == 1 & clpu_final$plot == 5,]$sub <- 22
clpu_final[clpu_final$block == 1 & clpu_final$plot == 5,]$unique.ID <- 122

clpu_final[clpu_final$block == 4 & clpu_final$plot == 13 & clpu_final$phyto.unique == "C",]$sub <- 15
clpu_final[clpu_final$block == 4 & clpu_final$plot == 13 & clpu_final$phyto.unique == "C",]$unique.ID <- 11962

clpu_final[clpu_final$block == 5 & clpu_final$plot == 1,]$sub <- 20
clpu_final[clpu_final$block == 5 & clpu_final$plot == 1,]$unique.ID <- 3146

clpu_final[clpu_final$block == 5 & clpu_final$plot == 10,]$sub <- 20
clpu_final[clpu_final$block == 5 & clpu_final$plot == 10 & clpu_final$phyto.unique == "B",]$unique.ID <- 11875

clpu_final[clpu_final$block == 8 & clpu_final$plot == 27,]$sub <- 24
clpu_final[clpu_final$block == 8 & clpu_final$plot == 27,]$unique.ID <- 6902

clpu_final[clpu_final$block == 12 & clpu_final$plot == 19,]$sub <- 17
clpu_final[clpu_final$block == 12 & clpu_final$plot == 19,]$unique.ID <- 7761

clpu_final[clpu_final$block == 14 & clpu_final$plot == 36,]$unique.ID <- 9227


# Make Phyto Dataframe ####
clpu.phyto <- clpu_final %>%
  mutate(phyto.seed.out = allo.df[allo.df$Species == "CLPU",2] + ## intercept
           (allo.df[allo.df$Species == "CLPU",5]*total.biomass.g) + ## slope 
           (allo.df[allo.df$Species == "CLPU", 8]*(total.biomass.g^2)), ## poly
         ## use tot.bio to seed.num
         
         phyto.seed.in = ifelse(!is.na(phyto.unique), phyto.n.indiv, 3),
         ## for phyto uniques, use the # indiv as the seeds.in, otherwise put 3 as the default
         
         phyto.seed.in = ifelse(phyto.n.indiv > 3, phyto.n.indiv, phyto.seed.in)) %>%
  ## then, check for # indiv > 3, use # indiv as seeds.in here also
  select(unique.ID, phyto, phyto.n.indiv, phyto.seed.in, phyto.seed.out)

# NOTE: ####
## there are probably several instances where the phyto collected was a recruit. We should check all collection notes for this possibility.

ggplot(clpu.phyto, aes(x=phyto.seed.out)) +
  geom_histogram()


## clean up env
rm(list = c("clpu", "clpu_final", "clpuC", "df", "tmp"))
