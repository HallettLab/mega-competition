# Load packages ####
library(tidyverse)
theme_set(theme_bw())

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

mica <- read.csv(paste0(lead, "MICA_phyto-processing_20230119.csv"))

## basic cleaning function
source("data_cleaning/phyto-processing_data-cleaning/basic_cleaning_function.R")

## allometry data
source("allometry/merge_allometric_relationships.R")

## uniqueID key
source("data_cleaning/unique_key.R")


# Basic Cleaning ####
micaC <- basic_cleaning_func(mica)

## Follow up on spot checks ####
#mica_int[mica_int$unique.ID == 8028,]
## 12-31-8
#mica_int[mica_int$unique.ID == 2042,]
## 3-41-24
#mica_final[mica_final$unique.ID == 7502,]
#mica_final[mica_final$unique.ID == 9130,]
#mica_final[mica_final$unique.ID == 10722,]

med_scales <- c("A", "E", "F", "G")  ## scales that need to be rounded

mica_final <- micaC %>%
  filter(complete.sample == "Y") %>% ## remove incompletes
  
  left_join(unique.key, by = c("treatment", "block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique")) %>% 
  ## add in unique ID numbers
  
  mutate(total.biomass.g.rounded = ifelse(scale.ID %in% med_scales, round(total.biomass.g, digits = 3), total.biomass.g)) %>% ## round to 3 decimal places
  
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g.rounded, total.biomass.g, seed.num, scale.ID, process.notes, census.notes, unique.ID) 


# Check Data ####
## look at total biomass
ggplot(mica_final, aes(x=total.biomass.g.rounded)) +
  geom_histogram()
## no rows removed

#check <- mica_final %>%
  #filter(is.na(total.biomass.g.rounded))
## already in our data cleaning checks - will follow up on


## look at phyto.n.indiv
ggplot(mica_final, aes(x=phyto.n.indiv)) +
  geom_histogram()

# Check Notes ####
unique(mica_final$process.notes)
## looks good

## create empty data frame
df <- data.frame()

## loop through all notes searching for "die" or "chang"
for(i in colnames(mica_final)[15:16]) {
  tmp <- dplyr::filter(mica_final, grepl("die", mica_final[,i]))
  df <- rbind(df, tmp)
}

for(i in colnames(mica_final)[15:16]) {
  tmp <- dplyr::filter(mica_final, grepl("chang", mica_final[,i]))
  df <- rbind(df, tmp)
}
## looks good

# Make Phyto DF ####
mica.phyto <- mica_final %>%
  mutate(phyto.seed.out = (allo.df[allo.df$Species == "MICA",2] + ## intercept
                             (allo.df[allo.df$Species == "MICA",5]*total.biomass.g.rounded) + ## slope
                             (allo.df[allo.df$Species == "MICA",8]*(total.biomass.g.rounded^2))), ## poly
         ## calc seed out from biomass weight & allo relationship
         
         phyto.seed.in = ifelse(!is.na(phyto.unique), phyto.n.indiv, 3),
         ## for phyto uniques, use the # indiv as the seeds.in, otherwise put 3 as the default
         
         phyto.seed.in = ifelse(phyto.n.indiv > 3, phyto.n.indiv, phyto.seed.in)) %>%
        ## then, check for # indiv > 3, use # indiv as seeds.in here also
  
  select(unique.ID, phyto, phyto.n.indiv, phyto.seed.in, phyto.seed.out)


## check the seed.in numbers
ggplot(mica.phyto, aes(x=phyto.n.indiv, y=phyto.seed.in)) +
  geom_point()

## clean up env
rm(list = c("mica", "mica_final", "micaC", "df", "tmp", "check"))
