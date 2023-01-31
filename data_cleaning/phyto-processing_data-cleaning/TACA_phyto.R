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

taca <- read.csv(paste0(lead, "TACA_phyto-processing_20230119.csv"))

## basic cleaning function
source("data_cleaning/phyto-processing_data-cleaning/basic_cleaning_function.R")

## allometry data
source("allometry/merge_allometric_relationships.R")

## uniqueID key
source("data_cleaning/unique_key.R")



# Clean Data ####
tacaC <- basic_cleaning_func(taca)

str(tacaC)
## total.biomass.g is a character
## ahh looks like one entry has a comma rather than a period.

unique(tacaC$total.biomass.g)


tacaC[tacaC$total.biomass.g == "3,340" & tacaC$block == 12 & tacaC$plot == 28 & tacaC$sub == 22, ]$total.biomass.g <- 3.340
tacaC[tacaC$block == 12 & tacaC$plot == 28 & tacaC$sub == 22, ]
## looks good now

tacaC$total.biomass.g <- as.numeric(tacaC$total.biomass.g)

med_scales <- c("A", "E", "F", "G")  ## scales that need to be rounded

taca_final <- tacaC %>%
  filter(complete.sample == "Y") %>% ## remove incompletes
  
  left_join(unique.key, by = c("treatment", "block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique")) %>% 
  ## add in unique.ID nums
  
  mutate(total.biomass.g.rounded = ifelse(scale.ID %in% med_scales, round(total.biomass.g, digits = 3), total.biomass.g)) %>% ## round to 3 decimal places
  
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g.rounded, total.biomass.g, seed.num, scale.ID, process.notes, collect.notes, unique.ID) 

## Wow, we filtered out nearly 90 samples that were incomplete.



# Check Data ####
## look at total biomass
ggplot(taca_final, aes(x=total.biomass.g.rounded)) +
  geom_histogram()
## no missing values

## look at phyto.n.indiv
ggplot(taca_final, aes(x=phyto.n.indiv)) +
  geom_histogram()

# Check Notes ####
unique(taca_final$process.notes)
## several phyto number changes occurred at the processing stage

## create empty data frame
df <- data.frame()

## loop through all notes searching for "die" or "chang"
for(i in colnames(taca_final)[15:16]) {
  tmp <- dplyr::filter(taca_final, grepl("die", taca_final[,i]))
  df <- rbind(df, tmp)
}

for(i in colnames(taca_final)[15:16]) {
  tmp <- dplyr::filter(taca_final, grepl("chang", taca_final[,i]))
  df <- rbind(df, tmp)
}
## should probably chekc the 3 that changed during processing? otherwise the phyto num changes are well documented.


# Make Phyto DF ####
taca.phyto <- taca_final %>%
  mutate(phyto.seed.out = (allo.df[allo.df$Species == "TACA",2] + ## intercept
                             (allo.df[allo.df$Species == "TACA",5]*total.biomass.g.rounded) + ## slope
                             (allo.df[allo.df$Species == "TACA",8]*(total.biomass.g.rounded^2))), ## poly
         ## calc seed out from biomass weight & allo relationship
         
         phyto.seed.in = ifelse(!is.na(phyto.unique), phyto.n.indiv, 3),
         ## for phyto uniques, use the # indiv as the seeds.in, otherwise put 3 as the default
         
         phyto.seed.in = ifelse(phyto.n.indiv > 3, phyto.n.indiv, phyto.seed.in)) %>%
        ## then, check for # indiv > 3, use # indiv as seeds.in here also
  
  select(unique.ID, phyto, phyto.n.indiv, phyto.seed.in, phyto.seed.out)


## check the seed.in numbers
ggplot(taca.phyto, aes(x=phyto.n.indiv, y=phyto.seed.in)) +
  geom_point()

## clean up env
rm(list = c("taca", "taca_final", "tacaC", "df", "tmp"))
