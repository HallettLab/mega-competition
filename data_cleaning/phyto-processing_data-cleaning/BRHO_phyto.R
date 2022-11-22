# Load packages ####
library(tidyverse)

# Read in Data ####
## phyto-processing data
source("data_cleaning/phyto-processing_data-cleaning/basic-cleaning_all-phytos.R")

## allometry data
source("allometry/merge_allometric_relationships.R")


# Final Cleaning ####
med_scales <- c("A", "E", "F", "G")  ## scales that need to be rounded

brho_final <- brhoC %>%
  filter(complete.sample == "Y") %>% ## remove incompletes
  
  mutate(inflor.g.rounded = ifelse(scale.ID %in% med_scales, round(inflor.g, digits = 3), inflor.g)) %>% ## round to 3 decimal places
  
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, inflor.g.rounded, total.biomass.g, seed.num, scale.ID, process.notes, census.notes, unique.ID) ## select only needed columns

 
# Check for Outliers ####
  ## look at inflor.g
ggplot(brho_final, aes(x=inflor.g.rounded)) +
    geom_histogram()
  ## no missing vals

ggplot(brho_final, aes(x=phyto.n.indiv)) +
    geom_histogram()



# Check Notes ####
## create empty data frame
df <- data.frame()

## loop through all notes searching for "die" or "chang"
for(i in colnames(brho_final)[14:15]) {
  tmp <- dplyr::filter(brho_final, grepl("die", brho_final[,i]))
  df <- rbind(df, tmp)
}

for(i in colnames(brho_final)[14:15]) {
  tmp <- dplyr::filter(brho_final, grepl("chang", brho_final[,i]))
  df <- rbind(df, tmp)
}

## the phyto change notes are not super descriptive but they all contain 'CORRECTED' so they are probably okay?


# Make Phyto DF ####
brho.phyto <- brho_final %>%
  mutate(BRHO.seed.out = (allo.df[allo.df$species == "BRHO",2] + 
                            (allo.df[allo.df$species == "BRHO",3]*inflor.g.rounded)),
         BRHO.seed.in = ifelse(phyto.n.indiv > 3, phyto.n.indiv, 3)) %>%
  select(unique.ID, phyto.n.indiv, BRHO.seed.in, BRHO.seed.out)


## check the seed.in numbers
ggplot(brho.phyto, aes(x=phyto.n.indiv, y=BRHO.seed.in)) +
  geom_point()
  ## looks good!