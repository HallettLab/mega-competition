# Load packages ####
library(tidyverse)

# Read in Data ####
## phyto-processing data
source("data_cleaning/phyto-processing_data-cleaning/basic-cleaning_all-phytos.R")

## allometry data
source("allometry/merge_allometric_relationships.R")


# Final Cleaning ####
med_scales <- c("A", "E", "F", "G")  ## scales that need to be rounded

gitr_final <- gitrC %>%
  filter(complete.sample == "Y") %>% ## remove incompletes
  
  mutate(total.biomass.g.rounded = ifelse(scale.ID %in% med_scales, round(total.biomass.g, digits = 3), total.biomass.g)) %>% ## round to 3 decimal places
  
  select(treatment, block, plot, sub, bkgrd, dens, phyto, phyto.n.indiv, phyto.unique, complete.sample, total.biomass.g.rounded, flower.num, scale.ID, process.notes, census.notes, unique.ID) ## select only needed columns


# Check for Outliers ####
## look at inflor.g
ggplot(gitr_final, aes(x=total.biomass.g.rounded)) +
  geom_histogram()
## no missing vals

ggplot(gitr_final, aes(x=phyto.n.indiv)) +
  geom_histogram()



# Check Notes ####
## this might eventually be best in the census script?
unique(gitr_final$process.notes) ## okay

## create empty data frame
df <- data.frame()

## loop through all notes searching for "die" or "chang"
for(i in colnames(gitr_final)[14:15]) {
  tmp <- dplyr::filter(gitr_final, grepl("die", gitr_final[,i]))
  df <- rbind(df, tmp)
}

for(i in colnames(gitr_final)[14:15]) {
  tmp <- dplyr::filter(gitr_final, grepl("chang", gitr_final[,i]))
  df <- rbind(df, tmp)
}

## phyto change notes: ####
    ## Unique ID 2605 -> needs an intra-phyto
    ## Unique ID 5115 -> needs an intra-phyto


    ## Unique ID 5015 -> Original phyto number was 1, JD changed to 3 and updated census; ML edited notes in drive master, no intra-phyto changes needed
    ## Unique ID 11444: Originally phyto number was 1, later changed to 2; ML edited note in drive master, no intra-phyto changes needed
    ## Unique ID 4940: originally phyto number was 2, JD changed to 3 and is thus correct; edited notes  in drive master


## Phyto checks; 
## unique 4514; unique 8670
gitr_checks <- gitr_final %>%
  filter(unique.ID == 4514 | unique.ID == 8670)


# Make Phyto DF ####

## make avg seed numbers into vectors
seeds.D <- as.numeric(gitr_seed_means[gitr_seed_means$treatment == "D",2])
seeds.C <- as.numeric(gitr_seed_means[gitr_seed_means$treatment == "C",2])


gitr.phyto <- gitr_final %>%
  mutate(GITR.flowers.out = (allo.df[allo.df$species == "GITR",2] + 
                            (allo.df[allo.df$species == "GITR",3]*total.biomass.g.rounded) + (allo.df[allo.df$species == "GITR",4]*(total.biomass.g.rounded^2))),
         ## use tot.bio to flower.num to get flowers out
         
         phyto.seed.out = ifelse(treatment == "D",  seeds.D*GITR.flowers.out,  seeds.C*GITR.flowers.out),
         ## use avg seed num per trt to calculate seeds out
           
         phyto.seed.in = ifelse(!is.na(phyto.unique), phyto.n.indiv, 3),
         ## for phyto uniques, use the # indiv as the seeds.in, otherwise put 3 as the default
         
         phyto.seed.in = ifelse(phyto.n.indiv > 3, phyto.n.indiv, phyto.seed.in)) %>%
        ## then, check for # indiv > 3, use # indiv as seeds.in here also
  
  select(unique.ID, phyto, phyto.n.indiv, phyto.seed.in, phyto.seed.out)


## check the seed.in numbers
ggplot(gitr.phyto, aes(x=phyto.n.indiv, y=phyto.seed.in)) +
  geom_point()
## looks good!

## check seed.out numbers
ggplot(gitr.phyto, aes(x=phyto.seed.out)) +
  geom_histogram()

#ggplot(gitr.phyto, aes(y=phyto.seed.out, x=bkgrd, color = treatment)) +
#  geom_boxplot()