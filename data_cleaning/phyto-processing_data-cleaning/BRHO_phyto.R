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

brho <- read.csv(paste0(lead, "BRHO_phyto-processing_20221201", ".csv"))

## basic cleaning function
source("data_cleaning/phyto-processing_data-cleaning/basic_cleaning_function.R")

## allometry data
source("allometry/merge_allometric_relationships.R")


# Final Cleaning ####
brhoC <- basic_cleaning_func(brho)

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
for(i in colnames(brho_final)[15:16]) {
  tmp <- dplyr::filter(brho_final, grepl("die", brho_final[,i]))
  df <- rbind(df, tmp)
}

for(i in colnames(brho_final)[15:16]) {
  tmp <- dplyr::filter(brho_final, grepl("chang", brho_final[,i]))
  df <- rbind(df, tmp)
}

unique(df$census.notes)

## the phyto change notes are not super descriptive but they all contain 'CORRECTED' so they are probably okay?
## No, should look into a few of these further.

unique(brho_final$census.notes)
## potential note of interest: 
    ## "only one phytometer located & collected'
    ## CORRECTED. seeds already, re-census on 04/20/2022 to include additional phyto. Manuel updated needed.
    ## Other= ANAR, BRHO phyto with seed head snapped off
    ## CORRECTED; manually change, changed phyto # Jd
    ## THIR,one other BRHO in S.12. far enough away that I didn't count it
    ## Additional phyto collected. NO re-census, phyto within neighborhood.
    ## CORRECTED. re-census on 4/22/22. two phytos collected. There may be an additional envelop for this subplot.


unique(brho_final$process.notes)
## okay

## To-Do####

## need to look through the collections notes for any modifications that should be made to the seeds.in number - based on phyto.unique, whether it was planted or a recruit

## seeds in will also be influenced by the intra-phyto.


# Make Phyto DF ####
brho.phyto <- brho_final %>%
  mutate(phyto.seed.out = (allo.df[allo.df$Species == "BRHO",2] + 
                            (allo.df[allo.df$Species == "BRHO",5]*inflor.g.rounded)),
         
         phyto.seed.in = ifelse(!is.na(phyto.unique), phyto.n.indiv, 3),
         phyto.seed.in = ifelse(phyto.n.indiv > 3, phyto.n.indiv, phyto.seed.in)) %>%
  
  select(unique.ID, phyto, phyto.n.indiv, phyto.seed.in, phyto.seed.out)


## check the seed.in numbers
ggplot(brho.phyto, aes(x=phyto.n.indiv, y=phyto.seed.in)) +
  geom_point()
  ## looks good!
#ggplot(brho.phyto, aes(x=phyto.unique, y=phyto.seed.in)) +
 # geom_point()
  ## looks like there is a phyto.unique that has 3 seeds in, meaning that 3 phytos were found there. Probably okay, there must have been more seeds than 3 planted in that particular subplot, but since it's split into a phyto.unique we are using the phyto.n.indiv number for this.

## clean up env
rm(list = c("brho", "brho_final", "brhoC", "df", "tmp"))