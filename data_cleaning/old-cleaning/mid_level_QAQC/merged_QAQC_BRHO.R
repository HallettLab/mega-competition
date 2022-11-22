## Merged BRHO QAQC ##

## the purpose of this script is:
    ## 1. to do species specific QAQC
    ## 2. clean the census data (clean here rather than earlier so that only clean the necessary rows not everything)
    ## 3. get BRHO data modeling ready so that it can be fed into the compile_split_final script and made into a modeling ready data frame


# set up env
library(tidyverse)
theme_set(theme_bw())

# Read in Data ####
source("data_cleaning/initial_data_prep/merge_processing_collections_data.R")

## read in allometric relationships
source("allometry/merge_allometric_relationships.R")

brho  <- all_dat_final %>%
  filter(phyto == "BRHO")

# Clean Census Data ####
## Phyto Change Notes ####
    ## keywords
        ## die, chang

## create empty data frame
df <- data.frame()

## loop through all notes searching for "die" or "chang"
for(i in colnames(brho)[45:48]) {
  tmp <- dplyr::filter(brho, grepl("die", brho[,i]))
  df <- rbind(df, tmp)
}

for(i in colnames(brho)[45:48]) {
  tmp <- dplyr::filter(brho, grepl("chang", brho[,i]))
  df <- rbind(df, tmp)
}

## add an empty intraphyto column to  brho df
    ## if there were intraphytos that died before collection they would go in this column
brho$intraphyto <- 0 

# Check Cols for Oddballs ####
## look at infor.g
ggplot(brho, aes(x=inflor.g)) +
  geom_histogram()
## no missing vals

ggplot(brho, aes(x=bkgrd.n.indiv)) +
  geom_histogram() +
  facet_wrap(~bkgrd)
## 12 missing vals that are likely controls
ggplot(brho, aes(x=phyto.n.indiv)) +
  geom_histogram()

ggplot(brho, aes(x=CRCO)) +
  geom_histogram()
ggplot(brho, aes(x=ERBO)) + ## there's one with 8 ERBO?
  geom_histogram()
ggplot(brho, aes(x=FIGA)) +
  geom_histogram()
ggplot(brho, aes(x=GAMU)) +
  geom_histogram()
ggplot(brho, aes(x=HYGL)) +
  geom_histogram()
ggplot(brho, aes(x=SIGA)) +
  geom_histogram()
ggplot(brho, aes(x=other)) +
  geom_histogram()

# Q here ####
## are we modeling by per capita or not?
    ## NO. REMOVE. 
## are we doing anything to the other column?



# Make Phyto DF ####
brho.phyto <- brho %>%
  mutate(BRHO.seed.out = (allo.df[allo.df$species == "BRHO",2] + 
                            (allo.df[allo.df$species == "BRHO",3]*inflor.g.rounded.percap))*phyto.n.indiv,
         #bkgrd.seed.out = bkgrd.n.indiv*bg.avg.seed.num, 
         BRHO.seed.in = 3,
         #bkgrd.stem.in = bkgrd.n.indiv
  ) %>%
  select(unique.ID, phyto.n.indiv, brho.seed.in, brho.seed.out)



# Make Census DF ####
brho.census <- brho %>%
  select(unique.ID, phyto.n.indiv, Nbrhood.size, bkgrd.n.indiv, CRCO, ERBO, FIGA, GAMU, HYGL, SIGA, other)
