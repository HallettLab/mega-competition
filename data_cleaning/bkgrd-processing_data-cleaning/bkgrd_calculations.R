

# Read in Data ####

## average seed output for a background individual
source("data_cleaning/bkgrd-processing_data-cleaning/bkgrd-processing_data-cleaning.R")

## germination data
source("data_cleaning/bkgrd-processing_data-cleaning/germination_rates.R")

## collections data (for bkgrd.n.indiv)
source("data_cleaning/phyto-collections_data-cleaning/phyto-collections_data-cleaning.R")


bkgrd.n.indiv <- collectionsC %>%
  select(unique.ID, block, plot, bkgrd, bkgrd.n.indiv)

bkgrd.test <- left_join(bkgrd.n.indiv, bg.germ, by = "bkgrd")
bkgrd.test2 <- left_join(bkgrd.test, bg.seeds, by = c("block", "plot", "bkgrd"))

bkgrd.seeds <- bkgrd.test2 %>%
  mutate(bg.seeds.in = bkgrd.n.indiv/avg.germ,
         bg.seeds.out = bkgrd.n.indiv*bg.avg.seed.num) %>%
  select(unique.ID, bg.seeds.in, bg.seeds.out, bkgrd)

## calculate background seeds in
    ## calculate by: seeds.in * germination.rate = stems.out
    ## we will use ONLY the wet germination rates for these calculations as we did not apply our drought treatment until after germinating rains

## calculate background seeds out
    ## avg seed per indiv * stems out = seeds.out


