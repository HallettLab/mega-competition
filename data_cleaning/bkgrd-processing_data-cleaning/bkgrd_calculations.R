# Read in Data ####

## average seed output for a background individual
source("data_cleaning/bkgrd-processing_data-cleaning/bkgrd-processing_data-cleaning.R")

## germination data
source("data_cleaning/bkgrd-processing_data-cleaning/germination_rates.R")

## collections data (for bkgrd.n.indiv)
source("data_cleaning/phyto-collections_data-cleaning/phyto-collections_data-cleaning.R")

## seeding dates
source("data_cleaning/bkgrd-processing_data-cleaning/bkgrd_seeding_dates.R")




# Combine info ####

## Plot dates & Germ rates ####

## First, plot.dates & germ info to quickly eval which germ rate to use
dates.germ <- left_join(plot.dates, bg.germ, by = c("bkgrd")) %>%
  mutate(bp.combo = paste(block, plot, sep = "_"))

block.plots <- unique(dates.germ$bp.combo)

df1 <- data.frame()

for(i in 1:length(block.plots)) {
  
  tmp.plot <- dates.germ %>%
    filter(bp.combo == block.plots[i])
  
  if(unique(tmp.plot$bkgrd == "ANAR") | unique(tmp.plot$bkgrd == "BRNI") | unique(tmp.plot$bkgrd == "CLPU")) {
    
    if (unique(tmp.plot$date) < "2021-11-10") { ## if planted 11/9 or before
      tmp.germ <- tmp.plot %>%
        filter(Temp == 20) %>%
        mutate(germ.calc = avg.germ)
      
    } else { ## if planted after 11/9, use cold germ rate
      
      tmp.germ <- tmp.plot %>%
        filter(Temp == 10) %>%
        mutate(germ.calc = avg.germ)
    }
    
   } else{ ## for every other species just use the average
      tmp.germ <- tmp.plot %>%
        mutate(germ.calc = avg.germ)
      
    }
    
  df1 <- rbind(df1, tmp.germ)
      
  }
  

## Avg seed & dates/rates ####
bg.info <- left_join(df1, bg.seeds, by = c("block", "plot", "bkgrd")) %>%
  select(block, plot, bkgrd, germ.calc, bg.avg.seed.num)

test <- bg.info %>%
  filter(is.na(germ.calc))
## seems like we're missing germ rates for some species due to nomenclature differences b/w germ data & everything else

bkgrd.n.indiv <- collectionsC %>%
  select(unique.ID, block, plot, bkgrd, bkgrd.n.indiv)

bkgrd.calc <- left_join(bkgrd.n.indiv, bg.info, by = c("block", "plot", "bkgrd"))



# Calc Seeds ####
bkgrd.seeds <- bkgrd.calc %>%
  mutate(bg.seeds.in = bkgrd.n.indiv/germ.calc,
         bg.seeds.out = bkgrd.n.indiv*bg.avg.seed.num) %>%
  select(unique.ID, bg.seeds.in, bg.seeds.out, bkgrd)

## calculate background seeds in
    ## calculate by: seeds.in * germination.rate = stems.out
    ## we will use ONLY the wet germination rates for these calculations as we did not apply our drought treatment until after germinating rains

## calculate background seeds out
    ## avg seed per indiv * stems out = seeds.out


