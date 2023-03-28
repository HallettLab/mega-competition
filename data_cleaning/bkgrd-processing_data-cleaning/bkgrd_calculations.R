## Background Calculations Script

## The goal of this script is to calculate the seeds in/seeds out data of each background subplot. It uses data from background processing, germination, background censuses, and seeding dates to back calculate seeds in by germ.rate*stem.count and seeds out by stem.count*avg.seeds.per.indiv. 

# Read in Data ####

## average seed output for a background individual
source("data_cleaning/bkgrd-processing_data-cleaning/bkgrd-processing_data-cleaning.R")

## germination data
source("data_cleaning/germination_data-cleaning/germination_rates.R")

## collections data (for bkgrd.n.indiv)
source("data_cleaning/phyto-collections_data-cleaning/phyto-collections_data-cleaning.R")

## seeding dates
source("data_cleaning/bkgrd-processing_data-cleaning/bkgrd_seeding_dates.R")


#test <- anti_join(plot.dates, bg.seeds, by = c("block", "plot", "bkgrd"))
## plot.dates has 398 rows, bg.seeds has 396; this happened because block 7 has 3 CLPU bgs and block 6 has 3 THIR bgs. One of each of these was not sampled for bg individuals.

# Combine DFs ####

## Dates & Germ Rates ####
## First, combine plot seeding dates & germ info to eval which germ rate to use for each plot
dates.germ <- left_join(plot.dates, bg.germ, by = c("bkgrd")) %>%
  mutate(bp.combo = paste(block, plot, sep = "_"))

## create a vector of unique block-plot combos
block.plots <- unique(dates.germ$bp.combo)

## Eval germ rates
## create an empty dataframe
germ.eval <- data.frame()

for(i in 1:length(block.plots)) {
  
  ## select a unique block-plot combo
  tmp.plot <- dates.germ %>%
    filter(bp.combo == block.plots[i])
  
  ## check if it's a temp sensitive species (BRNI, CLPU)
  if(unique(tmp.plot$bkgrd == "BRNI") | unique(tmp.plot$bkgrd == "CLPU")) {
        ## removed ANAR as temp sensitive species
    
    ## if it's temp sensitive check the planting date of a plot
    if (unique(tmp.plot$date) < "2021-11-10") { ## if planted on 11/9 or before
      tmp.germ <- tmp.plot %>%
        filter(Temp == 20) %>% ## use warm germ rate
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
    
  germ.eval <- rbind(germ.eval, tmp.germ)
      
  }


## Merge avg seed data ####
## merge the avg germ rates with the avg seed per indiv data
bg.info <- left_join(germ.eval, bg.seeds, by = c("block", "plot", "bkgrd")) %>%
  select(block, plot, bkgrd, germ.calc, bg.avg.seed.num, THIR.prop)

bkgrd.n.indiv <- bkgrd.n.indiv %>% 
  mutate(bkgrd = ifelse(bkgrd == "THIR-I", "THIR", bkgrd), 
         bkgrd = ifelse(bkgrd == "TWIL-I", "TWIL", bkgrd))

## Merge BG indiv data ####
bkgrd.calc <- left_join(bkgrd.n.indiv, bg.info, by = c("block", "plot", "bkgrd"))



# Calc Seeds ####
bkgrd.seeds <- bkgrd.calc %>%
  mutate(bg.seeds.in = bkgrd.n.indiv/germ.calc,
         bg.seeds.in = ifelse(bkgrd == "THIR", bg.seeds.in*THIR.prop, bg.seeds.in),
         ## account for TINC by reducing seeds in number by the prop of TINC present
         bg.seeds.out = bkgrd.n.indiv*bg.avg.seed.num) %>%
  select(unique.ID, bg.seeds.in, bg.seeds.out, bkgrd)

## calculate background seeds in
    ## calculate by: seeds.in * germination.rate = stems.out
    ## we will use ONLY the wet germination rates for these calculations as we did not apply our drought treatment until after germinating rains

## calculate background seeds out
    ## avg seed per indiv * stems out = seeds.out

# Clean Env ####
rm(list = c("bkgrd.calc", "bkgrd.n.indiv", "bg.germ", "bg.info", "bg.seeds", "dates.germ", "germ.eval"))