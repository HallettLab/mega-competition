
## Read in Data
## phyto seeds in/out
source("data_cleaning/phyto-processing_data-cleaning/compile_phyto-processing_data.R")

## bkgrd seeds in/out
source("data_cleaning/bkgrd-processing_data-cleaning/bkgrd_calculations.R")

## get rid of extraneous col
all.phytos <- all.phytos 

bg.phyto.seeds <- left_join(all.phytos, bkgrd.seeds, by = "unique.ID") %>%
  select(-phyto.n.indiv)
## want trt info here
## select one phyto, separate col for every species
## fill with 0s unless bg and phyto match, then we hv data in those cols

## keep all trt, block, plot, sub info in df
bg.phyto.seeds2 <- left_join(bg.phyto.seeds, unique.key, by = c("unique.ID", "phyto", "bkgrd"))

bg.phyto.seeds2 <- bg.phyto.seeds2[,c(1, 8:11,13,2,7,12, 3:6)] # reorder

wide <- bg.phyto.seeds2 %>%
  filter(phyto == "BRHO") %>% ## filter by specific phytometer
  mutate(bkgrd.names.in = paste0(bkgrd, ".seeds.in")) %>%
  pivot_wider(names_from = "bkgrd.names.in", values_from = "bg.seeds.in", values_fill = 0) %>% 
  mutate(BRHO.seeds.in = ifelse(bkgrd == "BRHO", BRHO.seeds.in, phyto.seed.in), ## when bkgrd is BRHO, put bkgrd seeds in
         BRHO.seeds.out = ifelse(bkgrd == "BRHO" & phyto == "BRHO", phyto.seed.out + bg.seeds.out, phyto.seed.out)) %>% ## for intra phytos, add phyto & bg seeds out values
  select(-Control.seeds.in, -NA.seeds.in, -phyto.seed.in, -phyto.seed.out, -bg.seeds.out) ## drop extraneous cols

wide <- wide[, c(1:9, 28, 10:25)] ## reorder

## intraspecific phytometer case: Seeds out: add phyto + bkgrd seeds out; need to add an extra stem into the seeds.in calculation because the intra phyto stem is not counted in the bkgrd.n.indiv census and we back calculated seeds.in from there. 

