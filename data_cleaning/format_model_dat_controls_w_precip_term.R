## This script combines phyto and background seeds in/out and gets data in a model ready format! 

# Read in Data ####
## phyto seeds in/out ####
source("data_cleaning/phyto-processing_data-cleaning/compile_phyto-processing_data.R")

## bkgrd seeds in/out ####
source("data_cleaning/bkgrd-processing_data-cleaning/bkgrd_calculations.R")

## filter for controls
controls <- left_join(all.phytos, unique.key, by = c("unique.ID", "phyto")) %>%
  mutate(bkgrd = ifelse(bkgrd == "ERBO", "Control", bkgrd)) %>% ## set ERBO backgrounds as controls
  filter(bkgrd == "Control") %>%
  mutate(dens = "none", phyto2 = phyto)

## still need to pivot wider :(

#model.dat.init <- controls %>%
  #pivot_wider(names_from = phyto, values_from = phyto.seed.in, values_fill = 0)

## add in weed census
model.dat <- left_join(controls, phyto.census[,c(1,5:11)], by = "unique.ID") %>%
  mutate(treat = ifelse(treatment == "C", 1, 0),
         weeds = CRCO + ERBO + FIGA + GAMU + HYGL + SIGA + other) %>% ## lump all the weeds together
  select(-CRCO, -ERBO, -FIGA, -GAMU, -HYGL, -SIGA, -other)


# Make Lambda Priors df ####
lambda_priors_mean <- controls %>%
  group_by(phyto) %>%
  summarise(mean_seeds_ctrl = mean(phyto.seed.out), 
            sd_seeds = sd(phyto.seed.out))

# Clean Env
#rm()
