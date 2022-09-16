
# Read in Data ####
## Clean processing data
source("data_cleaning/summer_phytos_standardization.R")

## Collections data
lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/" # Carmen's file path
date_collections <- 20220825

collections <- read.xlsx(paste0(lead, "Collections/Collections_merged/", date_collections, "_MASTER_Collections_2-in-progress.xlsx"), sheet = 2)

## make sure the dates read in correctly
collections$phyto.date.collect <- as.Date(collections$phyto.date.collect, origin = "1899-12-30")
collections$phyto.date.census <- as.Date(collections$phyto.date.census, origin = "1899-12-30")
collections$bg.date.census <- as.Date(collections$bg.date.census, origin = "1899-12-30")
collections$phyto.unique <- as.character(collections$phyto.unique)

# Clean Collections Data ####

## all of the vectors to eventually filter by
Cs <- c("c", "c ", "C ") ## phyto.uniques
As <- c("a", "A ") ## phyto.uniques
summer_phytos <- unique(proc_dat_clean$phyto)
nhood10 <- c("MICA", "PLER", "BRHO", "ANAR", "GITR", "ACAM") ## neighborhood size
nhood18 <- c("LENI", "TWIL-I", "AVBA", "THIR-I") ## neighborhood size

collectionsC <- collections %>%
  filter(phyto %in% summer_phytos) %>% ## filter out only the fully processed phytos
  filter(plot < 43, bkgrd != "VIVI", phyto.n.indiv > 0) %>%
  mutate(phyto.unique = ifelse(phyto.unique %in% As, "A", ## change all values to caps
                               ifelse(phyto.unique == "b","B", 
                                      ifelse(phyto.unique %in% Cs, "C", phyto.unique)))) %>%
  mutate(unique.ID = unique) %>% ## standardize column name
  mutate(bkgrd.n.indiv = ifelse(bkgrd == "Control", NA, bkgrd.n.indiv)) %>% ## change # of background indiv in controls to NA
  mutate(Nbrhood.size = ifelse(phyto %in% nhood10, 10, ## fill in all vals of neighborhood size
                               ifelse(phyto %in% nhood18, 18, Nbrhood.size)),
         bkgrd = ifelse(bkgrd == "BRNI ", "BRNI",
                        ifelse(bkgrd == "THIR-I ", "THIR-I",
                               ifelse(bkgrd == "TWIL-I ", "TWIL-I", 
                                      ifelse(bkgrd == "CLPU ", "CLPU", bkgrd)))))



# Merge the two together ####
## Check for unaligned rows ####
processing_rejects <- rbind(incompletes_to_check, nas_to_check)
#duplicated(processing_rejects)

unique(collectionsC$phyto.unique)
unique(proc_dat_clean$phyto.unique)
unique(collectionsC$phyto)
unique(proc_dat_clean$phyto)
unique(collectionsC$bkgrd)
unique(proc_dat_clean$bkgrd)
## looks like there are differences in backgrounds that are causing these issues, particularly the collections backgrounds
## Fixed this above and it's helped

sort(unique(collectionsC$sub))
sort(unique(proc_dat_clean$sub))

unique(collectionsC$dens)
unique(proc_dat_clean$dens)

sort(unique(collectionsC$block))
sort(unique(proc_dat_clean$block))

sort(unique(collectionsC$plot))
sort(unique(proc_dat_clean$plot))



unmatched_in_collections <- anti_join(collectionsC, proc_dat_clean, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))
## 348 observations that are in collections but not the processing data
  

unmatched_not_reject <- anti_join(unmatched, processing_rejects, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))
## 2 are present in the unmatched dataframe that are not in the incomplete samples
    ## ANAR 4-6-2 A
    ## AVBA 16-18-21-" "

unique(unmatched_not_reject$phyto.unique)
## the AVBA has a space in phyto.unique
## the ANAR has an A in phyto.unique in census data but not in the processing data


reject_not_unmatched <- anti_join(processing_rejects, unmatched, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))
## 4 are present in the incomplete samples that are not in the unmatched dataframe
## These shouldn't matter currently because they are incomplete
    ## AVBA 16-18-21-NA
    ## THIR-I 16-27-4-B
    ## the 2 BRHO phytos that were gopher casualties


unmatched_in_processing <- anti_join(proc_dat_clean, collectionsC, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))
## 4 samples are in the processing dataframe but not the collections dataframe
  ## ACAM 8-22-4
  ## ANAR 4-6-2-NA ## This appears to be a problem with a phyto.unique
  ## THIR-I 14-15-15-B
  ## THIR-I 15-39-13-C


## Fix 1 identified issue ####
proc_dat_clean[proc_dat_clean$block == 4 & proc_dat_clean$plot == 6 & proc_dat_clean$sub == 2,]
collectionsC[collectionsC$block == 4 & collectionsC$plot == 6 & collectionsC$sub == 2,]

collections_filtered <- collectionsC %>%
  filter(!(block %in% processing_rejects$block && plot %in% processing_rejects$plot && sub %in% processing_rejects$sub && phyto.unique %in% processing_rejects$phyto.unique))


## Merge ####
cen_proc_all <- left_join(proc_dat_clean, collectionsC, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))

test <- left_join(proc_dat_clean, collectionsC, by = c("block", "plot", "sub", "phyto.unique"))

ggplot(cen_proc_all, aes(x=phyto.n.indiv.x, y=phyto.n.indiv.y)) +
  geom_point()

phyton.nas <- cen_proc_all %>%
  filter(is.na(phyto.n.indiv.x))

phyton.nas2 <- cen_proc_all %>%
  filter(is.na(phyto.n.indiv.y))

no_census <- cen_proc_all %>%
  filter(is.na(unique.ID.y))
## 4 observations did not end up with census data after the merge...
no_census_test <- test %>%
  filter(is.na(unique.ID.y))


