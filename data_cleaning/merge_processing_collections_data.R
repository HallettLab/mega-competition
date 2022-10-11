# Load Packages ####
library(tidyverse)

# Read in Data ####
## Clean processing data
source("data_cleaning/phyto-processing_data-cleaning.R")

## Clean collections data
source("data_cleaning/phyto-collections_data-cleaning.R")


# Checks before Merge ####
processing_rejects <- rbind(incompletes_to_check, nas_to_check) ## combine all processing rejects
duplicated(processing_rejects)

## Check row nums ####
nrow(collectionsC) - nrow(proc_dat_clean)
## there are 347 more rows in collections than processing data
## there are 350 processing rejects, so somehow there are 3 samples unaccounted for currently



### Are processing and processing rejects separate? ####
    ## YES! 
unM_proc_and_rejects <- anti_join(proc_dat_clean, processing_rejects, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))
## There are 2959 here, same # of rows as the proc_dat_clean
## meaning there is no overlap between processing rejects and processing cleaned data.
## That's good.

try_the_opp <- anti_join(processing_rejects, proc_dat_clean,by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))
## yep 350 rows - so there are no matches in proc rejects and proc clean data.



### Collections should contain all the processing rejects rows? ####
    ## Yes, except for 2 gopher phytos
test <- semi_join(collectionsC, processing_rejects, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))
## semi_join -> returns the rows of the first table where it can find a match in the second table

test2 <- semi_join(processing_rejects, collectionsC, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))
## 348 matches when there should be 350

test3 <- anti_join(processing_rejects, collectionsC, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))
## just the gopher phytos - no worries

## old problem, fixed in phyto processing sheet
#unique(collectionsC$dens)
#unique(processing_rejects$dens) ## "" present here; collections data are ONLY "H", "L", NA

unM_coll_and_rejects <- anti_join(collectionsC, processing_rejects, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))
## There are 2958 rows here 
## 2958 + 348 = 3306, so [rows that don't match proc_rejects] + [rows that do match proc_rejects] add up to total



### Duplicates in Processing Data? ####
## 350 processing rejects removed from collections data
## 347 more rows in collections data than processing data


## so logically the mismatch in numbers should then be a row in the processing data that is NOT found in the censusing data?
## Need to isolate this somehow...

## first take out the processing rejects from the collectionsC data
## Then, the two dataframes SHOULD match up. But hopefully can isolate the one row that doesn't match...

coll_NO_rejects <- anti_join(collectionsC, processing_rejects, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))
## 2958 rows

## Now see what's diff between processing data and this dataframe
proc_coll_diff <- anti_join(proc_dat_clean, coll_NO_rejects, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))
## 4-6-2 ANAR shows up here (this has a discrepancy in phyto.unique that was found in an earlier version of this script)

## Fix phyto.unique mismatch in ANAR 4-6-2
## Seems like the phyto unique was removed from envelope & processing data but we forgot to remove it from the collections data so it is kicking up a fuss.
## Need to remove the A value from phyto unique in collections data
proc_dat_clean[proc_dat_clean$block == 4 & proc_dat_clean$plot == 6 & proc_dat_clean$sub == 2,]
collectionsC[collectionsC$block == 4 & collectionsC$plot == 6 & collectionsC$sub == 2,]$phyto.unique <- NA

## Find the duplicate present in proc_dat_clean (discovered in earlier version of the script)
proc_distinct <- proc_dat_clean %>%
  distinct_at(vars(block, plot, sub, bkgrd, dens, phyto, phyto.unique))
## Based on the ID variables I am filtering it does seem like there is a repeated value since there are only 2958 distinct values out of the 2959 rows in the clean processing data

## Make one ID column and filter for duplicates in this
temp_proc <- proc_dat_clean %>%
  mutate(sample.ID = paste(block, plot, sub, bkgrd, dens, phyto, phyto.unique, sep = "_"))

not_unique <- temp_proc %>%
  filter(duplicated(sample.ID))
## 7_34_11_PLNO_L_ACAM_NA

proc_dat_clean[proc_dat_clean$block == 7 & proc_dat_clean$plot == 34 & proc_dat_clean$sub == 11,]
## One duplicate ACAM sample. Unclear if this is two samples without phyto.uniques or if the same sample was weighed twice.
## Need to physically check samples for this.

## Temp Solution ####
## use the distinct function to remove one of the values for this sample
## later determine which to use and go back to fix it.

proc_dat_distinct <- proc_dat_clean %>%
  distinct_at(vars(block, plot, sub, bkgrd, dens, phyto, phyto.unique), .keep_all = TRUE)

nrow(collectionsC) - nrow(proc_dat_distinct)
## 348 differences between these
## 348 processing rejects (not including gopher BRHOs)





## Check rows in coll NOT proc ####
unmatched_in_collections <- anti_join(collectionsC, proc_dat_clean, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))
## 348 observations that are in collections but not the processing data
## need to investigate these rows, they should align well with the 350 processing rejects

unmatched_in_collections2 <- anti_join(collectionsC, proc_dat_distinct, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))
## yep, still 348


### Unmatched but NOT proc rejects ####
unmatched_not_reject <- anti_join(unmatched_in_collections, processing_rejects, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))
## nothing left here


### Proc rejects but NOT unmatched ####
reject_not_unmatched <- anti_join(processing_rejects, unmatched_in_collections, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))
## ONLY 2 LEFT AND THESE ARE GOPHER CASUALTIES


## Check rows in proc NOT coll ####
unmatched_in_processing <- anti_join(proc_dat_clean, collectionsC, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))
## NOTHING LEFT HERE!


# Merge ####
cen_proc_all <- left_join(proc_dat_distinct, collectionsC, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))

## Checks ####
ggplot(cen_proc_all, aes(x=phyto.n.indiv.x, y=phyto.n.indiv.y)) +
  geom_point()
## all align without removing any rows. Phew.

ggplot(cen_proc_all, aes(x=unique.ID.x, y=unique.ID.y)) +
  geom_point()
## these all align but there are a lot of missing vals (makes sense - a lot were not included on the processing data sheets)

## Final Merged Data Frame ####
all_dat_final <- cen_proc_all %>%
  mutate(phyto.n.indiv = phyto.n.indiv.x,
         unique.ID = unique.ID.y) %>%
  select(-phyto.n.indiv.y, -phyto.n.indiv.x, -unique.ID.y, -unique.ID.x)

# Clean up Environment ####
rm(list = c("cen_proc_all", "coll_NO_rejects", "collections", "collectionsC", "date_collections", "incompletes_to_check", "lead", "nas_to_check", "nhood10", "nhood18", "proc_coll_diff", "proc_dat_clean", "proc_dat_distinct", "proc_distinct",  "reject_not_unmatched", "summer_phytos", "temp_proc", "test", "test2", "unM_coll_and_rejects", "unM_proc_and_rejects", "unmatched_in_collections", "unmatched_in_collections2", "unmatched_in_processing", "unmatched_not_reject", "not_unique", "test3", "try_the_opp"))
