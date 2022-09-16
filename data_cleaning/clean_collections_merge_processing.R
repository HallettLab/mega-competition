
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

## check for spaces in phyto names
unique(collections$phyto)

## check for spaces in background names
unique(collections$bkgrd)

## check for spaces in phyto.uniques
phyto.unique_spaces_collections <- collections %>%
  filter(phyto.unique == " ")
#view(phyto.unique_spaces_collections)

phyto.unique_spaces_proc <- proc_dat_clean %>%
  filter(phyto.unique == " ")
#view(phyto.unique_spaces_proc)
## there are 4 phyto.uniques with spaces in the clean processing data 
## these 4 are also in the collections data
## in addition, the collections data also contain 1 relevant phyto.unique space that should be fixed
collections[collections$block == 16 & collections$plot == 18 & collections$sub == 21,]$phyto.unique <- NA
    ## Fix this issue here


## Fix phyto.unique mismatch in ANAR 4-6-2
    ## Seems like the phyto unique was removed from envelope & processing data but we forgot to remove it from the collections data so it is kicking up a fuss.
    ## Need to remove the A value from phyto unique in collections data
proc_dat_clean[proc_dat_clean$block == 4 & proc_dat_clean$plot == 6 & proc_dat_clean$sub == 2,]
collections[collections$block == 4 & collections$plot == 6 & collections$sub == 2,]$phyto.unique <- NA


## Make the modifications
collectionsC <- collections %>%
  filter(plot < 43, bkgrd != "VIVI", phyto.n.indiv > 0) %>%
  mutate(phyto.unique = ifelse(phyto.unique %in% As, "A", ## change all values to caps
                               ifelse(phyto.unique == "b","B", 
                                      ifelse(phyto.unique %in% Cs, "C", phyto.unique)))) %>%
  mutate(unique.ID = unique) %>% ## standardize column name
  mutate(bkgrd.n.indiv = ifelse(bkgrd == "Control", NA, bkgrd.n.indiv)) %>% ## change # of background indiv in controls to NA
  mutate(Nbrhood.size = ifelse(phyto %in% nhood10, 10, ## fill in all vals of neighborhood size
                               ifelse(phyto %in% nhood18, 18, Nbrhood.size)),
         phyto = ifelse(phyto == "ACAM ", "ACAM", ## fix phytos with spaces in the name
                        ifelse(phyto == "THIR-I ", "THIR-I", phyto)),
         bkgrd = ifelse(bkgrd == "BRNI ", "BRNI", ## fix backgrounds with spaces in the name
                        ifelse(bkgrd == "THIR-I ", "THIR-I",
                               ifelse(bkgrd == "TWIL-I ", "TWIL-I", 
                                      ifelse(bkgrd == "CESO ", "CESO",
                                             ifelse(bkgrd == "CLPU ", "CLPU", bkgrd)))))) %>%
  filter(phyto %in% summer_phytos) ## filter out only the fully processed phytos
    ## IMPORTANT THAT THIS STEP IS AFTER FIXING PHYTO NAMES



# Checks before Merge ####
processing_rejects <- rbind(incompletes_to_check, nas_to_check) ## combine all processing rejects
duplicated(processing_rejects)

## Check row nums ####
nrow(collectionsC) - nrow(proc_dat_clean)
## there are 347 more rows in collections than processing data
## WHY are there 347 more rows in collections but 348 that do not match.....

### Are proc and proc rejects separate? ####
unM_proc_and_rejects <- anti_join(proc_dat_clean, processing_rejects, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))
## There are 2959 here, same # of rows as the proc_dat_clean
## meaning there is no overlap between processing rejects and processing cleaned data.
## That's good.

#try_the_opp <- anti_join(processing_rejects, proc_dat_clean,by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))
## yep 350 rows - so there are no matches in proc rejects and proc clean data.


### Collections should contain all the processing rejects rows? ####
test <- semi_join(collectionsC, processing_rejects, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))

test2 <- semi_join(processing_rejects, collectionsC, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))
## looks like all of the processing rejects are present in collectionsC except the two Gopher BRHOs


unM_coll_and_rejects <- anti_join(collectionsC, processing_rejects, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))
## There are 2958 rows here 
## 2958 + 348 = 3306, so the rows that don't match proc_rejects + rows that do match proc_rejects add up

### Duplicates in Processing Data? ####
## so logically the mismatch in numbers should then be a row in the processing data that is NOT found in the censusing data?
## Need to isolate this somehow...

## first take out the processing rejects from the collectionsC data
## Then, the two dataframes SHOULD match up. But hopefully can isolate the one row that doesn't match...

coll_NO_rejects <- anti_join(collectionsC, processing_rejects, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))
## 2958 rows

## Now see what's diff between processign data and this dataframe
proc_coll_diff <- anti_join(proc_dat_clean, coll_NO_rejects, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))
## Nothing, apparently.
## Is there a duplicate in the proc_dat_clean

proc_distinct <- proc_dat_clean %>%
  distinct_at(vars(block, plot, sub, bkgrd, dens, phyto, phyto.unique))
## Based on the ID variables I am filtering it does seem like there is a repeated value since there are only 2958 distinct values.

## Now to find said repeated value..... ugh.
## Make one ID column and filter for duplicates in this...

temp_proc <- proc_dat_clean %>%
  mutate(sample.ID = paste(block, plot, sub, bkgrd, dens, phyto, phyto.unique, sep = "_"))

not_unique <- temp_proc %>%
  filter(duplicated(sample.ID))
## 7_34_11_PLNO_L_ACAM_NA

proc_dat_clean[proc_dat_clean$block == 7 & proc_dat_clean$plot == 34 & proc_dat_clean$sub == 11,]
## One duplicate ACAM sample. Unclear if this is two samples without phyto.uniques or if the same sample was weighed twice.
## Need to physically check samples for this.

### Temp Solution ####
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
## 2 are present in the unmatched dataframe that are not in the incomplete samples
## ANAR 4-6-2-A 
    ## FIXED!
## AVBA 16-18-21-" " 
    ## FIXED!
## these both have issues with the phyto.uniques

#unique(unmatched_not_reject$phyto.unique)
## the AVBA has a space in phyto.unique
## the ANAR has an A in phyto.unique in census data but not in the processing data

### Proc rejects but NOT unmatched ####
reject_not_unmatched <- anti_join(processing_rejects, unmatched_in_collections, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))
## ONLY 2 LEFT AND THESE ARE GOPHER CASUALTIES
## 4 are present in the incomplete samples that are not in the unmatched dataframe
## These shouldn't matter currently because they are incomplete

    ## AVBA 16-18-21-NA ## looks like the phyto-unique in the processing data (NA), does not align with the phyto-unique in collections data because of a space in the collections data phyto-unique.
        ## FIXED 
    ## THIR-I 16-27-4-B 
        ## FIXED
    ## the 2 BRHO phytos that were gopher casualties (7-25-8 A & B); These are in the processing rejects but not in collections because collections data were filtered for phyto > 0
        ## THESE DON'T MATTER

## Investigate THIR-I 16-27-4 
#collectionsC[collectionsC$block == 16 & collectionsC$plot == 27 & collectionsC$sub == 4,]
## phyto.unique B is NOT present in the collections cleaned data
#collections[collections$block == 16 & collections$plot == 27 & collections$sub == 4,]
## phyto.unique B IS present in the collections cleaned data
## Why is it being filtered out? -- looks like a space in teh phyto name. 
    ## FIXED THIS!


## Check rows in proc NOT coll ####
unmatched_in_processing <- anti_join(proc_dat_clean, collectionsC, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))
## NOTHING LEFT HERE!
## 4 samples are in the processing dataframe but not the collections dataframe
    ## ACAM 8-22-4-B
    ## ANAR 4-6-2-NA ## This appears to be a problem with a phyto.unique
    ## THIR-I 14-15-15-B
    ## THIR-I 15-39-13-C
## These issues have largely been resolved (I believe by taking out spaces in the phyto names)
## The only one left after the most recent modifications is ANAR 4-6-2-NA


### Investigate ACAM 8-22-4-B 
## seems like ACAM 8-22-4-B is somehow being removed from the collections data during filtering...
#unique(collections[collections$block == 8 & collections$plot == 22 & collections$sub == 4,]$phyto) 
## Ohhh... there is a space in the phyto name. Fix this above.
#unique(collectionsC[collectionsC$block == 8 & collectionsC$plot == 22 & collectionsC$sub == 4,]$phyto) 
## the space in phyto name has been fixed. 

## WHY are there issues ####
#unique(collectionsC$phyto.unique)
#unique(proc_dat_clean$phyto.unique)
#unique(collectionsC$phyto)
#unique(proc_dat_clean$phyto)
#unique(collectionsC$bkgrd)
#unique(proc_dat_clean$bkgrd)
## looks like there are differences in backgrounds that are causing these issues, particularly the collections backgrounds
## Fixed this above and it's helped

#sort(unique(collectionsC$sub))
#sort(unique(proc_dat_clean$sub))

#unique(collectionsC$dens)
#unique(proc_dat_clean$dens)

#sort(unique(collectionsC$block))
#sort(unique(proc_dat_clean$block))

#sort(unique(collectionsC$plot))
#sort(unique(proc_dat_clean$plot))


# Merge ####
cen_proc_all <- left_join(proc_dat_distinct, collectionsC, by = c("block", "plot", "sub", "bkgrd", "dens", "phyto", "phyto.unique"))


ggplot(cen_proc_all, aes(x=phyto.n.indiv.x, y=phyto.n.indiv.y)) +
  geom_point()
## all align without removing any rows. Phew.

ggplot(cen_proc_all, aes(x=unique.ID.x, y=unique.ID.y)) +
  geom_point()
## these all align but there are a lot of missing vals (makes sense - a lot were not included on the processing data sheets)

mc_dat <- cen_proc_all %>%
  mutate(phyto.n.indiv = phyto.n.indiv.x,
         unique.ID = unique.ID.y) %>%
  select(-phyto.n.indiv.y, -phyto.n.indiv.x, -unique.ID.y, -unique.ID.x)

# Clean up Environment ####
rm(list = c("As", "cen_proc_all", "coll_NO_rejects", "collections", "collectionsC", "Cs", "date_collections", "incompletes_to_check", "lead", "nas_to_check", "nhood10", "nhood18", "phyto.unique_spaces_collections", "phyto.unique_spaces_proc", "proc_coll_diff", "proc_dat_clean", "proc_dat_distinct", "proc_distinct",  "reject_not_unmatched", "summer_phytos", "temp_proc", "test", "test2", "unM_coll_and_rejects", "unM_proc_and_rejects", "unmatched_in_collections", "unmatched_in_collections2", "unmatched_in_processing", "unmatched_not_reject"))
