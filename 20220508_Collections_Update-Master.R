### Collections Update ###
# This script is the final iteration! It only took 8 versions
# The key things you will have to change are the date, old.date, lead, and the path to the master on the google drive.
# additionally deleting files in the google drive may be tricky...

## For each week there are three masters: 
### 1. The issues master initially written post merge but before notes are combed through. This is when the red things on the weird things list are fixed and deletions are done. This is then saved as the in progress master
### 2. In progress master is read into this script, split into blocks, and uploaded to google drive. Things on this master may further change throughout the week as weird things are resolved, but nothing can be deleted as these will be overwritten with data in individual datasheets
### 3. Prior to a merge, the in progress master is downloaded from google drive and is saved as the final master. This is the input for the merge.

rm(list=ls())
options(warn = 1)

library(googlesheets4)
library(plyr)
library(tidyverse)
library(googledrive)
library(openxlsx)

#### Change Dates ####
old.date <- 20220817 # date of last data merger
date <- 20220825 # date of new data download

#### Change Lead ####
lead <- "/Users/Marina/Documents/Dropbox/" # Marina's file path
#lead <- "/Users/carme/Dropbox (University of Oregon)/" # Carmen's file path

#### Download Files from Google Drive ####

# THIS CREATES A FOLDER, ONLY DO THIS ONCE
dir.create(paste0(lead, "Mega_Competition/Data/Collections/Collections_entered/", date)) 

paths <- list(Matthew = data.frame(), Jazz = data.frame(), Jasmin = data.frame())

#paths[["Carmen"]] <- drive_ls(path = "https://drive.google.com/drive/folders/1FifbCLkwWLDTIQQ-16rsYeWB0qLgt-bX")

paths[["Matthew"]] <- drive_ls(path = "https://drive.google.com/drive/folders/1Y9yqRl4Je13fTvXNz9Z5qjNSCB3G7bTu")

paths[["Jazz"]] <- drive_ls(path = "https://drive.google.com/drive/folders/1QwSd0Cxv7zneVE2Cs3cMGpRgOrk2Q8Vr")

paths[["Jasmin"]] <- drive_ls("https://drive.google.com/drive/u/0/folders/1bH798yHnyf5QEPGoqxU7PSsfjc-xxoM6")

#paths[["Anjum"]] <- drive_ls("https://drive.google.com/drive/u/0/folders/1U1b9f50Rs_005z0WWOW01wOq95m6yOkB")

for(i in names(paths)){
  
  dir.create(paste0(lead, "Mega_Competition/Data/Collections/Collections_entered/", date, "/", i, "_Block_Collections/"))
  
  for(j in 1:nrow(paths[[i]])){
    drive_download(
      file = paths[[i]][j,]$name,
      path = paste0(lead, "Mega_Competition/Data/Collections/Collections_entered/", date, "/", i, "_Block_Collections/", paths[[i]][j,]$name),
      type = "xlsx",
      overwrite = FALSE
    )
  }
}

#### Read Downloaded Files ####
files <- list(Matthew = data.frame(), Jazz = data.frame(), Jasmin = data.frame())

for(i in names(files)) {
  files[[i]] <- list.files(paste0(lead, "Mega_Competition/Data/Collections/Collections_entered/", date, "/", i, "_Block_Collections/"))
}

sheet <- c(1:6) # number of sheets

all.data <- list(bkgrd = list(), phyto = list(), weeds = list(), WUE = list(), census.notes = list(), seeding.notes = list())

for(i in names(files)) {
  for(j in sheet) {
    for(k in files[[i]]) {
      if(j == 1) {
          tmp <- read.xlsx(paste0(lead, "Mega_Competition/Data/Collections/Collections_entered/", date, "/", i, "_Block_Collections/", k), sheet = j, na.strings = c("", "NA", "N/A", -9999))
        tmp$date.collect <- as.Date(tmp$date.collect, origin = "1899-12-30")
        tmp[which(tmp$unique == 33), 5:7] <- NA
        all.data[[j]][[i]] <- rbind(all.data[[j]][[i]], tmp)
        all.data[[j]][[i]] <- all.data[[j]][[i]][order(all.data[[j]][[i]]$block, all.data[[j]][[i]]$plot),]
        
      }
      
      if(j == 2) {
        tmp <- read.xlsx(paste0(lead, "Mega_Competition/Data/Collections/Collections_entered/", date, "/", i, "_Block_Collections/", k), sheet = j, na.strings = c("", "NA", "N/A", -9999))
        tmp$phyto.date.collect <- as.Date(tmp$phyto.date.collect, origin = "1899-12-30") 
        tmp$phyto.date.census <- as.Date(tmp$phyto.date.census, origin = "1899-12-30")
        tmp$bg.date.census <- as.Date(tmp$bg.date.census, origin = "1899-12-30")
        
        all.data[[j]][[i]] <- rbind(all.data[[j]][[i]], tmp)
        
        all.data[[j]][[i]] <- all.data[[j]][[i]][order(all.data[[j]][[i]]$block, all.data[[j]][[i]]$plot, all.data[[j]][[i]]$sub),] 
        
        }
      
      if(j == 3) {
        tmp <- read.xlsx(paste0(lead, "Mega_Competition/Data/Collections/Collections_entered/", date, "/", i, "_Block_Collections/", k), sheet = j, na.strings = c("", "NA", "N/A", -9999))
        tmp$date.collect <- as.Date(tmp$date.collect, origin = "1899-12-30")
        all.data[[j]][[i]] <- unique(rbind(all.data[[j]][[i]], tmp))
        all.data[[j]][[i]] <- all.data[[j]][[i]][!is.na(all.data[[j]][[i]][,"initials"]),]
        all.data[[j]][[i]] <- all.data[[j]][[i]][order(all.data[[j]][[i]]$species, all.data[[j]][[i]]$rep),]
      }
      
      if(j == 4) {
        tmp <- read.xlsx(paste0(lead, "Mega_Competition/Data/Collections/Collections_entered/", date, "/", i, "_Block_Collections/", k), sheet = j, na.strings = c("", "NA", "N/A", -9999))
        tmp$date.collect <- as.Date(tmp$date.collect, origin = "1899-12-30")
        all.data[[j]][[i]] <- unique(rbind(all.data[[j]][[i]], tmp))
        all.data[[j]][[i]] <- all.data[[j]][[i]][!is.na(all.data[[j]][[i]][,"initials"]),]
        all.data[[j]][[i]] <- all.data[[j]][[i]][order(all.data[[j]][[i]]$species, all.data[[j]][[i]]$rep),]
      } 
      
      if(j == 5) {
        tmp <- read.xlsx(paste0(lead, "Mega_Competition/Data/Collections/Collections_entered/", date, "/", i, "_Block_Collections/", k), sheet = j, na.strings = c("", "NA", "N/A", -9999))
        tmp$phyto.date.collect <- as.Date(tmp$phyto.date.collect, origin = "1899-12-30") 
        tmp$phyto.date.census <- as.Date(tmp$phyto.date.census, origin = "1899-12-30")
        tmp$bg.date.census <- as.Date(tmp$bg.date.census, origin = "1899-12-30")
        tmp$phyto.unique <- as.character(tmp$phyto.unique)
        all.data[[j]][[i]] <- unique(rbind(all.data[[j]][[i]], tmp))
        all.data[[j]][[i]] <- all.data[[j]][[i]][order(all.data[[j]][[i]]$block, all.data[[j]][[i]]$plot, all.data[[j]][[i]]$sub),] 
        
        all.data[[j]][[i]][,"plot.notes"] <- as.character(all.data[[j]][[i]][,"plot.notes"])
        
        all.data[[j]][[i]][,"sub.notes"] <- as.character(all.data[[j]][[i]][,"sub.notes"])
        
        all.data[[j]][[i]][,"Nbrhood.size"] <- as.numeric(all.data[[j]][[i]][,"Nbrhood.size"])
      } 
      
      if(j == 6) {
        tmp <- read.xlsx(paste0(lead, "Mega_Competition/Data/Collections/Collections_entered/", date, "/", i, "_Block_Collections/", k), sheet = j, na.strings = c("", "NA", "N/A", -9999))
        all.data[[j]][[i]] <- unique(rbind(all.data[[j]][[i]], tmp))
        all.data[[j]][[i]] <- all.data[[j]][[i]][order(all.data[[j]][[i]]$block, all.data[[j]][[i]]$plot, all.data[[j]][[i]]$sub),] 
        
      } 
    }
  }
} 


## 06/08/2022 Notes from soft merge:
  ## 3-23-4: had to delete collection dates for everyone as it kicked up errors
  ## JA block 7, census.notes tab, several rows had unique values in the wrong column. Needed to be moved over before the data could be read in.


## Common errors:
###Error in charToDate(x) : character string is not in a standard unambiguous format --> someone entered a date incorrectly. look at your env to find which person/block this is in and change it in their datasheet

### Warning in unzip(xlsxFile, exdir = xmlDir) : error 1 in extracting from zip file Error in file(con, "r") : invalid 'description' argument --> this means you have a data sheet open! close and rerun

#### Read Google drive direct ####
# This section pulls directly from google drive, great for mid-merge checks (takes about an hour to read files this way though and may not be in the correct file format)
# paths <- list(Carmen = data.frame(), Matthew = data.frame(), Jazz = data.frame(), Anjum = data.frame(), Jasmin = data.frame())
# 
# paths[["Carmen"]] <- drive_ls(path = "https://drive.google.com/drive/folders/1FifbCLkwWLDTIQQ-16rsYeWB0qLgt-bX")
# 
# paths[["Matthew"]] <- drive_ls(path = "https://drive.google.com/drive/folders/1Y9yqRl4Je13fTvXNz9Z5qjNSCB3G7bTu")
# 
# paths[["Jazz"]] <- drive_ls(path = "https://drive.google.com/drive/folders/1QwSd0Cxv7zneVE2Cs3cMGpRgOrk2Q8Vr")
# 
# paths[["Jasmin"]] <- "https://drive.google.com/drive/u/0/folders/1bH798yHnyf5QEPGoqxU7PSsfjc-xxoM6"
#
#paths[["Anjum"]] <- "https://drive.google.com/drive/u/0/folders/1U1b9f50Rs_005z0WWOW01wOq95m6yOkB"
# 
# sheet <- c(1:5) # number of sheets
# 
# all.data <- list(bkgrd = list(), phyto = list(), weeds = list(), WUE = list(), notes = list())
# 
# 
# for(i in names(paths)){
#   for(j in sheet) {
#     for(k in paths[[i]]$id){
#       if(j == 1) {
#         tmp <- read_sheet(k, sheet = j, na = c("", "NA", "N/A", -9999), col_types = "dd??iTccn")
#         all.data[[j]][[i]] <- rbind(all.data[[j]][[i]], tmp)
#       }
#       
#       if(j == 2) {
#         tmp <- read_sheet(k, sheet = j, na = c("", "NA", "N/A", -9999), col_types = "dddccciTTcdiTiiiiiiicccn") #"iiiccciTTciiTiiiiiiiccci") 
#         
#         all.data[[j]][[i]] <- rbind(all.data[[j]][[i]], tmp)
#         
#         # fix empty switches
#       }
#       
#       if(j == 3) {
#         tmp <- read_sheet(k, sheet = j, na = c("", "NA", "N/A", -9999), col_types = "nnncnTcc")
#         all.data[[j]][[i]] <- unique(rbind(all.data[[j]][[i]], tmp))
#         all.data[[j]][[i]] <- all.data[[j]][[i]][!is.na(all.data[[j]][[i]][,"initials"]),]
#       }
# 
#       if(j == 4) {
#         tmp <- read_sheet(k, sheet = j, na = c("", "NA", "N/A", -9999), col_types = "??ccniTcc")
#         all.data[[j]][[i]] <- unique(rbind(all.data[[j]][[i]], tmp))
#         all.data[[j]][[i]] <- all.data[[j]][[i]][!is.na(all.data[[j]][[i]][,"initials"]),]
#         
#       }
#     }
#   }
# }

#### Read Final Master ####
# Download final master from google drive; this path will change each time because it is a different file each time
drive_download(
      file = "https://docs.google.com/spreadsheets/d/1E3nQpWJGcXRlwht1cPvtOJ_p7Mb2nEAihJ36BxFgk6k/edit#gid=199051268",
      path = paste0(lead, "Mega_Competition/Data/Collections/Collections_merged/", old.date, "_MASTER_Collections_3-final"),
      type = "xlsx",
      overwrite = FALSE
    )

master <- list(bkgrd = data.frame(), phyto = data.frame(), weeds = data.frame(), WUE = data.frame(), census.notes = data.frame(), seeding.notes = data.frame())

sheet <- 1:6

for(j in sheet) {
    
    master[[j]] <- read.xlsx(paste0(lead, "Mega_Competition/Data/Collections/Collections_merged/", old.date, "_MASTER_Collections_3-final.xlsx"), sheet = j, na.strings = c("", "NA", "N/A"))
        
    if(j == 1) { 
      master[[j]] <- master[[j]][order(master[[j]]$block, master[[j]]$plot),]
      #master[[j]][which(master[[j]]$unique == 33), 5:7] <- NA # where did this line of code come from? 
    }
    
    if(j == 2 | j == 5) {
      master[[j]]$phyto.date.collect <- as.Date(master[[j]]$phyto.date.collect, origin = "1899-12-30")
    
      master[[j]]$phyto.date.census <- as.Date(master[[j]]$phyto.date.census, origin = "1899-12-30")
    
      master[[j]]$bg.date.census <- as.Date(master[[j]]$bg.date.census, origin = "1899-12-30")
      
      master[[j]]$phyto.unique <- as.character(master[[j]]$phyto.unique)
      
      master[[j]] <- master[[j]][order(master[[j]]$block, master[[j]]$plot,  master[[j]]$sub),]
      
    }
    
    if( j == 1 | j == 3 | j == 4) {
      master[[j]]$date.collect <- as.Date(master[[j]]$date.collect, origin = "1899-12-30")
    }
    
    if(j == 3) {
      master[[j]] <- master[[j]][order(master[[j]]$block, master[[j]]$species, master[[j]]$rep),]
      master[[j]][,"notes"] <- as.character(master[[j]][,"notes"])
    }
    
    if(j == 4) {
      master[[j]] <- master[[j]][order(master[[j]]$species, master[[j]]$rep),]
      master[[j]][,"notes"] <- as.character(master[[j]][,"notes"])
    }
    
    if(j == 5) {
      master[[j]] <- master[[j]][order(master[[j]]$block, master[[j]]$plot, master[[j]]$plot),]
      master[[j]][,"plot.notes"] <- as.character(master[[j]][,"plot.notes"])
      master[[j]][,"sub.notes"] <- as.character(master[[j]][,"sub.notes"])
    }

}

options(warn = 2) ## script will break down if there is an error

#### Update: Bkgrd ####

columns.to.fill <- c("n.indiv", "date.collect", "initials", "notes")

columns.filled <- c("block", "plot", "bkgrd", "dens", "unique")

for(i in names(all.data[[1]])) {
  new <- anti_join(all.data[[1]][[i]], master[[1]], "unique") # new rows
  tmp <- anti_join(all.data[[1]][[i]], new) # extract old rows for updating
  
  
  # check to make sure there are no differences in filled columns between master and tmp, stop if any differences
  stopifnot("Differences between datasets detected, fix before running for loop" = all_equal(master[[1]][, columns.filled], tmp[,columns.filled])) 
  
  #assign new rows a unique ID
  if(nrow(new) != 0) {
    for(j in 1:nrow(new)) {
      new[,"unique"][j] <- max(master$unique, na.rm = T) + j
    }
  }
  
  # Update the master with data from each spreadsheet
  for(j in master[[1]]$unique){ # for each unique row
    for(k in columns.to.fill) {# for each column to be filled
      if(is.na(master[[1]][which(master[[1]]$unique == j), ][k])) { # check to see if it is NA
        master[[1]][which(master[[1]]$unique == j),][k] <- tmp[which(tmp$unique == j), ][k] # if it is NA, replace with data from tmp; 
      }
    }
  }
  
  # Append the master dataframe with the new rows
    master[[1]] <- rbind(master[[1]], new)

}

## Run if Diffs ##
diffs <- rbind(anti_join(master[[1]][,columns.filled], tmp[,columns.filled]),
              anti_join(tmp[,columns.filled], master[[1]][,columns.filled])) 

# sort by block/plot rather than unique id
master[[1]] <- master[[1]][order(master[[1]]$bkgrd, master[[1]]$block),]

#### Update: Weeds ####
# AS OF 5/26/2022, ALL WEEDS ARE ON MASTER, THIS PART CAN BE SKIPPED
# columns.to.fill <- c("block", "plot", "sub", "date.collect", "initials", "notes")
# 
# columns.filled <- c("species", "rep")
# 
# # CHECK TO MAKE SURE THIS WORKS
# for(i in names(all.data[[3]])) {
#   
#   tmp <- anti_join(all.data[[3]][[i]], master[[3]]) 
#   tmp2 <- anti_join(master[[3]], tmp)
#   
#   master[[3]] <- rbind(tmp, tmp2)
#   
# }
# 
# master[[3]] <- master[[3]][order(master[[3]]$species, master[[3]]$block, master[[3]]$rep),]

#### Update: WUE ####
## This is done now! CE commented out before 06/08 merge.
# the next merge will be the last time this has to be done, then it can be commented out
#columns.to.fill <- c("block", "plot", "sub", "date.collect", "initials", "notes")

#columns.filled <- c("species", "rep")

#for(i in names(all.data[[4]])) {
  
#  tmp <- semi_join(all.data[[4]][[i]], master[[4]], c("species", "rep")) 
#  tmp2 <- anti_join(master[[4]], tmp, c("species", "rep"))
  
#  master[[4]] <- rbind(tmp, tmp2)
#  master[[4]] <- master[[4]][order(master[[4]]$species, master[[4]]$rep),]

#}

#### Update: Census Notes ####

replacements <- data.frame()

master[[5]]$Nbrhood.size <- as.numeric(master[[5]]$Nbrhood.size)

for(i in names(all.data[[5]])) {

  tmp <- anti_join(all.data[[5]][[i]], master[[5]])
  tmp2 <- anti_join(master[[5]], tmp)

  master[[5]] <- rbind(tmp, tmp2)
  replacements <- rbind(replacements, tmp) # only newly added census notes

}

#### Update: Phyto ####

columns.to.fill <- c("phyto.n.indiv", "phyto.date.census", "phyto.date.collect", "phyto.unique", "bkgrd.n.indiv", "bg.date.census", "CRCO", "ERBO", "FIGA", "GAMU", "HYGL", "SIGA", "pheno.notes", "collect.notes", "background.notes", "other", "notes")

columns.filled <- c("block", "plot", "sub", "bkgrd", "dens", "phyto", "unique")

base <- max(master[[2]]$unique, na.rm = T) 

for(i in names(all.data[[2]])) {
  # first we have to find which rows are not in the master (e.g. adding a phytometer row), extract these to be binded at the end
  new <- anti_join(all.data[[2]][[i]], master[[2]], "unique")# new rows
  tmp <- anti_join(all.data[[2]][[i]], new) # extract old rows for updating
  
   # check to make sure there are no differences in filled columns between master and tmp, stop if any differences
  stopifnot("Differences between datasets detected, fix before running for loop" = all_equal(master[[2]][master[[2]]$unique <= base, columns.filled], tmp[,columns.filled])) 
  
  #assign new rows a unique ID
  if(nrow(new) != 0) {
    for(j in 1:nrow(new)) {
      new[j,"unique"] <- max(master[[2]]$unique, na.rm = T) + j
    }
  }
  
  # Update the master with data from each spreadsheet
  for(j in master[[2]]$unique){ # for each unique row
    for(k in columns.to.fill){ # for each column to be filled
      if(is.na(master[[2]][which(master[[2]]$unique == j), ][k])) { # check to see if it is NA
        master[[2]][which(master[[2]]$unique == j),][k] <- tmp[which(tmp$unique == j), ][k] 
      }
    }
  }
  
  # Append the master dataframe with the new rows
     master[[2]] <- rbind(master[[2]], new)

} 

# sort by block/plot/sub rather than unique id
master[[2]] <- master[[2]][order(master[[2]]$block, master[[2]]$plot, master[[2]]$sub),]

master[[2]]$phyto.n.indiv <- as.integer(master[[2]]$phyto.n.indiv)


## Run if Diffs ##
diffs <- rbind(anti_join(master[[2]][,columns.filled], tmp[,columns.filled]),
              anti_join(tmp[,columns.filled], master[[2]][,columns.filled])) 


#### Use Census.Notes to change phytos ####

# investigate duplicates

### List dates and changes below, COMMENT OUT AFTER DONE
# 05/23/2022: uniques 11575, 7482, and 10936 are duplicates, investigated and getting rid of rows 166, 176, 155 which have outdated notes
# rm <- c(166, 176, 155) #rownames to remove
# master[[5]] <- master[[5]][!(rownames(master[[5]]) %in% rm),]

## 05/28/2022: uniques 9163, NA, 2529, 1363, 1853, 2186, 5796, 6437, 7812, 8113, 8477, 9538, 9588, 9813, 9863, 10561 are duplicates
#tmp<-master[[5]]
#rm <- c(2842, 2901, 29, 50, 60, 148, 166, 188, 195, 204, 228, 229, 236, 239, 263)
#master[[5]] <- master[[5]][!(rownames(master[[5]]) %in% rm),]

## 06/08/2022: uniques NA, 2536, 11710, 3454, 5448, 5473, 5849, 6024, 7862, 8594, 9438, 9613, 10138, 10162
# tmp<-master[[5]]
# rm <- c(91, 104, 112, 174, 175, 185, 189, 232, 257, 279, 286, 311, 312)

## 06/15/2022: unique dupes: 3  1108  1363  1461  1507  1528  1570  2454  3559  3584  3609 11672  4009  5348  6071 6199  6446  7378  7803  8113  8213  8519  9044 9237  9413 10113 10840
# master[[5]][168,]$unique <- 11961
# master[[5]][169,]$unique <- 11962
# master[[5]][duplicated(master[[5]]$unique),]$unique 
# tmp <- master[[5]]
# rm <- c(2, 32, 49, 55, 58, 62, 63, 104, 141, 142, 143, 151, 159, 201, 227, 233, 244, 264, 281, 290, 293, 307, 329, 335, 340, 379, 407)

#master[[5]] <- master[[5]][!(rownames(master[[5]]) %in% rm),]

## 6/22/2022: 
#tmp<-master[[5]]

## there are two rows of 7-22-2 in the replacement dataframe. I am keeping the one with the collection date as this is likely to be the most recent. The other change just looks like a change to the notes.

for(i in replacements$unique) { # for each unique ID in the replacement dataframe
  master[[2]][which(master[[2]]$unique == i),] <- replacements[which(replacements$unique == i),] #replace phyto row with the census notes row
}

#### Write Issues Master ####

#dates are funky if READ directly from google drive FYI, though this is rarely done

write.xlsx(master, file = paste0(lead, "Mega_Competition/Data/Collections/Collections_merged/", date, "_MASTER_Collections_1-issues.xlsx"))

# this is where post merge checks take place. Once you change anything in the issues master, save as yyyymmdd_MASTER_Collections_2-in-progress.xlsx. Once you are satisfied with all checks, proceed to next step.

#### Read in progress Master ####

rm(list=ls()) # start with clean directory just to be safe

date <- 20220825 # date of new data 

#lead <- "/Users/Marina/Documents/Dropbox/" # Marina's file path
lead <- "/Users/carme/Dropbox (University of Oregon)/" # Carmen's file path

master <- list(bkgrd = data.frame(), phyto = data.frame(), weeds = data.frame(), WUE = data.frame(), census.notes = data.frame(), seeding.notes = data.frame())

sheet <- 1:6

for(j in sheet) {
    master[[j]] <- read.xlsx(paste0(lead, "Mega_Competition/Data/Collections/Collections_merged/", date, "_MASTER_Collections_2-in-progress.xlsx"), sheet = j, na.strings = c("", "NA", "N/A"))
        
    if(j == 1) { 
      master[[j]] <- master[[j]][order(master[[j]]$block, master[[j]]$plot),]
      master[[j]][which(master[[j]]$unique == 33), 5:7] <- NA
    }
    
    if(j == 2 | j == 5) {
      master[[j]]$phyto.date.collect <- as.Date(master[[j]]$phyto.date.collect, origin = "1899-12-30")
    
      master[[j]]$phyto.date.census <- as.Date(master[[j]]$phyto.date.census, origin = "1899-12-30")
    
      master[[j]]$bg.date.census <- as.Date(master[[j]]$bg.date.census, origin = "1899-12-30")
      
      master[[j]]$phyto.unique <- as.character(master[[j]]$phyto.unique)
      
      master[[j]] <- master[[j]][order(master[[j]]$block, master[[j]]$plot,  master[[j]]$sub),]
      
    }
    
    if( j == 1 | j == 3 | j == 4) {
      master[[j]]$date.collect <- as.Date(master[[j]]$date.collect, origin = "1899-12-30")
    }
    
    if(j == 3) {
      master[[j]] <- master[[j]][order(master[[j]]$block, master[[j]]$species, master[[j]]$rep),]
      master[[j]][,"notes"] <- as.character(master[[j]][,"notes"])
    }
    
    if(j == 4) {
      master[[j]] <- master[[j]][order(master[[j]]$species, master[[j]]$rep),]
      master[[j]][,"notes"] <- as.character(master[[j]][,"notes"])
    }

}

#### Block Split for datasheets ####
dir.create(paste0(lead, "Mega_Competition/Data/Collections/Collections_datasheets/", date)) 

FilterData <- list(bkgrd = data.frame(), phyto = data.frame(), weeds = data.frame(), WUE = data.frame(), census.notes = data.frame(), seeding.notes = data.frame())

# Write files to local directory
for (i in unique(master[[1]]$block)){
    print(i)
    FilterData[[1]] <- master[[1]] %>% filter(block == i) 
    FilterData[[2]] <- master[[2]] %>% filter(block == i) 
    FilterData[[3]] <- master[[3]]
    FilterData[[4]] <- master[[4]]
    FilterData[[5]] <- master[[5]]
    FilterData[[6]] <- master[[6]]
    filename <- paste0(lead, "Mega_Competition/Data/Collections/Collections_datasheets/", date, "/Block", i, "_Collections.xlsx")
    write.xlsx(FilterData, file = filename)
}

# Copy files to google drive
name <- c("Jazz", "Matthew", "Master")

paths <- list(Matthew = data.frame(), Jazz = data.frame(), Jasmin = data.frame(), Master = data.frame())

#paths[["Carmen"]] <- "https://drive.google.com/drive/folders/1FifbCLkwWLDTIQQ-16rsYeWB0qLgt-bX"

paths[["Matthew"]] <- "https://drive.google.com/drive/folders/1Y9yqRl4Je13fTvXNz9Z5qjNSCB3G7bTu"

paths[["Jazz"]] <- "https://drive.google.com/drive/folders/1QwSd0Cxv7zneVE2Cs3cMGpRgOrk2Q8Vr"

#paths[["Jasmin"]] <- "https://drive.google.com/drive/u/0/folders/1bH798yHnyf5QEPGoqxU7PSsfjc-xxoM6"

#paths[["Anjum"]] <- "https://drive.google.com/drive/u/0/folders/1U1b9f50Rs_005z0WWOW01wOq95m6yOkB"

paths[["Master"]] <- "https://drive.google.com/drive/folders/1F9So6hjVPXS5e6nRptbeXxo5H1HsAM1d"

# delete files inside google drive folders before proceeding just to avoid confusion; this may not be possible unless you CREATED these files, the other lead may have to delete

for(i in unique(master[[1]]$block)){
  for(j in name){
      drive_upload(
        media = paste0(lead, "Mega_Competition/Data/Collections/Collections_datasheets/", date, "/Block", i, "_Collections.xlsx"),
        path = paths[[j]],
        name = paste0(date, "_", j, "_Block", i, "_Collections"),
        type = "spreadsheet",
        overwrite = T,
      )
  }
}
