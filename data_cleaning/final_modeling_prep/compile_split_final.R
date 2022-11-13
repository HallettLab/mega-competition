
source("data_cleaning/final_modeling_prep/BRHO_model_prep.R")
source("data_cleaning/final_modeling_prep/GITR_model_prep.R")


colnames(brho.model)
colnames(gitr.model)

clean.dfs <- c("brho.model", "gitr.model")

census.all <- as.data.frame(matrix(ncol=11))
colnames(census.all) <- c("unique.ID", "intraphyto", "bkgrd.n.indiv", "CRCO", "ERBO", "FIGA", "GAMU", "HYGL", "SIGA", "other", "census.notes")

## use a for-loop to pull out the census data
for (i in 1:length(clean.dfs)) {
  
  ## filter out the correct dataframe
  tmp.dat <- get(clean.dfs[i])
  
  ## select the desired columns
  tmp.cen <- tmp.dat %>%
    select(unique.ID, intraphyto, bkgrd.n.indiv, CRCO, ERBO, FIGA, GAMU, HYGL, SIGA, other, census.notes)
  
  ## append
  census.all <- rbind(census.all, tmp.cen)
  
}
census.all <- census.all %>%
  filter(!is.na(unique.ID))

## eventually save this output


## compile & separate phyto data
phyto.all <- as.data.frame(matrix(ncol=11))


for (i in 1:length(clean.dfs)) {
  
  ## filter out the correct dataframe
  tmp.dat <- get(clean.dfs[i])
  
  ## get species name
  tmp.sp <- substr(clean.dfs[i], 1, 4) ## get sp name from first 4 letters of the df name

  ## change column name to "XXXX.seed.in" or "XXXX.seed.out"
  tmp.phyto <- tmp.dat %>%
    mutate(paste0(tmp.sp, "seed.in") = phyto.seed.in,
           paste0(tmp.sp, "seed.out") = phyto.seed.out) %>%
    select(unique.ID, paste0(tmp.sp, "seed.in"), paste0(tmp.sp, "seed.out"))
  ## will need to go back to indiv scripts to change to generic "phyto.seed.in" column name
  ## might be able to do a few more of the individual scripts in for-loops like these. Think about this!!
  
  
}






