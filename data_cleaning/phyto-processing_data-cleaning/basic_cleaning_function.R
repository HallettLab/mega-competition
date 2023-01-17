## The purpose of this script is to define a function that will standardize & clean up the phyto-processing data. It deals with common issues like capitalization in several columns, leading/trailing white spaces, blank spaces, etc.

# Set up env
library(tidyverse)


# Define Cleaning Function 
basic_cleaning_func <- function(phyto_data, ...) {
  
  drought <- c(1, 3, 4, 6, 12, 14) ## create treatment vector
  
  temp <- phyto_data %>%
    mutate(complete.sample = complete., ## change column names
           treatment = ifelse(block %in% drought, "D", "C")) %>%  ## add a treatment column
    
    mutate(across(where(is.character), str_trim)) %>% ## remove leading & trailing whitespace!!
    
    mutate(across(c(phyto.unique,scale.ID,complete.sample), toupper)) %>% ## capitalize all vals 
    
    mutate_all(na_if,"") %>% ## make blank values NAs
    
    filter(plot < 43, bkgrd != "VIVI") ## get rid of trifolium sub experiment and VIVI plots that were not used at all.
  
  return(temp)
  
}
