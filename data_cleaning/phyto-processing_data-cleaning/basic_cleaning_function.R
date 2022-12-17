# Set up env
library(tidyverse)


# Define Cleaning Function 
basic_cleaning_func <- function(phyto_data, ...) {
  
  drought <- c(1, 3, 4, 6, 12, 14) ## create treatment vector
  
  temp <- phyto_data %>%
    mutate(complete.sample = complete., ## change column names
           unique.ID = unique,
           treatment = ifelse(block %in% drought, "D", "C")) %>%  ## add a treatment column
    
    mutate(across(where(is.character), str_trim)) %>% ## remove leading & trailing whitespace!!
    
    mutate(across(c(phyto.unique,scale.ID,complete.sample), toupper)) %>% ## capitalize all vals 
    
    mutate_all(na_if,"") %>% ## make blank values NAs
    
    filter(plot < 43, bkgrd != "VIVI")
  
  return(temp)
  
}