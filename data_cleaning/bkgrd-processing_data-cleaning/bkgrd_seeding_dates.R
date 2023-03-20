library(tidyverse)
library(lubridate)
library(stringr)

# Read in Data ####
# specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Set-up/Plot-backgrounds/Plot-backgrounds_entered/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Set-up/Plot-backgrounds/Plot-backgrounds_entered/"
  
} else {
  # Marina
  lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Set-up/Plot-backgrounds/Plot-backgrounds_entered/"
} 

plot.seeding <- read.csv(paste0(lead, "20220214_plot-blackgrounds.csv"))

filter.bkgrd <- c("VIVI", "ERBO", "empty", "Control")

plot.dates <- plot.seeding %>%
  filter(plot < 43, !background %in% filter.bkgrd) %>%
  mutate(bkgrd = background) %>%
  mutate(year = 2021, ## separate out date info
         month = 11, 
         day = str_sub(date, 4)) %>%
  mutate(date.chr = paste0(year, month, day),
         date = ymd(date.chr)) %>%
  mutate(bkgrd = ifelse(bkgrd == "THIR-I", "THIR", bkgrd),
         bkgrd = ifelse(bkgrd == "TWIL-I", "TWIL", bkgrd)) %>%
  select(block, plot, bkgrd, date)


rm(list = c("plot.seeding", "filter.bkgrd"))
