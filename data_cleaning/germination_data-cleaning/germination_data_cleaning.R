# Load Libraries ####
library(tidyverse)
library(stringr)

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

# Load Data ####
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Background-Processing/Background-Processing_entered/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Germination/Germination_cleaned/"
  
} else {
  # Marina
  lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Germination/Germination_cleaned/"
} 

germ <- read.csv(paste0(lead, "20220218_Germination-Data_full.csv"))

# Change Sp Codes ####
germ2 <- germ %>%
  mutate(species2 = sub("^(.{1,5}).(.*)", "\\1\\2", Species),
         species = sub("^(.{1,2}).(.*)", "\\1\\2", species2)) %>%
  mutate(species = ifelse(species == "FEPE", "LOMU",
                          ifelse(species == "ELCA", "TACA", ifelse(species == "TRHI", "THIR", 
                                                                   ifelse(species == "TRWI", "TWIL", species))))) %>%
  select(-Species, -species2)
## change species from 6 letter codes to 4 letter codes to match all other mega comp dfs

## get ready to input to models
mean.germ <- germ2 %>% 
  group_by(species, water) %>%
  summarize(mean.germ = mean(p.germ), se.germ = calcSE(p.germ)) %>%
  mutate(treatment = ifelse(water == "dry", "D", "C"),
         phyto = species) %>%
  ungroup() %>%
  select(phyto, treatment, mean.germ, se.germ)

## clean env
rm(germ, germ2)
