## Clean Seed Survival Data

# Set up Env ####
library(tidyverse)

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}


# Read in Data ####
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Seed-Survival/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Seed-Survival/"
  
} else {
  # Marina
  lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Seed-Survival/"
} 
surv <- read.csv(paste0(lead, "Seed-bag-survival_Year1.csv"))


# Clean Data ####
# remove uninoculated trifoliums and species we are no longer using
rm <- c("Stipa pulchra", "Vicia villosa", "Erodium botrys")
surv <- filter(surv, X != "uninoculated", !Species %in% rm)

# look at distribution
ggplot(surv, aes(x = Species, y = n.viable)) +
  geom_boxplot() # few big outliers, looks decent


surv.sum <- surv %>%
  ## change to 4 letter species codes 
  mutate(genus = toupper(substr(Species, 1, 2)), 
         sp.ep = substr(toupper(sub("([^ ]* )", "\\2", Species)), 1, 2),
         species = paste0(genus, sp.ep), 
         species = ifelse(species == "FEPE", "LOMU", 
                          ifelse(species == "ELCA", "TACA", species))) %>%
  group_by(species) %>%
  summarize(
    surv.mean.p = mean(n.viable)/100,
    surv.se.p = calcSE(n.viable)/100
  )


rm(list = c("rm", "surv"))