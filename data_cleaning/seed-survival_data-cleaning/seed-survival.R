# Seed survival
## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Seed-Survival/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Seed-Survival/"
  
} else {
  # Marina
  lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Seed-Survival/"
} 
surv <- read.csv(paste0(lead, "Seed-bag-survival_Year1.csv"))

library(tidyverse)

# remove uninoculated trifoliums and species we are no longer using
rm <- c("Stipa pulchra", "Vicia villosa", "Erodium botrys")
surv <- filter(surv, X != "uninoculated", !Species %in% rm)

# look at distribution
ggplot(surv, aes(x = Species, y = n.viable)) +
  geom_boxplot() # few big outliers, looks decent

surv.sum <- surv %>%
  group_by(Species) %>%
  summarize(
    surv.mean.p = mean(n.viable)/100,
    surv.se.p = calcSE(n.viable)/100
  )

  surv.sum$Species <- factor(surv.sum$Species, levels = surv.sum$Species[order(surv.sum$surv.mean.p)])
  
ggplot(surv.sum, aes(x = Species, y = surv.mean.p)) +
  geom_point() +
  geom_errorbar(aes(ymin = surv.mean.p - surv.se.p, ymax = surv.mean.p + surv.se.p)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


