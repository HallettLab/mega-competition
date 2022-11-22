# Calculating Germination Rates #

# SE Function ####
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

# Load Libraries ####
library(tidyverse)

# Load Data ####
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Background-Processing/Background-Processing_entered/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Germination/Germination_cleaned/"
  
} else {
  # Marina
  lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Germination/Germination_cleaned/"
} 

germ <- read.csv(paste0(lead, "20220218_Germination-Data_full.csv"))

  
# Explore Data ####

#Calculate average germination rates per species per treatment
germ.sum.trt <- germ %>%
  group_by(Species, Temp, WP) %>%
  summarize(avg.germ = mean(p.germ), se.germ = calcSE(p.germ))


#Calculate average germination rates per species
germ.sum.sp <- germ %>%
  group_by(Species) %>%
  summarize(avg.germ = mean(p.germ), se.germ = calcSE(p.germ))

germ.sum.sp$Temp <- 15 # "average temp"
germ.sum.sp$WP <- -0.25 # "average water potential"

germ.sum.sp <- germ.sum.sp[,c(1,4,5,2,3)]

germ.sum <- rbind(germ.sum.sp, germ.sum.trt)

ggplot(germ.sum.trt[germ.sum.trt$WP != -0.5,], aes(x = as.factor(Temp), y = avg.germ)) +
  geom_point() +
  geom_errorbar(aes(ymin = avg.germ - se.germ, ymax = avg.germ + se.germ, width = 0.1)) +
  facet_wrap(~Species)
