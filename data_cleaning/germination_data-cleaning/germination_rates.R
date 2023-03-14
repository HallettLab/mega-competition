# Calculating Germination Rates #

# SE Function ####
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

# Load Libraries ####
library(tidyverse)
library(stringr)


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

  
# Calc Avg Germ ####
## Per Treat ####
#Calculate average germination rates per species per treatment (temp & water potential)
germ.sum.trt <- germ2 %>%
  group_by(species, Temp, WP) %>%
  summarize(avg.germ = mean(p.germ), se.germ = calcSE(p.germ))

## Per Sp ####
#Calculate average germination rates per species
germ.sum.sp <- germ2 %>%
  filter(WP == 0) %>%
  group_by(species) %>%
  summarize(avg.germ = mean(p.germ), se.germ = calcSE(p.germ)) %>%
  mutate(Temp = 15, ## "average temp"
         WP = 0) ## "average water potential"

germ.sum.sp <- germ.sum.sp[,c(1,4,5,2,3)] ## reorder cols

germ.sum.sp.DC <- germ2 %>% #using this in BH models
  group_by(species, WP) %>%
  summarize(avg.germ = mean(p.germ), se.germ = calcSE(p.germ))

germ.sum.sp.DC$trt <- ifelse(germ.sum.sp.DC$WP == 0, "C", "D")

## Combine ####
germ.sum <- rbind(germ.sum.sp, germ.sum.trt) %>%
  filter(WP == 0)
unique(germ.sum$WP)

# Explore Data ####
ggplot(germ.sum.trt[germ.sum.trt$WP != -0.5,], aes(x = as.factor(Temp), y = avg.germ)) +
  geom_point() +
  geom_errorbar(aes(ymin = avg.germ - se.germ, ymax = avg.germ + se.germ, width = 0.1)) +
  facet_wrap(~species)

ggplot(germ.sum.trt[germ.sum.trt$WP == -0.5,], aes(x = as.factor(Temp), y = avg.germ)) +
  geom_point() +
  geom_errorbar(aes(ymin = avg.germ - se.germ, ymax = avg.germ + se.germ, width = 0.1)) +
  facet_wrap(~species)

## 0 is the 'wettest WP'

# Create BG germ DF ####
## Germ by temperature
avgtemp <- c("ACAM", "AMME", "AVBA", "BRHO", "CESO", "TACA", "LOMU", "GITR", "LENI", "MAEL", "MICA", "PLNO", "TWIL", "THIR")
## no effect of temp on germination, use average

vartemp <- c("ANAR", "BRNI", "CLPU")
## variable temp: for blocks seeded in warm rain use warm germ, otherwise cold

coldtemp <- c("PLER")
## better germ in cold temps, use cold

species <- unique(germ.sum$species)

## create empty data frame
df <- data.frame()

## loop thru each species
for (i in 1:length(species)) {

sp <- species[i] ## separate species

if (sp %in% avgtemp) { ## if the species isn't affected by temp, use the average germ across water & temp treatments
  
  temp <- germ.sum %>%
    filter(Temp == 15, species == sp)
  
} else if (sp %in% vartemp) { ## if the species germ rate depends on temp
  
  temp <- germ.sum %>%
    filter(Temp != 15, species == sp)
  
} else { ## for anything else (cold temp species - PLER)
  
  temp <- germ.sum %>%
    filter(species == sp, Temp == 10, WP == 0)
  
}
  
df <- rbind(df, temp)

}

bg.germ <- df %>%
  mutate(bkgrd = species)




rm(list = c("avgtemp", "coldtemp", "germ.sum", "germ2", "i", "lead", "sp","species", "temp", "vartemp", "germ.sum.sp", "germ.sum.trt", "germ", "df"))
