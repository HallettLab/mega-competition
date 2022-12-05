## Evaluate Spot-Checks

## set up env
library(tidyverse)
theme_set(theme_bw())
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

# import data ####
## set file path
# specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_spot-checks/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_spot-checks/"
  
} else {
  # Marina
  lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Processing/Phytometer-Processing/Phytometer-Processing_spot-checks/"
} 

## brho
brho.sc <- read.csv(paste0(lead, "brho_spotcheck_list.csv"))
brho.sc.2 <- read.csv(paste0(lead, "brho_spotcheck_list_OLD.csv")) %>%
  filter(!is.na(spot.check.inflor.g))


# BRHO ####
## need to find difference b/w orig measurement and spot check measurement
brhocheck <- brho.sc %>%
  mutate(measure.diff = inflor.g - spot.check.inflor.g)

brhocheck$block <- as.factor(brhocheck$block)
## then check for the biggest differences
ggplot(brhocheck, aes(x=measure.diff)) +
  geom_histogram()

ggplot(brhocheck, aes(x=block, y= measure.diff)) +
  geom_boxplot()
## one concerning sample

brho.sum <- brhocheck %>%
  group_by(block) %>%
  summarise(mean.mdiff = mean(measure.diff), max.mdiff = max(abs(measure.diff)), median.mdiff = median(measure.diff), se.mdiff = calcSE(measure.diff))

ggplot(brho.sum, aes(x=block, y=mean.mdiff)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean.mdiff - se.mdiff, ymax = mean.mdiff + se.mdiff), width = 0.25)

ggplot(brho.sum, aes(x=block, y=median.mdiff)) +
  geom_point()

ggplot(brho.sum[brho.sum$max.mdiff < 0.2,], aes(x=block, y=max.mdiff)) +
  geom_point()
## take out the obvious outlier to check whether anything else is of a concerning magnitude
    ## is 0.016 concerning?

## also comb the notes for anything out of place
unique(brhocheck$spot.check.notes)

brho.changes <- brhocheck %>%
  filter(measure.diff > 0.01)
## allo relationship
    ## slope = 951.72967 seeds per gram of biomass

# 952 seeds/g * 0.01g
952*0.01
## so for every 0.01 g off there are ~9 seeds diff
952*0.016 ## = 15.232 seeds off
## will leave this one for now.

## 3-15-13 B changed on 12/1/2022 by CW





