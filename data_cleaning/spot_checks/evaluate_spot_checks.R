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

## gitr
gitr.sc <- read.csv(paste0(lead, "gitr_spotcheck_list.csv"))


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








# GITR ####
## need to find difference b/w orig measurement and spot check measurement
gitrcheck <- gitr.sc %>%
  mutate(measure.diff = abs(total.biomass.g - spot.check.total.biomass.g),
         completion.match = ifelse(complete.sample == spot.check.complete.sample, "Y", "N"), 
         rel.measure.diff = measure.diff/total.biomass.g)

ggplot(gitrcheck, aes(x=measure.diff)) +
  geom_histogram() +
  xlab("Abs(Measurement Diff)") +
  ggtitle("GITR Spot Check")
## one value is clearly an issue, need to check it
ggsave("data_cleaning/spot_checks/spot_check_figs/gitr_diff.png")

ggplot(gitrcheck, aes(x=block, y=measure.diff, group = block)) +
  geom_boxplot() +
  ylab("Abs(Measurement Diff)") +
  ggtitle("GITR Spot Check")

ggsave("data_cleaning/spot_checks/spot_check_figs/gitr_diff_blocks.png", width = 5, height = 4)


ggplot(gitrcheck, aes(x=block, y=rel.measure.diff, group = block)) +
  geom_boxplot() +
  ylab("Abs(Measurement Diff)/Tot Bio") +
  ggtitle("GITR Spot Check")

ggsave("data_cleaning/spot_checks/spot_check_figs/gitr_rel_diff_blocks.png", width = 5, height = 4)

ggplot(gitrcheck, aes(x=rel.measure.diff)) +
  geom_histogram() +
  xlab("Abs(Measurement Diff)/Tot Bio") +
  ggtitle("GITR Spot Check")

gitr.sum <- gitrcheck %>%
  group_by(block) %>%
  summarise(mean.mdiff = mean(measure.diff), max.mdiff = max(abs(measure.diff)), median.mdiff = median(measure.diff), se.mdiff = calcSE(measure.diff))

ggplot(gitr.sum, aes(x=block, y=mean.mdiff)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean.mdiff - se.mdiff, ymax = mean.mdiff + se.mdiff), width = 0.25) +
  ylab("Mean Abs(Measurement Diff)") + xlab("Block")

ggsave("data_cleaning/spot_checks/spot_check_figs/gitr_mean_diff_blocks.png", width = 4, height = 3)


## Separate Redos ####
gitr.redos <- gitrcheck %>%
  filter(measure.diff > 0.01)

## some of these do need changes, many were removing dirt or background flowers



## GITR Allo Relationship: 
## How many seeds are there for each 0.01g biomass that we are off...

(77.61267*0.01) + (-7.113502*0.01^2)
## 77.613 flowers/g, -7.114 flowers/g2

## 0.7754153 flowers per 0.01 g?
0.7754153*11.640625 ## = 9 seeds off
0.7754153*8.701754 ## = 6 seeds off

(77.61267*0.026) + (-7.113502*0.026^2)

2.013121*11.640625 ## = 23.43399 seeds off
2.013121*8.701754 ## = 17.51768 seeds off










