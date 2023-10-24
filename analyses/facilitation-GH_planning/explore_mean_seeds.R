model.dat <- read.csv("data/model_dat.csv")
library(tidyverse)
theme_set(theme_classic())

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

brho <- model.dat %>%
  filter(phyto == "BRHO", 
         bkgrd == "ACAM") %>%
  mutate(percap.seeds = phyto.seed.out/)

ggplot(brho, aes(x=phyto.seed.out)) +
  geom_histogram() +
  facet_wrap(~treatment)

brho.sum <- brho %>%
  group_by(treatment) %>%
  summarise(mean.seeds = mean(phyto.seed.out), se.seeds = calcSE(phyto.seed.out))


ggplot(brho.sum, aes(x=treatment, y=mean.seeds)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean.seeds - se.seeds, ymax = mean.seeds + se.seeds), width = 0.3)
