
library(tidyverse)
theme_set(theme_bw())

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

# Read in Data ####
# specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Allometry/Allometry_entered/")){
  # Carmen
  allo_lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Allometry/Allometry_entered/"
  
} else {
  # Marina
  allo_lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Allometry/Allometry_entered/"
} 

## Allometry data
brni_allo <- read.csv(paste0(allo_lead, "BRNI_allometry-processing_20230301.csv"))

brni_seeds <- read.csv(paste0(allo_lead, "BRNI-seeds_allometry-processing_20230223.csv"))


brni_seed_means <- brni_seeds %>%
  group_by(treatment) %>%
  summarise(mean_seeds = mean(seeds.per.one.pod, na.rm = T), se_seeds = calcSE(seeds.per.one.pod))


brni_allo2 <- brni_allo %>%
  filter(!is.na(viable.pod.num)) %>%
  mutate(seeds.predicted = viable.pod.num*brni_seed_means[brni_seed_means$treatment == "C",]$mean_seeds, 
         viability = viable.pod.num/pod.num)

brni_allo2$seeds.num <- as.numeric(brni_allo2$seeds.num)

ggplot(brni_allo2, aes(x=seeds.num, y=seeds.predicted)) +
  geom_point() +
  geom_abline(slope = 1)

ggplot(brni_allo2, aes(x=total.biomass.g, viable.pod.num)) +
  geom_point()+
  ggtitle("BRNI Allo")
ggsave("allometry/preliminary_figs/brni_bio_via_pods.png", width = 5, height = 3)

ggplot(brni_allo2, aes(x=total.biomass.g, pod.num)) +
  geom_point() +
  ggtitle("BRNI Allo")
ggsave("allometry/preliminary_figs/brni_bio_all_pods.png", width = 5, height = 3)

ggplot(brni_allo2, aes(x=total.biomass.g, seeds.num)) +
  geom_point()


ggplot(brni_allo2, aes(x=total.biomass.g, y=viability)) +
  geom_point()
ggsave("allometry/preliminary_figs/brni_bio_viability.png", width = 5, height = 3)

