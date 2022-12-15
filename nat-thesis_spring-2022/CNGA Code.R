library(tidyverse)
library(dplyr)
library(lme4)
library(MuMIn)
library(lmerTest)
library(cowplot)

setwd("~/Documents/Repositories/mega-competition/nat-thesis_spring-2022") 

Focal<- read.csv("Focal Individuals.csv")
Back<- read.csv("Background_Individuals.csv")
Neighbor<- read.csv("Neighborhood_Counts.csv")

## CE file paths, please save: 
#Focal<- read.csv("nat-thesis_spring-2022/Focal Individuals.csv")
#Back<- read.csv("nat-thesis_spring-2022/Background_Individuals.csv")
#Neighbor<- read.csv("nat-thesis_spring-2022/Neighborhood_Counts.csv")


Neighbor_Clean<-Neighbor%>%
  filter(Phytometer!="BRHO", Phytometer!="PLER", Phytometer!="BHRO")

Neighbor_Clean$CRCO <- NULL

Neighbor_Clean$Weed_Sum <-rowSums(Neighbor_Clean[ ,c(10,11,12,13,14,15)], na.rm=TRUE)

Neighbor_Clean <- Neighbor_Clean %>% 
  mutate(Survival = Phyto.. / 3)

colnames(Neighbor_Clean) <- c("Block", "Plot", "Sub", "Treatment", "Background", "Density","Sample.Name", "Back.Ind", "Phyto","ERBO","FIGA","GAMU","HYGL","SIGA","other","X", "Weed_Sum", "Survival")

colnames(Focal) <- c("Sample.Name", "Species", "Innoculation", "Treatment","Block", "Plot", "Quad", "Stem.Count", "Weight.of.Biomass", "Adjusted.Biomass",  "Weight.of.Roots", "Weight.of.Soil", "ARA.Soil")

Focal_All<-left_join(Neighbor_Clean,Focal,by=c("Block", "Plot", "Treatment", "Sample.Name"))
#"c"=character vector (list)

twil <- c("TWIL-I", "TWIL-U")
inoc <- c("TWIL-I", "THIR-I")

Focal_All_Cleaned <- Focal_All %>%
  mutate(Species = ifelse(Sample.Name %in% twil, "TWIL", "THIR"))  %>% 
  mutate(Inoc = ifelse(Sample.Name %in% inoc, "I", "U")) %>% 
  mutate(Trt = ifelse(Block == 5, "A", Treatment)) %>% 
  select(-Treatment, -Innoculation) %>% 
  mutate(Treatment = Trt, Innoculation = Inoc) %>% 
  select(-Trt, -Inoc) 

summary <- Focal_All_Cleaned %>%
  group_by(Species, Innoculation, Treatment) %>%
  summarize(mean.biomass = mean(Adjusted.Biomass, na.rm = TRUE),
            se.biomass = calcSE(Adjusted.Biomass), 
            n = length(Adjusted.Biomass))

ggplot(summary, aes(x=bkgrd, y=mean.biomass, color=Treatment)) + 
  geom_point() +
  geom_errorbar(aes(ymin = mean.biomass-se.biomass, ymax = mean.biomass + se.biomass), width = 0.25) 

Focal_All_Boxplot<-Focal_All_Cleaned %>%
  filter(Survival != 0)

### Final Figures ####

focals <- ggplot(Focal_All_Boxplot,aes(x=Species, y=Adjusted.Biomass, color=Treatment))+
  geom_boxplot()+
  theme_bw()+
  ylab("Focal Biomass (g)") +
  scale_color_manual(values = c("#008080", "#ca562c"), labels = c("Ambient", "Drought"))
focals

focals_inoc <- ggplot(Focal_All_Boxplot,aes(x=Innoculation, y=Adjusted.Biomass, color=Treatment))+
  geom_boxplot()+
  theme_bw()+
  ylab("Focal Biomass (g)")+
  facet_wrap(~Species)+
  scale_color_manual(values = c("#008080", "#ca562c"), labels = c("Ambient", "Drought")) +
  theme(legend.position = "none")
focals_inoc

bgs <- ggplot(Back,aes(x=Species, y=Adjusted.Biomass, color=Treatment))+
  geom_boxplot()+
  theme_bw()+
  ylab("Back Biomass (g)")+
  scale_color_manual(values = c("#008080", "#ca562c"), labels = c("Ambient", "Drought")) +
  theme(legend.position = "none") 
bgs

## Combining all three panels into one figure!

pt2 <- plot_grid(focals_inoc, bgs,
                 labels=c("B","C"))

pt1 <- plot_grid(focals, 
                 labels = c("A"))

plot_grid(pt1, pt2, ncol = 1, nrow = 2)


#TO DO 
#do the same thing we did for megacomp: summary > plot (use mean and se)
 
#looking at the relative change (divide something by total biomass) in biomass under drought for each species 

#check if there's an interaction (+ instead of *) (between drought and inoculation, if there is, maybe remove treatment 

#for illustrating coexistence, we might need to compare growth of TWIL and THIR in the different backgrounds (average the background by number of individuals)
