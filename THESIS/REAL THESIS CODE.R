library(tidyverse)
library(dplyr)
#%>% is called a pipeline=(ex.name%>%) it will continue whatever you want to apply to the initial data frame
#tidyverse gets the data into the format you want; when you run statistics you aren't using tidyverse
#"~" denotes an equation
#hold "control" and "return" to run line

#library(minpack.lm)
#library(nlstools)
#library(grid)
#library(gridExtra)

setwd("~/Desktop/Repositories/mega-competition/THESIS/")  

Focal<- read.csv("Focal Individuals.csv")
Back<- read.csv("Background_Individuals.csv")
Neighbor<- read.csv("Neighborhood_Counts.csv")

colnames(Neighbor)
colnames(Focal)

Neighbor_Clean<-Neighbor%>%
  filter(Phytometer!="BRHO", Phytometer!="PLER", Phytometer!="BHRO")
#"!=" does not equal 

Neighbor_Clean$CRCO <- NULL
head(Neighbor_Clean)
#removed CRCO

Neighbor_Clean$Weed_Sum <-rowSums(Neighbor_Clean[ ,c(10,11,12,13,14,15)], na.rm=TRUE)
#summed weed counts

Neighbor_Clean <- Neighbor_Clean %>% 
  mutate(Survival = Phyto.. / 3) 
#created survival percentage

colnames(Neighbor_Clean) <- c("Block..", "Plot..", "Sub..", "Treatment", "Background", "Density","Sample.Name", "Back.Ind..", "Phyto..","ERBO","FIGA","GAMU","HYGL","SIGA","other","X", "Weed_Sum", "Survival") 

Focal_All<-left_join(Neighbor_Clean,Focal,by=c("Block..", "Plot..", "Treatment", "Sample.Name"))
#"c"=character vector (list)

count_focal<-Focal_All%>%
  group_by(Species, Innoculation, Treatment) %>% 
  summarize(count=n())

mean_focal<-Focal_All%>%
  group_by(Species, Innoculation, Treatment) %>% 
  summarize(mean_biomass=mean(Adjusted.Biomass),sd_biomass=sd(Adjusted.Biomass))

#TRIF STATS
THIR_focal<-Focal_All%>%
  filter(Species=="THIR")
#Boolean "==" evaluates if an expression is true or false; if true it will keep it, if false it will remove it
aov_THIR_focal<-aov(Adjusted.Biomass~Treatment*Innoculation, data=THIR_focal)
#"+" means no interaction between variables, "*" means you are assuming there is interaction 
summary(aov_THIR_focal)
#look at "Pr(>F) for the p-value
#"*" in Signif.codes mean less than 0.05
#"." in Signif.codes means "marginally significant"

aov_THIR_focal_add<-aov(Adjusted.Biomass~Treatment+Innoculation, data=THIR_focal)
summary(aov_THIR_focal_add)

ggplot(THIR_focal,aes(x=Treatment, y=Adjusted.Biomass))+
  geom_boxplot()
#aes means "aesthetics"?, tell it x-var and y-var
#"+" like adding another "layer"

ggplot(THIR_focal,aes(x=Innoculation, y=Adjusted.Biomass))+
  geom_boxplot()

ggplot(THIR_focal,aes(x=Innoculation, y=Adjusted.Biomass, color=Treatment))+
  geom_boxplot()


#TWIL STATS
TWIL_focal<-Focal_All%>%
  filter(Species=="TWIL")
#Boolean "==" evaluates if an expression is true or false; if true it will keep it, if false it will remove it
aov_TWIL_focal<-aov(Adjusted.Biomass~Treatment*Innoculation, data=TWIL_focal)
#"+" means no interaction between variables, "*" means you are assuming there is interaction 
summary(aov_TWIL_focal)
#look at "Pr(>F) for the p-value
#"*" in Signif.codes mean less than 0.05
#"." in Signif.codes means "marginally significant"

aov_TWIL_focal_add<-aov(Adjusted.Biomass~Treatment+Innoculation, data=TWIL_focal)
summary(aov_TWIL_focal_add)

ggplot(TWIL_focal,aes(x=Innoculation, y=Adjusted.Biomass))+
  geom_boxplot()

ggplot(TWIL_focal,aes(x=Treatment, y=Adjusted.Biomass))+
  geom_boxplot()

ggplot(TWIL_focal,aes(x=Innoculation, y=Adjusted.Biomass, color=Treatment))+
  geom_boxplot()

###WEED COUNT (X-AXIS) AND PERCENT SURVIVAL (Y-AXIS) CONTINOUS VARIABLES AS SCATTERPLOT AND BOXPLOT;COLOR BY TREATMENT OR INOCULATION

ggplot(TWIL_focal, aes(x=Weed_Sum, y=Survival, color=Treatment))+
  geom_boxplot()

ggplot(THIR_focal, aes(x=Weed_Sum, y=Survival, color=Innoculation))+
  geom_boxplot()

ggplot(TWIL_focal, aes(x=Weed_Sum, y=Survival, color=Treatment))+
  geom_point()

ggplot(THIR_focal, aes(x=Weed_Sum, y=Survival, color=Innoculation))+
  geom_point()

#separate by TWIL and THIR
#run anova 

#means and stand.devs
#what significance tests should i run
###Two-way ANOVA: good for categorical comparisons (ex.FOCAL=we have multiple predictor variables; two independent variables (Treatment and Inoculation), one dependent variable (Adjusted.Biomass)) 
#FOCAL: TWIL-U and TWIL-I D/A biomass, THIR-U and THIR-I D/A biomass
#BACK: TWIL-U D/A biomass and nodules, THIR-U D/A biomass and nodules
#nodules=continuous variable---scatter plot
#NEIGHBOR:  TWIL-U and TWIL-I D/A biomass w/ weeds OR background species, THIR-U and THIR-I D/A biomass w/ weeds OR background specie
#make graphs---box-and-whisker plot graphs to start 