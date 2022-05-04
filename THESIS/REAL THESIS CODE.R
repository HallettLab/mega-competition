library(tidyverse)
library(dplyr)
library(lme4)
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
#####CONCLUSION: TREATMENT ALONE IS MARGINALLY SIGNIFICANT (0.0772)

aov_THIR_focal_add<-aov(Adjusted.Biomass~Treatment+Innoculation, data=THIR_focal)
summary(aov_THIR_focal_add)
######CONCLUSION: TREATMENT ALONE IS MARGINALLY SIGNIFICANT (0.0887)

ggplot(THIR_focal,aes(x=Treatment, y=Adjusted.Biomass))+
  geom_boxplot()
#aes means "aesthetics"?, tell it x-var and y-var
#"+" like adding another "layer"

ggplot(THIR_focal,aes(x=Innoculation, y=Adjusted.Biomass))+
  geom_boxplot()

ggplot(THIR_focal,aes(x=Innoculation, y=Adjusted.Biomass, color=Treatment))+
  geom_boxplot()

aov_THIR_weed<-aov(Adjusted.Biomass~Weed_Sum*Survival, data=THIR_focal)
summary(aov_THIR_weed)

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
######CONCLUSION: NO SIGNIFICANCE

aov_TWIL_focal_add<-aov(Adjusted.Biomass~Treatment+Innoculation, data=TWIL_focal)
summary(aov_TWIL_focal_add)
######CONCLUSION: NO SIGNIFICANCE

TWIL_focal_model <- lmer(Adjusted.Biomass~ Treatment+Innoculation+(1|Block..),data=TWIL_focal, na.action = "na.fail")

ggplot(TWIL_focal,aes(x=Innoculation, y=Adjusted.Biomass))+
  geom_boxplot()

ggplot(TWIL_focal,aes(x=Treatment, y=Adjusted.Biomass))+
  geom_boxplot()

ggplot(TWIL_focal,aes(x=Innoculation, y=Adjusted.Biomass, color=Treatment))+
  geom_boxplot()

#WEED STATS

#TWIL
aov_TWIL_weed<-aov(Adjusted.Biomass~Weed_Sum*Innoculation, data=TWIL_focal)
summary(aov_TWIL_weed)
######CONCLUSION: NO SIGNIFICANCE

aov_TWIL_weed<-aov(Adjusted.Biomass~Weed_Sum+Innoculation, data=TWIL_focal)
summary(aov_TWIL_weed)
######CONCLUSION: NO SIGNIFICANCE

aov_TWIL_weed<-aov(Adjusted.Biomass~Weed_Sum*Treatment, data=TWIL_focal)
summary(aov_TWIL_weed)
######CONCLUSION: NO SIGNIFICANCE

aov_TWIL_weed<-aov(Adjusted.Biomass~Weed_Sum+Treatment, data=TWIL_focal)
summary(aov_TWIL_weed)
######CONCLUSION: NO SIGNIFICANCE

aov_TWIL_weed<-aov(Survival~Weed_Sum*Innoculation, data=TWIL_focal)
summary(aov_TWIL_weed)
######CONCLUSION: NO SIGNIFICANCE

aov_TWIL_weed<-aov(Survival~Weed_Sum+Innoculation, data=TWIL_focal)
summary(aov_TWIL_weed)
######CONCLUSION: NO SIGNIFICANCE

aov_TWIL_weed<-aov(Survival~Weed_Sum*Treatment, data=TWIL_focal)
summary(aov_TWIL_weed)
######CONCLUSION: NO SIGNIFICANCE

aov_TWIL_weed<-aov(Survival~Weed_Sum+Treatment, data=TWIL_focal)
summary(aov_TWIL_weed)
######CONCLUSION: NO SIGNIFICANCE


#THIR
aov_THIR_weed<-aov(Adjusted.Biomass~Weed_Sum*Innoculation, data=THIR_focal)
summary(aov_THIR_weed)
######CONCLUSION: NO SIGNIFICANCE

aov_THIR_weed<-aov(Adjusted.Biomass~Weed_Sum+Innoculation, data=THIR_focal)
summary(aov_THIR_weed)
######CONCLUSION: NO SIGNIFICANCE

aov_THIR_weed<-aov(Adjusted.Biomass~Weed_Sum*Treatment, data=THIR_focal)
summary(aov_THIR_weed)
######CONCLUSION: TREATMENT ALONE IS MARGINALLY SIGNIFICANT (0.072)

aov_THIR_weed<-aov(Adjusted.Biomass~Weed_Sum+Treatment, data=THIR_focal)
summary(aov_THIR_weed)
######CONCLUSION: TREATMENT ALONE IS MARGINALLY SIGNIFICANT (0.0649)

aov_THIR_weed<-aov(Survival~Weed_Sum*Innoculation, data=THIR_focal)
summary(aov_THIR_weed)
######CONCLUSION: NO SIGNIFICANCE

aov_THIR_weed<-aov(Survival~Weed_Sum+Innoculation, data=THIR_focal)
summary(aov_THIR_weed)
######CONCLUSION: NO SIGNIFICANCE

aov_THIR_weed<-aov(Survival~Weed_Sum*Treatment, data=THIR_focal)
summary(aov_THIR_weed)
######CONCLUSION: NO SIGNIFICANCE

aov_THIR_weed<-aov(Survival~Weed_Sum+Treatment, data=THIR_focal)
summary(aov_THIR_weed)
######CONCLUSION: NO SIGNIFICANCE


#BACK STATS

#TWIL
TWIL_back<-Back%>%
  filter(Species=="TWIL")

aov_TWIL_back<-aov(Adjusted.Biomass~Treatment, data=TWIL_back)
summary(aov_TWIL_back)
######CONCLUSION: NO SIGNIFICANCE

aov_TWIL_back<-aov(Adjusted.Nodules~Treatment, data=TWIL_back)
summary(aov_TWIL_back)
######CONCLUSION: NO SIGNIFICANCE

lm_TWIL_back<-lm(Adjusted.Nodules~Adjusted.Biomass, data=TWIL_back)
#lm=linear model; comparing two continuous variables; testing for linear relationship between them 
#think about which might be more predictive of the other
summary(lm_TWIL_back)
######CONCLUSION: BIOMASS IS SIGNIFICANT (0.04129); for every unit increase in biomass we see a decrease in nodules.
#Estimate(Intercept) is y-int, Adjusted.Biomass-Estimate is equation
ggplot(TWIL_back, aes(x=Adjusted.Biomass, y=Adjusted.Nodules))+
  geom_point()+
  geom_smooth(method="lm")
#note that there is one outlier that may be influencing results
ggplot(TWIL_back, aes(x=Adjusted.Biomass, y=Adjusted.Nodules))+
  geom_point()+
  geom_smooth(method="lm")+
  xlim(4.5,10)
  #coord_cartesian(xlim=c(4.5,10))
#check trend without outlier; there is still a slight negative trend: CREATE NEW DATA FRAME WITHOUT OUTLIER TOO AND COMPARE SIGNIFICANCE


#Can't Use For Back (only one per block)
#TWIL_back_model <- lmer(Adjusted.Nodules~Adjusted.Biomass +(1|Block..),data=TWIL_back, na.action = "na.fail")
#linear mixed effects models = includes a "random effect" (in our case block) that might contribute in the variance in biomass (for example), so we want to control for that

#THIR
THIR_back<-Back%>%
  filter(Species=="THIR")

aov_THIR_back<-aov(Adjusted.Biomass~Treatment, data=THIR_back)
summary(aov_THIR_back)
######CONCLUSION: TREATMENT IS SIGNIFICANT (0.0284)

aov_THIR_back<-aov(Adjusted.Nodules~Treatment, data=THIR_back)
summary(aov_THIR_back)
######CONCLUSION: NO SIGNIFICANCE


lm_THIR_back<-lm(Adjusted.Biomass~Adjusted.Nodules, data=THIR_back)
summary(lm_THIR_back)
ggplot(THIR_back, aes(x=Adjusted.Nodules, y=Adjusted.Biomass))+
  geom_point()+
  geom_smooth(method="lm")
######CONCLUSION: NOT SIGNIFICANT

###HELP

#BOXPLOTS W/ WEEDS
ggplot(TWIL_focal, aes(x=Weed_Sum, y=Survival, color=Treatment))+
  geom_boxplot()

ggplot(THIR_focal, aes(x=Weed_Sum, y=Survival, color=Innoculation))+
  geom_boxplot()

#SCATTERPLOTS???
#ggplot(Focal_All, aes(x=Survival, y=Weed_Sum, color=Treatment))+
  #geom_point()

#BARPLOTS
ggplot(THIR_focal, aes(x=Survival, y=Weed_Sum, fill=Innoculation))+
  geom_bar(position='dodge', stat='identity')

ggplot(THIR_focal, aes(x=Survival, y=Weed_Sum, fill=Treatment))+
  geom_bar(position='dodge', stat='identity')

ggplot(TWIL_focal, aes(x=Survival, y=Weed_Sum, fill=Innoculation))+
  geom_bar(position='dodge', stat='identity')

ggplot(TWIL_focal, aes(x=Survival, y=Weed_Sum, fill=Treatment))+
  geom_bar(position='dodge', stat='identity')



###Two-way ANOVA: good for categorical comparisons (ex.FOCAL=we have multiple predictor variables; two independent variables (Treatment and Inoculation), one dependent variable (Adjusted.Biomass)) 

#FOCAL: TWIL-U and TWIL-I D/A biomass, THIR-U and THIR-I D/A biomass
#BACK: TWIL-U D/A biomass and nodules, THIR-U D/A biomass and nodules
#nodules=continuous variable---scatter plot
#NEIGHBOR:  TWIL-U and TWIL-I D/A biomass w/ weeds OR background species, THIR-U and THIR-I D/A biomass w/ weeds OR background specie
#make graphs---box-and-whisker plot graphs to start 