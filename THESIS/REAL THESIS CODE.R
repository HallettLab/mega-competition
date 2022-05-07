###5/1 What info from ANOVA models do we get versus the other models? What is coming out as significant? Interesting? Find a package that calculates p-value.
#How long should my introduction be? Scope? How much background or just what is necessary? Ask lab members. 

library(tidyverse)
library(dplyr)
library(lme4)
library(MuMIn)
library(lmerTest)

#install.packages("name") to install new package
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

Focal_All_Boxplot<-Focal_All%>%
  filter(!is.na(Species))

ggplot(Focal_All_Boxplot,aes(x=Species, y=Adjusted.Biomass, color=Treatment))+
  geom_boxplot()+
  theme_bw()+
  ylab("Biomass (g)")+
  ggtitle("Focal Treatment")
#USE THIS GRAPH

ggplot(Focal_All_Boxplot,aes(x=Species, y=Adjusted.Biomass, color=Treatment))+
  geom_boxplot()+
  theme_bw()+
  ylab("Biomass (g)")+
  facet_wrap(~Innoculation)+
  ggtitle("Focal Inoculation and Treatment")
#this graph compares treatment and inoculation 

ggplot(Back,aes(x=Species, y=Adjusted.Biomass, color=Treatment))+
  geom_boxplot()+
  theme_bw()+
  ylab("Biomass (g)")+
  ggtitle("Background Treatment")
#USE THIS GRAPH; in the figure caption be clear which figure is focal data and which is back data...
  #why does this graph look opposite of focal individual. graph? background individuals were growing in competition with itself (intraspecific competition)...even when we compare it to uninoculated focal individuals it's still the opposite relationship
      #maybe trif. individuals are limiting themselves more than other species...

#TRIF STATS
THIR_focal<-Focal_All%>%
  filter(Species=="THIR")
#Boolean "==" evaluates if an expression is true or false; if true it will keep it, if false it will remove it
#aov_THIR_focal<-aov(Adjusted.Biomass~Treatment*Innoculation, data=THIR_focal)
#"+" means no interaction between variables, "*" means you are assuming there is interaction 
#summary(aov_THIR_focal)
#look at "Pr(>F) for the p-value
#"*" in Signif.codes mean less than 0.05
#"." in Signif.codes means "marginally significant"
#####CONCLUSION: TREATMENT ALONE IS MARGINALLY SIGNIFICANT (P-VALUE 0.0772)

aov_THIR_focal_add<-aov(Adjusted.Biomass~Treatment+Innoculation, data=THIR_focal)
summary(aov_THIR_focal_add)
######CONCLUSION: TREATMENT ALONE IS MARGINALLY SIGNIFICANT (0.0887)

#THIR_focal_model <- lmerTest::lmer(Adjusted.Biomass~ Treatment+Innoculation+(1|Block..),data=THIR_focal, na.action = "na.fail")
#dredge(THIR_focal_model)
#CONCLUSION: THERE ARE 3 MODELS WE CAN'T DISTINGUISH BETWEEN (1-3), SO THERE MIGHT BE SOME EFFECT OF INOCULATION AND TREATMENT 

#stepTHIR<-step(THIR_focal_model)
#CONCLUSION: INOCULATION ALONE IS MARGINALLY SIGNIFICANT (0.09435)
#get_model(stepTHIR)
#CONCLUSION:Adjusted.Biomass~ 1 returned: THE NULL MODEL IS BEST FIT

#THIR_focal_nullmodel1<-lmer(Adjusted.Biomass~ 1+(1|Block..),data=THIR_focal, na.action = "na.fail")
#summary(THIR_focal_nullmodel1)
#CONCLUSION: BLOCK DOES EXPLAIN SOME VARIANCE (look at Variance for row "Block..")

#THIR_focal_model2<-lmerTest::lmer(Adjusted.Biomass~ Innoculation+(1|Block..),data=THIR_focal, na.action = "na.fail")
#summary(THIR_focal_model2)
#CONCLUSION: INOCULATION IS MARGINALLY SIGNIFICANT (0.0944)
    #Block may be contributing to inoculation as a marginally significant factor..
#ranova(THIR_focal_model2)
#looking at random effects (ex.block effect on dependent variable) and putting it in an anova-like table..using likelihood ratio tests (vs. a fixed effect such as treatment on the dependent variable)
#CONCLUSION: (looking at AIC) SINCE BLOCK..AIC DIFFERENCE IS =2 (AND THE LOWEST AIC), INCLUDING BLOCK MAY BE THE BEST FIT FOR OUR DATA.
    #P-VALUE=1


#THIR_focal_model3<-lmerTest::lmer(Adjusted.Biomass~ Treatment+(1|Block..),data=THIR_focal, na.action = "na.fail")
#summary(THIR_focal_model3)
#CONCLUSION: TREATMENT IS MARGINALLY SIGNIFICANT (0.09566)
#ranova(THIR_focal_model3)
#CONCLUSION: (looking at AIC) SINCE BLOCK..AIC DIFFERENCE IS =2 (AND THE LOWEST AIC), INCLUDING BLOCK MAY BE THE BEST FIT FOR OUR DATA..MAKES SENSE THEY ARE THE SAME SINCE IT'S STILL LOOKING AT RANDOM EFFECT OF BLOCK

ggplot(THIR_focal,aes(x=Treatment, y=Adjusted.Biomass))+
  geom_boxplot()
#aes means "aesthetics"?, tell it x-var and y-var
#"+" like adding another "layer"

ggplot(THIR_focal,aes(x=Innoculation, y=Adjusted.Biomass))+
  geom_boxplot()

ggplot(THIR_focal,aes(x=Treatment, y=Adjusted.Biomass))+
  geom_boxplot()
#USE THIS ONE

#aov_THIR_weed<-aov(Adjusted.Biomass~Weed_Sum*Survival, data=THIR_focal)
#summary(aov_THIR_weed)

#TWIL STATS
TWIL_focal<-Focal_All%>%
  filter(Species=="TWIL")
#Boolean "==" evaluates if an expression is true or false; if true it will keep it, if false it will remove it
#aov_TWIL_focal<-aov(Adjusted.Biomass~Treatment*Innoculation, data=TWIL_focal)
#"+" means no interaction between variables, "*" means you are assuming there is interaction 
#summary(aov_TWIL_focal)
#look at "Pr(>F) for the p-value
#"*" in Signif.codes mean less than 0.05
#"." in Signif.codes means "marginally significant"
######CONCLUSION: NO SIGNIFICANCE

aov_TWIL_focal_add<-aov(Adjusted.Biomass~Treatment+Innoculation, data=TWIL_focal)
summary(aov_TWIL_focal_add)
######CONCLUSION: NO SIGNIFICANCE

#TWIL_focal_model <- lmer(Adjusted.Biomass~ Treatment+Innoculation+(1|Block..),data=TWIL_focal, na.action = "na.fail")
#dredge(TWIL_focal_model)
#we are using this model to check if block impacts data
#linear mixed effect model; using a (AIC) score (low is good; if one score is at least 2 lower than the others then we say it's the best model (it "fit" the best)) to compare the impact of the predictor variables (Treatment, Inoculation, or Treatment and Inoculation, or neither) on biomass 
#make sure to report df...even if we don't know what it means
#delta=difference between the first model (1) and everything else below it
#CONCLUSION: NULL MODEL BEST EXPLAINS OUR DATA; IT HAS THE LOWEST AIC AND THE NEXT CLOSEST AIC IS 6.5 AWAY (WHICH IS >2)
    #RESULTS SECTION: biomass did not vary significantly between treatment or inoculation...the best fit modeling predicting biomass is the null model, which didn't include either predictor 

#TWIL_focal_nullmodel<-lmer(Adjusted.Biomass~ 1+(1|Block..),data=TWIL_focal, na.action = "na.fail")
#summary(TWIL_focal_nullmodel)
#"1"=null model, no predictors 
#block does not explain the intercept of biomass, it explains a small amount of residual variance..
#CONCLUSION: block doesn't impact the trends we see in the data 

ggplot(TWIL_focal,aes(x=Innoculation, y=Adjusted.Biomass))+
  geom_boxplot()

ggplot(TWIL_focal,aes(x=Treatment, y=Adjusted.Biomass))+
  geom_boxplot()

ggplot(TWIL_focal,aes(x=Innoculation, y=Adjusted.Biomass, color=Treatment))+
  geom_boxplot()

#WEED STATS

#TWIL

TWIL_focal_lm<-lm(Adjusted.Biomass~Back.Ind..+Weed_Sum+Treatment+Innoculation,data=TWIL_focal)
summary(TWIL_focal_lm)
######CONCLUSION: NO SIGNIFICANCE
#How do we add covariates to a model (that's how we want to factor weeds into this)

#aov_TWIL_weed<-aov(Adjusted.Biomass~Weed_Sum*Innoculation, data=TWIL_focal)
#summary(aov_TWIL_weed)
######CONCLUSION: NO SIGNIFICANCE

aov_TWIL_weed<-aov(Adjusted.Biomass~Weed_Sum+Innoculation, data=TWIL_focal)
summary(aov_TWIL_weed)
######CONCLUSION: NO SIGNIFICANCE

aov_TWIL_backind<-aov(Adjusted.Biomass~Back.Ind..+Innoculation, data=TWIL_focal)
summary(aov_TWIL_backind)
######CONCLUSION: NO SIGNIFICANCE

#aov_TWIL_weed<-aov(Adjusted.Biomass~Weed_Sum*Treatment, data=TWIL_focal)
#summary(aov_TWIL_weed)
######CONCLUSION: NO SIGNIFICANCE

aov_TWIL_weed<-aov(Adjusted.Biomass~Weed_Sum+Treatment, data=TWIL_focal)
summary(aov_TWIL_weed)
######CONCLUSION: NO SIGNIFICANCE

aov_TWIL_backind<-aov(Adjusted.Biomass~Back.Ind..+Treatment, data=TWIL_focal)
summary(aov_TWIL_backind)
######CONCLUSION: NO SIGNIFICANCE

#aov_TWIL_weed<-aov(Survival~Weed_Sum*Innoculation, data=TWIL_focal)
#summary(aov_TWIL_weed)
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
THIR_focal_lm<-lm(Adjusted.Biomass~Back.Ind..+Weed_Sum+Treatment+Innoculation,data=THIR_focal)
summary(THIR_focal_lm)
######CONCLUSION: NO SIGNIFICANCE

#aov_THIR_weed<-aov(Adjusted.Biomass~Weed_Sum*Innoculation, data=THIR_focal)
#summary(aov_THIR_weed)
######CONCLUSION: NO SIGNIFICANCE

aov_THIR_weed<-aov(Adjusted.Biomass~Weed_Sum+Innoculation, data=THIR_focal)
summary(aov_THIR_weed)
######CONCLUSION: NO SIGNIFICANCE

#aov_THIR_weed<-aov(Adjusted.Biomass~Weed_Sum*Treatment, data=THIR_focal)
#summary(aov_THIR_weed)
######CONCLUSION: TREATMENT ALONE IS MARGINALLY SIGNIFICANT (0.072)

aov_THIR_weed<-aov(Adjusted.Biomass~Weed_Sum+Treatment, data=THIR_focal)
summary(aov_THIR_weed)
######CONCLUSION: TREATMENT ALONE IS MARGINALLY SIGNIFICANT (0.0649)

#aov_THIR_weed<-aov(Survival~Weed_Sum*Innoculation, data=THIR_focal)
#summary(aov_THIR_weed)
######CONCLUSION: NO SIGNIFICANCE

aov_THIR_weed<-aov(Survival~Weed_Sum+Innoculation, data=THIR_focal)
summary(aov_THIR_weed)
######CONCLUSION: NO SIGNIFICANCE

#aov_THIR_weed<-aov(Survival~Weed_Sum*Treatment, data=THIR_focal)
#summary(aov_THIR_weed)
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
  xlim(4.5,10)+
  xlab("Biomass (g)")+
  ylab("Nodule Mass (g)")+
  theme_bw()+
  ggtitle("Background T.willdenovii Biomass and Nodule Mass")
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