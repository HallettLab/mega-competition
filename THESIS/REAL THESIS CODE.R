###5/1 What info from ANOVA models do we get versus the other models? What is coming out as significant? Interesting? Find a package that calculates p-value.
#How long should my introduction be? Scope? How much background or just what is necessary? Ask lab members. 

library(tidyverse)
library(dplyr)
library(lme4)
library(MuMIn)
library(lmerTest)
library(cowplot) ## using this package to make a multipanel figure

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

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

## CE file paths, please save: 
#Focal<- read.csv("THESIS/Focal Individuals.csv")
#Back<- read.csv("THESIS/Background_Individuals.csv")
#Neighbor<- read.csv("THESIS/Neighborhood_Counts.csv")

# DATA CLEANING ####
colnames(Neighbor)
colnames(Focal)

Neighbor_Clean<-Neighbor%>%
  filter(Phytometer!="BRHO", Phytometer!="PLER", Phytometer!="BHRO") ## filtering out BRHO and PLER phytometers
#"!=" does not equal 

Neighbor_Clean$CRCO <- NULL
head(Neighbor_Clean)
#removed CRCO

Neighbor_Clean$Weed_Sum <-rowSums(Neighbor_Clean[ ,c(10,11,12,13,14,15)], na.rm=TRUE)
#summed weed counts

Neighbor_Clean <- Neighbor_Clean %>% 
  mutate(Survival = Phyto.. / 3) 
#created survival percentage

colnames(Neighbor_Clean) <- c("Block", "Plot", "Sub", "Treatment", "Background", "Density","Sample.Name", "Back.Ind", "Phyto","ERBO","FIGA","GAMU","HYGL","SIGA","other","X", "Weed_Sum", "Survival") 
## CE removed the dots after several of the column names, for easier use later on

colnames(Focal) <- c("Sample.Name", "Species", "Innoculation", "Treatment",        
"Block", "Plot", "Quad", "Stem.Count", "Weight.of.Biomass", "Adjusted.Biomass",  "Weight.of.Roots", "Weight.of.Soil", "ARA.Soil")
## also changing focal column names to remove the dots

Focal_All<-left_join(Neighbor_Clean,Focal,by=c("Block", "Plot", "Treatment", "Sample.Name"))
#"c"=character vector (list)

twil <- c("TWIL-I", "TWIL-U")
inoc <- c("TWIL-I", "THIR-I")

Focal_All_Cleaned <- Focal_All %>%
  mutate(Species = ifelse(Sample.Name %in% twil, "TWIL", "THIR"))  %>% ## I'm filling out the species column fully here. This line of code checks the same name column and if it contains TWIL-I or TWIL-U it fills the Species column with TWIL and if it does NOT contain these, it fills in THIR
  mutate(Inoc = ifelse(Sample.Name %in% inoc, "I", "U")) %>%
  mutate(Trt = ifelse(Block == 5, "A", Treatment)) %>% ## making a new treatment column as one is missing a value 
  select(-Treatment, -Innoculation) %>% ## remove the old treatment column 
  mutate(Treatment = Trt, Innoculation = Inoc) %>% ## remake the treatment column with filled in value so that we don't need to change the name in all of the following code.
  select(-Trt, -Inoc) ## get rid of unnecessary columns
  

## here I am selecting the Sample.Name and Species columns and putting them in a new dataframe just so I can check them side by side and make sure my last line of code worked correctly.
check <- Focal_All_Cleaned %>%
  select(Sample.Name, Species, Innoculation)

count_focal<-Focal_All%>%
  group_by(Species, Innoculation, Treatment, Density) %>% 
  summarize(count=n())
## checking the number of replicates
## TWIL-I Ambient -> only species with only one replicate. Other sp/treatment combos have at least 4 replicates. 

## for TWIL-I -> part of the story might be that it survived better in drought treatments than ambient

mean_focal<-Focal_All_Cleaned %>%
  group_by(Species, Innoculation, Treatment) %>% 
  summarize(mean_biomass=mean(Adjusted.Biomass, na.rm = TRUE),sd_biomass=sd(Adjusted.Biomass, na.rm = TRUE))

## remove individuals that didn't survive
Focal_All_Boxplot<-Focal_All_Cleaned %>%
  filter(Survival != 0)

# VISUALIZE Biomass by Trt ####
focals <- ggplot(Focal_All_Boxplot,aes(x=Species, y=Adjusted.Biomass, color=Treatment))+
  geom_boxplot()+
  theme_bw()+
  ylab("Biomass (g)") +
  scale_color_manual(values = c("#008080", "#ca562c"), labels = c("Ambient", "Drought")) 

## setting colors (orange for drought, blue for ambient to make more intuitive) and also labeling the legend more clearly so someone looking at the figure does not wonder what D and A are!
  #ggtitle("Focal Treatment") ## removed title as these are not present on final figures
#USE THIS GRAPH

focals_inoc <- ggplot(Focal_All_Boxplot,aes(x=Species, y=Adjusted.Biomass, color=Treatment))+
  geom_boxplot()+
  theme_bw()+
  ylab("Biomass (g)")+
  facet_wrap(~Innoculation)+
  scale_color_manual(values = c("#008080", "#ca562c"), labels = c("Ambient", "Drought")) +
  theme(legend.position = "none")# + ## removing legend for the multipanel figure
  #ggtitle("Focal Inoculation and Treatment")
#this graph compares treatment and inoculation 

bgs <- ggplot(Back,aes(x=Species, y=Adjusted.Biomass, color=Treatment))+
  geom_boxplot()+
  theme_bw()+
  ylab("Biomass (g)")+
  scale_color_manual(values = c("#008080", "#ca562c"), labels = c("Ambient", "Drought")) +
  theme(legend.position = "none") #+ ## removing legend for the multipanel figure
  #ggtitle("Background Treatment")
#USE THIS GRAPH; in the figure caption be clear which figure is focal data and which is back data...
  #why does this graph look opposite of focal individual. graph? background individuals were growing in competition with itself (intraspecific competition)...even when we compare it to uninoculated focal individuals it's still the opposite relationship
      #maybe trif. individuals are limiting themselves more than other species...

## Combining all three panels into one figure!

## creating half of the plot (getting inoculated focals and backgr plots on the same line)
pt2 <- plot_grid(focals_inoc, bgs,
          labels=c("B","C"))
## creating the other half of the plot
pt1 <- plot_grid(focals, 
                 labels = c("A"))
## combining both parts of the plot to make the final multi-panel figure
plot_grid(pt1, pt2, ncol = 1, nrow = 2)


## feel free to change any of these figure aesthetics :) 




# STATISTICS ####

## THIR focal stats ####
THIR_focal<-Focal_All_Cleaned%>%
  filter(Species=="THIR", Survival != 0)
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

## THIR focal figs ####
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

## TWIL focal stats ####
TWIL_focal<-Focal_All_Cleaned%>%
  filter(Species=="TWIL", Survival != 0)
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
## TWIL focal figs ####
ggplot(TWIL_focal,aes(x=Innoculation, y=Adjusted.Biomass))+
  geom_boxplot()

ggplot(TWIL_focal,aes(x=Treatment, y=Adjusted.Biomass))+
  geom_boxplot()

ggplot(TWIL_focal,aes(x=Innoculation, y=Adjusted.Biomass, color=Treatment))+
  geom_boxplot()


## WEED Stats ####

### TWIL focal biomass ####
TWIL_focal_lm<-lm(Adjusted.Biomass~Back.Ind+Weed_Sum+Treatment+Innoculation,data=TWIL_focal)
summary(TWIL_focal_lm)
######CONCLUSION: NO SIGNIFICANCE
#How do we add covariates to a model (that's how we want to factor weeds into this)

#aov_TWIL_weed<-aov(Adjusted.Biomass~Weed_Sum*Innoculation, data=TWIL_focal)
#summary(aov_TWIL_weed)
######CONCLUSION: NO SIGNIFICANCE

aov_TWIL_weed<-aov(Adjusted.Biomass~Weed_Sum+Innoculation, data=TWIL_focal)
summary(aov_TWIL_weed)
######CONCLUSION: NO SIGNIFICANCE

aov_TWIL_backind<-aov(Adjusted.Biomass~Back.Ind+Innoculation, data=TWIL_focal)
summary(aov_TWIL_backind)
######CONCLUSION: NO SIGNIFICANCE

#aov_TWIL_weed<-aov(Adjusted.Biomass~Weed_Sum*Treatment, data=TWIL_focal)
#summary(aov_TWIL_weed)
######CONCLUSION: NO SIGNIFICANCE

aov_TWIL_weed<-aov(Adjusted.Biomass~Weed_Sum+Treatment, data=TWIL_focal)
summary(aov_TWIL_weed)
######CONCLUSION: NO SIGNIFICANCE

aov_TWIL_backind<-aov(Adjusted.Biomass~Back.Ind+Treatment, data=TWIL_focal)
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
THIR_focal_lm<-lm(Adjusted.Biomass~Back.Ind+Weed_Sum+Treatment+Innoculation,data=THIR_focal)
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


## BACK stats ####

### TWIL ####
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


## visualize the relationship between nodules and biomass
ggplot(TWIL_back, aes(x=Adjusted.Biomass, y=Adjusted.Nodules))+
  geom_point()+
  geom_smooth(method="lm") +
  theme_bw() +
  xlab("Average Background Biomass (g)") +
  ylab("Average Background Nodule mass (g)")
## this is another figure we could use for the thesis!

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

### THIR ####
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

## one comparison we could make here between TWIL and THIR -> there is a negative relationship between biomass and nodules for both species, but the relationship is significant for TWIL but not THIR.

###HELP

## Survival ####
### Survival Figs ####
## Survival by treatment 
ggplot(Focal_All_Cleaned, aes(x=Treatment, y=Survival, fill = Species)) +
  geom_bar(position='dodge', stat='identity')
## this doesn't look right to me yet... not sure why the values of survival are the same between treatments for the species...

ggplot(Focal_All_Cleaned, aes(x=Treatment, y=Survival, fill = Species)) +
  geom_boxplot()

## Survival by background individuals
ggplot(Focal_All_Cleaned, aes(y=Back.Ind, x=Survival, color = Sample.Name)) +
  geom_boxplot()
ggplot(Focal_All_Cleaned, aes(y=Survival, x=Back.Ind)) +
  geom_point()

## look at mean survival
mean_survival <- Focal_All_Cleaned %>%
  group_by(Sample.Name, Species, Innoculation, Treatment) %>%
  summarise(mean_surv = mean(Survival), SE_surv = calcSE(Survival), mean_weeds = mean(Weed_Sum))

ggplot(mean_survival, aes(x=Treatment, y=mean_surv, fill = Sample.Name)) +
  geom_bar(position='dodge', stat='identity') +
  geom_errorbar(aes(ymin = mean_surv - SE_surv, ymax = mean_surv + SE_surv), width = 0.2, position=position_dodge(.9)) +
  theme_bw() +
  ylab("Mean Survival (%)")




## survival depending on weeds or bg individuals
ggplot(mean_survival, aes(x=mean_weeds, y=mean_surv)) +
  #geom_bar(position='dodge', stat='identity') +
  geom_errorbar(aes(ymin = mean_surv - SE_surv, ymax = mean_surv + SE_surv), width = 0.1) +
  geom_point(aes(fill=Sample.Name), 
             colour="black",pch=21, size=4.5) +
  theme_bw() +
  ylab("Mean Survival (%)") + xlab("Mean Weed Density") +
  facet_wrap(~Treatment)


### Survival Stats ####

## NOTE: the way we were filtering using 'Species' gets rid of all of the observations that did not survive, thus we can't use this filter for the survival data. Below I filter using Sample.Name and this is the correct way to filter when we want to look at survival data!

twils <- c("TWIL-I", "TWIL-U") ## make a list of TWIL samples

TWIL_survival <- Focal_All %>%
  filter(Sample.Name %in% twils) ## filter by this list

twil_surv <- aov(Survival~Treatment+Sample.Name + Back.Ind + Weed_Sum, data = TWIL_survival) ## Sample.Name is a stand in for inoculation here, as the Inoculation column does not have values when the phyto didn't survive
summary(twil_surv)

twil_surv_all <- lm(Survival~Treatment+Sample.Name + Back.Ind + Weed_Sum, data = TWIL_survival)
summary(twil_surv_all)

## THIR survival
THIR_survival <- Focal_All %>%
  filter(!(Sample.Name %in% twils))

thir_surv <- aov(Survival~Treatment+Sample.Name + Back.Ind + Weed_Sum, data = THIR_survival) ## Sample.Name is a stand in for inoculation here, as the Inoculation column does not have values when the phyto didn't survive
summary(thir_surv)






# MORE FIGURES ####
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