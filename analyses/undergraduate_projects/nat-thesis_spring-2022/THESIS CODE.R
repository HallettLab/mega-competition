library(tidyverse)
library(minpack.lm)
library(nlstools)
library(grid)
library(gridExtra)

setwd("~/Desktop/Repositories/THESIS/")  

Focal<- read.csv("Focal_Individuals.csv")
Back<- read.csv("Background_Individuals.csv")
Neighbor<- read.csv("Neighborhood_Counts.csv")

#means and stand.devs
#what significance tests should i run
###ANOVA: good for categorical comparisons 
#FOCAL: TWIL-U and TWIL-I D/A biomass, THIR-U and THIR-I D/A biomass
#BACK: TWIL-U D/A biomass and nodules, THIR-U D/A biomass and nodules
#nodules=continuous variable---scatter plot
#NEIGHBOR:  TWIL-U and TWIL-I D/A biomass w/ weeds OR background species, THIR-U and THIR-I D/A biomass w/ weeds OR background specie
#make graphs---box-and-whisker plot graphs to start 