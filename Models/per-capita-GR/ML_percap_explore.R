source("Models/per-capita-GR/percapita_GR.R")

library(ggplot2)
library(ggfortify)
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(Rmisc)

# Prep Data ####
trait.adult <- read.csv("/users/Marina/Documents/Dropbox/Mega_Competition/Data/Traits/Megacomp_adult-traits.csv")

trait.seed <- read.csv("/users/Marina/Documents/USDA-PostDoc/Projects/Seed-Traits/Data/20230202_Seed-Traits_cleaning.csv")

trait.seed <- trait.seed[,-10] # get rid of height because we have better height data for adults 
colnames(trait.seed)[18] <- "cn.seed"
trait <- merge(trait.seed, trait.adult, by.x = "code", by.y = "Code", all.x = F, all.y = T)
trait[trait$code_4 == "PLER",]$E.S <- 0.8946559
trait[trait$code_4 == "PLER",]$coat.thick <- 0.0104

# Adult trait PCA ####
adults <- c("code_4", "nativity", "growth_form", "FunGroup", "Height_cm", "SLA_cm2.g", "LWC", "CN")
adults.pca <- c("Height_cm", "SLA_cm2.g", "LWC", "CN")
  
pca <- prcomp(trait[, adults.pca], scale = T)
summary(pca)

adults.pca <- cbind(trait, pca$x[,1:4])


autoplot(pca, x = 1, y = 2, data = adults.pca, frame = F, loadings = T, loadings.label = T, label = F, col = "FunGroup", size = 2) +
  theme_classic() +
  #geom_text(aes(label = code, col = Rating)) +
  #stat_ellipse(aes(group = group)) + 
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    legend.title = element_blank())
  # ) +
  # scale_color_manual(values = c(adhesive = "#1B9E77", ant = "red4", unassisted = "darkgoldenrod3", ingestion = "#7570B3", wind = "#F17236"))



# Seed trait PCA ####
seeds <- c("code_4", "nativity", "growth_form", "FunGroup", "ldd2", "wing.loading", "coat.perm", "E.S", "coat.thick",  "mass.mg", "set.time.mpsec", "shape",  "size", "appendage.type", "prop.C", "prop.N", "cn.seed")
  
seeds.pca <- c("wing.loading", "coat.perm", "E.S", "coat.thick",  "mass.mg", "set.time.mpsec", "shape",  "size", "cn.seed")

pca <- prcomp(trait[, seeds.pca], scale = T)
summary(pca)

seeds.pca <- cbind(trait, pca$x[,1:4])


autoplot(pca, x = 1, y = 2, data = seeds.pca, frame = F, loadings = T, loadings.label = T, label = F, col = "FunGroup", size = 2) +
  theme_classic() +
  #geom_text(aes(label = code, col = Rating)) +
  #stat_ellipse(aes(group = group)) + 
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    legend.title = element_blank())


# Together PCA ####
all.pca <- c("Height_cm", "SLA_cm2.g", "LWC", "CN", "coat.perm", "E.S", "coat.thick",  "mass.mg", "set.time.mpsec", "shape",  "size", "cn.seed")

pca <- prcomp(trait[, all.pca], scale = T)
summary(pca)

all.pca <- cbind(trait, pca$x[,1:4])

autoplot(pca, x = 1, y = 2, data = all.pca, frame = F, loadings = T, loadings.label = T, label = T, col = "FunGroup", size = 1) +
  theme_classic() +
  #geom_text(aes(label = code, col = Rating)) +
  #stat_ellipse(aes(group = group)) + 
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    legend.title = element_blank())

percap.growth <- merge(percap.growth, all.pca, by.x = "phyto", by.y = "code_4")

percap.growth <- filter(percap.growth, percap.growthrate > 0)

percap.growth$phyto.FunGroup <- paste(percap.growth$nativity, percap.growth$growth_form, sep = " ")

percap.growth <- merge(percap.growth, trait[,c(21,22,25)], by.x = "bkgrd", by.y = "code_4")

percap.growth$bkgrd.FunGroup <- paste(percap.growth$nativity.y, percap.growth$growth_form.y, sep = " ")

