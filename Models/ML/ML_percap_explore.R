source("Models/per-capita-GR/percapita_GR.R")

library(tidyverse)
library(ggplot2)
library(ggfortify)
library(lme4)
library(lmerTest)
library(emmeans)
#library(Rmisc)

rm(allo.df, test, tmp.controls, tmp.repeated.reps, with.controls, unique.key, tmp.block, tmp.rep, tmp.sp)

# Prep Trait Data ####
trait.adult <- read.csv("/users/Marina/Documents/Dropbox/Mega_Competition/Data/Traits/Megacomp_adult-traits.csv")

trait.seed <- read.csv("/users/Marina/Documents/USDA-PostDoc/Projects/Seed-Traits/Data/20230202_Seed-Traits_cleaning.csv")

trait.seed <- trait.seed[,-10] # get rid of height because we have better height data for adults 
colnames(trait.seed)[17] <- "cn.seed"
trait <- merge(trait.seed, trait.adult, by.x = "code", by.y = "Code", all.x = F, all.y = T)
trait[trait$code_4 == "PLER",]$E.S <- 0.8946559
trait[trait$code_4 == "PLER",]$coat.thick <- 0.0104

# Adult trait PCA ####
adults <- c("code_4", "nativity", "growth_form", "FunGroup", "Height_cm", "SLA_cm2.g", "LWC", "CN")
adults.pca <- c("Height_cm", "SLA_cm2.g", "LWC", "CN")
  
pca <- prcomp(trait[, adults.pca], scale = T)
summary(pca)

adults.pca <- cbind(trait, pca$x[,1:4])

adults.pca$FunGroup <- paste(adults.pca$nativity, adults.pca$growth_form, sep = " ")

autoplot(pca, x = 1, y = 2, data = adults.pca, frame = F, loadings = T, loadings.label = T, label = F, col = "FunGroup", size = 2) +
  theme_classic() +
  #geom_text(aes(label = code, col = Rating)) +
  #stat_ellipse(aes(group = group)) + 
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    legend.title = element_blank())

# Seed trait PCA ####
# seeds <- c("code_4", "nativity", "growth_form", "FunGroup", "ldd2", "wing.loading", "coat.perm", "E.S", "coat.thick",  "mass.mg", "set.time.mpsec", "shape",  "size", "appendage.type", "prop.C", "prop.N", "cn.seed")
#   
# seeds.pca <- c("wing.loading", "coat.perm", "E.S", "coat.thick",  "mass.mg", "set.time.mpsec", "shape",  "size", "cn.seed")
# 
# pca <- prcomp(trait[, seeds.pca], scale = T)
# summary(pca)
# 
# seeds.pca <- cbind(trait, pca$x[,1:4])
# 
# 
# autoplot(pca, x = 1, y = 2, data = seeds.pca, frame = F, loadings = T, loadings.label = T, label = F, col = "FunGroup", size = 2) +
#   theme_classic() +
#   #geom_text(aes(label = code, col = Rating)) +
#   #stat_ellipse(aes(group = group)) + 
#   theme(
#     panel.border = element_rect(colour = "black", fill = NA, size = 1),
#     legend.title = element_blank())


# Together PCA ####
# all.pca <- c("Height_cm", "SLA_cm2.g", "LWC", "CN", "coat.perm", "E.S", "coat.thick",  "mass.mg", "set.time.mpsec", "shape",  "size", "cn.seed")
# 
# pca <- prcomp(trait[, all.pca], scale = T)
# summary(pca)
# 
# all.pca <- cbind(trait, pca$x[,1:4])
# all.pca$FunGroup <- paste(all.pca$nativity, all.pca$growth_form, sep = " ")
# 
# autoplot(pca, x = 1, y = 2, data = all.pca, frame = F, loadings = T, loadings.label = T, label = T, col = "FunGroup", size = 1) +
#   theme_classic() +
#   #geom_text(aes(label = code, col = Rating)) +
#   #stat_ellipse(aes(group = group)) + 
#   theme(
#     panel.border = element_rect(colour = "black", fill = NA, size = 1),
#     legend.title = element_blank())

# Prep GR data (Adults) ####
percap.growth <- merge(percap.growth, adults.pca, by.x = "phyto", by.y = "code_4")

percap.growth[percap.growth$percap.growthrate == 0,]$percap.growthrate <- 1

#percap.growth <- filter(percap.growth, percap.growthrate > 0) # the ones that are zero are ones that died before reproduction

percap.growth$phyto.FunGroup <- paste(percap.growth$nativity, percap.growth$growth_form, sep = " ")

# now remerge but with background species to get background fungroups
percap.growth <- merge(percap.growth, adults.pca[,c(21,22,25)], by.x = "bkgrd", by.y = "code_4")

percap.growth$bkgrd.FunGroup <- paste(percap.growth$nativity.y, percap.growth$growth_form.y, sep = " ")

# separate out controls from other backgrounds
percap.growth.controls <- filter(percap.growth, dens == "none")
percap.growth.nocontrols <- filter(percap.growth, dens != "none")

percap.growth.controls <- unique(percap.growth.controls[,-1])

# Explore: controls & FunGroup ####

ggplot(percap.growth.controls, aes(x = treatment, y = log(percap.growthrate), col = phyto.FunGroup)) +
  geom_boxplot()

percap.sum <- summarySE(percap.growth.controls, measurevar = "percap.growthrate", groupvars = c("treatment", "phyto.FunGroup"), na.rm = T)

ggplot(percap.sum, aes(x = treatment, y = log(percap.growthrate), col = phyto.FunGroup, group = phyto.FunGroup)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = log(percap.growthrate - se), ymax = log(percap.growthrate + se), width = 0.2))

# Explore: controls & Traits ####
ggplot(percap.growth.controls, aes(x = PC1, y = log(percap.growthrate), col = treatment, group = treatment)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(percap.growth.controls, aes(x = PC2, y = log(percap.growthrate), col = treatment, group = treatment)) +
  geom_point() +
  geom_smooth(method = "lm")


# Explore: bkgds & fungroup ####
percap.growth.nocontrols <- percap.growth.nocontrols[,c(62,65,1:61,63:64)]

ggplot(percap.growth.nocontrols, aes(x = treatment, y = log(percap.growthrate), col = bkgrd.FunGroup)) +
  geom_boxplot() +
  facet_wrap(~phyto.FunGroup)

percap.sum <- summarySE(percap.growth.nocontrols[percap.growth.nocontrols$dens == "H",], measurevar = "percap.growthrate", groupvars = c("treatment", "phyto.FunGroup", "bkgrd.FunGroup"), na.rm = T)

ggplot(percap.sum, aes(x = treatment, y = log(percap.growthrate), col = bkgrd.FunGroup, group = bkgrd.FunGroup)) +
  geom_point() +
  geom_line() + 
  geom_errorbar(aes(ymin = log(percap.growthrate - se), ymax = log(percap.growthrate + se), width = 0.2)) +
  facet_wrap(~phyto.FunGroup)

percap.sum <- summarySE(percap.growth.nocontrols[percap.growth.nocontrols$dens == "L",], measurevar = "percap.growthrate", groupvars = c("treatment", "phyto.FunGroup", "bkgrd.FunGroup"), na.rm = T)

ggplot(percap.sum, aes(x = treatment, y = log(percap.growthrate), col = bkgrd.FunGroup, group = bkgrd.FunGroup)) +
  geom_point() +
  geom_line() + 
  geom_errorbar(aes(ymin = log(percap.growthrate - se), ymax = log(percap.growthrate + se), width = 0.2)) +
  facet_wrap(~phyto.FunGroup)

ggplot(percap.sum, aes(x = dens, y = log(percap.growthrate), col = treatment, group = treatment)) +
  geom_point() +
  geom_line() + 
  geom_errorbar(aes(ymin = log(percap.growthrate - se), ymax = log(percap.growthrate + se), width = 0.2)) +
  facet_wrap(~bkgrd.FunGroup + phyto.FunGroup)

percap.sp.sum <- summarySE(percap.growth.nocontrols, measurevar = "percap.growthrate", groupvars = c("treatment", "phyto", "phyto.FunGroup", "bkgrd.FunGroup"), na.rm = T)

ggplot(percap.sp.sum, aes(x = treatment, y = log(percap.growthrate), col = bkgrd.FunGroup, group = bkgrd.FunGroup)) +
  geom_point() +
  geom_line() + 
  geom_errorbar(aes(ymin = log(percap.growthrate - se), ymax = log(percap.growthrate + se), width = 0.2)) +
  facet_wrap(~phyto.FunGroup + phyto) # CESO acting up

# native forbs have a strong response to drought than grasses or exotic forbs, regardless of who they are competing with
# exotic grasses have a negative response to drought, though less strong than forbs (def natives, exotic forb error bars are high), competition with other grasses has a stronger effect than competition with forbs, but no interaction with drought
# exotic forbs have a naegative response to drought, no differences between who they are competing with


# Explore: bkgds & Traits ####
ggplot(percap.growth.nocontrols, aes(x = PC1, y = log(percap.growthrate))) +
  geom_point() +
  facet_wrap(~bkgrd.FunGroup) +
  geom_smooth(method = "lm") 

ggplot(percap.growth.nocontrols, aes(x = PC2, y = log(percap.growthrate))) +
  geom_point() +
  facet_wrap(~bkgrd.FunGroup) +
  geom_smooth(method = "lm")

ggplot(percap.growth.nocontrols, aes(x = SLA_cm2.g, y = log(percap.growthrate))) +
  geom_point() +
  facet_wrap(~bkgrd.FunGroup) +
  geom_smooth(method = "lm")

ggplot(percap.growth.nocontrols, aes(x = Height_cm, y = log(percap.growthrate), group = treatment, col = treatment)) +
  #geom_point() +
  facet_wrap(~bkgrd.FunGroup) +
  geom_smooth(method = "lm")

# Exploratory models ####
m1 <- lmer(log(percap.growthrate) ~ treatment * phyto.FunGroup + (1|block), data = percap.growth.controls)
plot(fitted(m1), resid(m1))
qqnorm(resid(m1))
qqline(resid(m1), col = 2, lwd = 2, lty = 2)
anova(m1) 

pairs(emmeans(m1, ~ treatment * phyto.FunGroup), adjust = "BH") # effect of treatment, fungroup, and interaction!! this doesnt include background fungroup, but good first pass, shows that drought is having an effect on percap growth rates but not on grasses (!), and a stronger effect on native forbs than on exotic forbs

m1 <- lmer(log(percap.growthrate) ~ treatment * phyto.FunGroup * bkgrd.FunGroup + (1|block), data = percap.growth.nocontrols)
plot(fitted(m1), resid(m1))
qqnorm(resid(m1))
qqline(resid(m1), col = 2, lwd = 2, lty = 2)
anova(m1) 

pairs(emmeans(m1, ~ treatment * bkgrd.FunGroup * phyto.FunGroup), adjust = "BH")




ggplot(percap.sum, aes(x = treatment, y = log(percap.growthrate), col = bkgrd.FunGroup, group = bkgrd.FunGroup)) +
  geom_point() +
  geom_line() + 
  geom_errorbar(aes(ymin = log(percap.growthrate - se), ymax = log(percap.growthrate + se), width = 0.2)) +
  facet_wrap(~phyto.FunGroup)


percap.sum <- summarySE(percap.growth.controls, measurevar = "percap.growthrate", groupvars = c("treatment", "phyto.FunGroup"), na.rm = T)

ggplot(percap.sum, aes(x = treatment, y = percap.growthrate, col = phyto.FunGroup, group = phyto.FunGroup)) +
  geom_point() +
  geom_line() + 
  geom_errorbar(aes(ymin = percap.growthrate - se, ymax = percap.growthrate + se, width = 0.2))

### 
percap.growth <- merge(percap.growth, adult.pca, by.x = "phyto", by.y = "code_4")

percap.growth <- filter(percap.growth, percap.growthrate > 0)

percap.growth$phyto.FunGroup <- paste(percap.growth$nativity, percap.growth$growth_form, sep = " ")

percap.growth <- merge(percap.growth, trait[,c(21,22,25)], by.x = "bkgrd", by.y = "code_4")

percap.growth$bkgrd.FunGroup <- paste(percap.growth$nativity.y, percap.growth$growth_form.y, sep = " ")


# # Explore adults ####
# percap.growth.adults <- merge(percap.growth, adults.pca[,-47], by.x = "phyto", by.y = "code_4")
# 
# percap.growth.adults <- filter(percap.growth.adults, percap.growthrate > 0)
# 
# percap.growth.adults$phyto.FunGroup <- paste(percap.growth.adults$nativity, percap.growth.adults$growth_form, sep = " ")
# 
# percap.growth.adults <- merge(percap.growth.adults, trait[,c(20,21,24)], by.x = "bkgrd", by.y = "code_4")
# 
# percap.growth.adults$bkgrd.FunGroup <- paste(percap.growth.adults$nativity.y, percap.growth.adults$growth_form.y, sep = " ")
# 
# ggplot(percap.growth.adults, aes(x = PC1, y = log(percap.growthrate), col = treatment, group = treatment)) +
#   geom_point() +
#   facet_wrap(~bkgrd.FunGroup) +
#   geom_smooth(method = "lm") 
# 
# 
# m1 <- lmer(log(percap.growthrate) ~ treatment * PC1 + (1|block), data = percap.growth.adults)
# plot(fitted(m1), resid(m1))
# qqnorm(resid(m1))
# qqline(resid(m1), col = 2, lwd = 2, lty = 2)
# summary(m1) 
# anova(m1)
# 
# ggplot(percap.growth.adults, aes(x = PC2, y = log(percap.growthrate), col = treatment, group = treatment)) +
#   geom_point() +
#   geom_smooth(method = "lm") 
# 
# # drought decreases growth rates
# # growth rates are higher in taller/low SLA species
# # ineraction between drought and PC2 in that taller, low SLA species respond more negatively to drough than shorter, high SLA species (odd), but I also dont love these SLA estimates and this axis is way more strongly correalted with height.
# m1 <- lmer(log(percap.growthrate) ~ treatment * PC2 + (1|block), data = percap.growth.adults)
# plot(fitted(m1), resid(m1))
# qqnorm(resid(m1))
# qqline(resid(m1), col = 2, lwd = 2, lty = 2)
# summary(m1) 
# anova(m1)
# 
# ggplot(percap.growth.adults, aes(x = PC2, y = log(percap.growthrate), col = treatment, group = treatment)) +
#   geom_point() +
#   facet_wrap(~bkgrd.FunGroup) +
#   geom_smooth(method = "lm") 
