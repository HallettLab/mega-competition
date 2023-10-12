
# Set up Env ####
library(ggfortify)
library(tidyverse)
library(stats)
theme_set(theme_bw())

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

# Read in Data ####
## per cap data
source("Models/CW/per-capita-GR/percapita_GR.R")

## traits data
## specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Traits/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Traits/"
  
} else {
  # Marina
  lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Traits/"
} 

trait.adult <- read.csv(paste0(lead, "Megacomp_adult-traits.csv")) %>%
  mutate(phyto = code_4, 
         bkgrd = code_4) 

trait.seed <- read.csv(paste0(lead, "20230202_Seed-Traits_cleaning.csv"))


# Clean Data ####
trait.seed <- trait.seed[,-10] # get rid of height because we have better height data for adults 
colnames(trait.seed)[17] <- "cn.seed"
trait <- merge(trait.seed, trait.adult, by.x = "code", by.y = "Code", all.x = F, all.y = T)
trait[trait$code_4 == "PLER",]$E.S <- 0.8946559
trait[trait$code_4 == "PLER",]$coat.thick <- 0.0104

trait <- trait %>%
  mutate(FunGroup = paste(nativity, growth_form, sep = "_"))
## add in functional group column


# Adult trait PCA ####
adults <- c("code_4", "nativity", "growth_form", "FunGroup", "Height_cm", "SLA_cm2.g", "LWC", "CN")
adults.pca <- c("Height_cm", "SLA_cm2.g", "LWC", "CN")
## the columns that actually go in the pca

pca <- prcomp(trait[, adults.pca], scale = T) ## give pca function the relevant columns of adult traits, but no id info
summary(pca)

adults.pca <- cbind(trait, pca$x[,1:4])


autoplot(pca, x = 1, y = 2, data = adults.pca, frame = F, loadings = T, loadings.label = T, label = F, col = "FunGroup", size = 2) +
  theme_classic() +
  #geom_text(aes(label = code, col = Rating)) +
  #stat_ellipse(aes(group = group)) + 
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    legend.title = element_blank())
# ) +
# scale_color_manual(values = c(adhesive = "#1B9E77", ant = "red4", unassisted = "darkgoldenrod3", ingestion = "#7570B3", wind = "#F17236"))

#ggsave("adult-trait_pca.png", height = 3, width = 5)


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

ggsave("seed-trait_pca.png", height = 3, width = 6)

# Together PCA ####
all.pca <- c("Height_cm", "SLA_cm2.g", "LWC", "CN", "coat.perm", "E.S", "coat.thick",  "mass.mg", "set.time.mpsec", "shape",  "size", "cn.seed")

pca <- prcomp(trait[, all.pca], scale = T)
summary(pca)

all.pca <- cbind(trait, pca$x[,1:4])

autoplot(pca, x = 1, y = 2, data = all.pca, frame = F, loadings = T, loadings.label = T, label = F, col = "FunGroup", size = 1) +
  theme_classic() +
  #geom_text(aes(label = code, col = Rating)) +
  #stat_ellipse(aes(group = group)) + 
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    legend.title = element_blank())



# Merge PCA & percap GR####
percap.w.pca <- left_join(percap.growth, adults.pca, by = "phyto") %>%
  mutate(bkgrd = bkgrd.x,
         phyto.FunGroup = FunGroup,
         phyto.PC1 = PC1,
         phyto.PC2 = PC2, 
         phyto.PC3 = PC3,
         phyto.PC4 = PC4) %>%
  select(unique.ID, treatment, block, plot, sub, phyto.unique, bkgrd, dens, dens.num, phyto, phyto.n.indiv, phyto.seed.in, phyto.seed.out, percap.growthrate, phyto.FunGroup:phyto.PC4)

percap.w.pca.bg <- left_join(percap.w.pca, adults.pca, by = "bkgrd") %>%
  mutate(phyto = phyto.x,
         bkgrd.FunGroup = FunGroup,
         bkgrd.PC1 = PC1,
         bkgrd.PC2 = PC2, 
         bkgrd.PC3 = PC3,
         bkgrd.PC4 = PC4) %>%
  select(unique.ID, treatment, block, plot, sub, phyto.unique, bkgrd, dens, dens.num, phyto, phyto.n.indiv, phyto.seed.in, phyto.seed.out, percap.growthrate, phyto.FunGroup:phyto.PC4, bkgrd.FunGroup:bkgrd.PC4) %>%
  filter(dens != "none")

# Explore ####
sum.percap <- percap.w.pca.bg %>%
  group_by(treatment, phyto.FunGroup) %>%
  summarise(mean_percap_GR = mean(percap.growthrate),
            se_percap_GR = calcSE(percap.growthrate))

ggplot(sum.percap, aes(x=treatment, y=mean_percap_GR, color = phyto.FunGroup)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_percap_GR - se_percap_GR, ymax = mean_percap_GR + se_percap_GR), width = 0.25)

#ggsave("GR_by_treatment.png", width = 4, height = 2)

sum.percap.bkgrds <- percap.w.pca.bg %>%
  group_by(treatment, phyto.FunGroup, bkgrd.FunGroup) %>%
  summarise(mean_percap_GR = mean(percap.growthrate),
            se_percap_GR = calcSE(percap.growthrate))

ggplot(sum.percap.bkgrds, aes(x=treatment, y=mean_percap_GR, color = bkgrd.FunGroup)) +
  geom_point(size = 2) +
  #geom_line(group_by(treatment)) +
  geom_errorbar(aes(ymin = mean_percap_GR - se_percap_GR, ymax = mean_percap_GR + se_percap_GR), width = 0.25) +
  facet_wrap(~phyto.FunGroup)

ggsave("FG_phytos_bgs.png", width = 6, height = 3)

sum.percap.bkgrds.pca <- percap.w.pca.bg %>%
  group_by(treatment, phyto.FunGroup, bkgrd.FunGroup) %>%
  summarise(mean_percap_GR = mean(percap.growthrate),
            se_percap_GR = calcSE(percap.growthrate),
            meanPC1.phyto = mean(phyto.PC1), 
            meanPC2.phyto = mean(phyto.PC2),
            meanPC3.phyto = mean(phyto.PC3), 
            meanPC4.phyto = mean(phyto.PC4),
            meanPC1.bkgrd = mean(bkgrd.PC1), 
            meanPC2.bkgrd = mean(bkgrd.PC2),
            meanPC3.bkgrd = mean(bkgrd.PC3), 
            meanPC4.bkgrd = mean(bkgrd.PC4))

ggplot(sum.percap.bkgrds.pca, aes(x=meanPC1.phyto, y=mean_percap_GR, color = phyto.FunGroup)) +
  geom_point()

ggplot(sum.percap.bkgrds.pca, aes(x=meanPC2.phyto, y=mean_percap_GR, color = phyto.FunGroup)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin=mean_percap_GR-se_percap_GR, ymax = mean_percap_GR+se_percap_GR), width = 0.1) +
  facet_wrap(~treatment*bkgrd.FunGroup)



ggplot(percap.w.pca.bg, aes(x= phyto.PC2, y= percap.growthrate, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~phyto.FunGroup*bkgrd.FunGroup, scales = "free")

ggplot(percap.w.pca.bg, aes(x= phyto.PC1, y= percap.growthrate, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~phyto.FunGroup*bkgrd.FunGroup, scales = "free")

ggplot(percap.w.pca.bg, aes(x=percap.growthrate)) +
  geom_histogram()

ggplot(percap.w.pca.bg[percap.w.pca.bg$phyto.FunGroup == "native_forb",], aes(x=percap.growthrate)) +
  geom_histogram()

ggplot(percap.w.pca.bg[percap.w.pca.bg$phyto.FunGroup == "exotic_forb",], aes(x=percap.growthrate)) +
  geom_histogram()

ggplot(percap.w.pca.bg[percap.w.pca.bg$phyto.FunGroup == "exotic_grass",], aes(x=percap.growthrate)) +
  geom_histogram()

ggplot(percap.w.pca.bg[percap.w.pca.bg$phyto.FunGroup == "exotic_grass",], aes(x= phyto.PC2, y= percap.growthrate, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~bkgrd.FunGroup) +
  scale_color_manual(values = c("#003366", "#FFA630")) +
  ggtitle("Exotic Grass Phytos by FG background")

#ggsave("EG_by_FG_bgs.png", width = 5, height = 2.75)

ggplot(percap.w.pca.bg[percap.w.pca.bg$phyto.FunGroup == "exotic_forb",], aes(x= phyto.PC2, y= percap.growthrate, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~bkgrd.FunGroup) +
  scale_color_manual(values = c("#003366", "#FFA630")) +
  ggtitle("Exotic Forb Phytos by FG background")

#ggsave("EF_by_FG_bgs.png", width = 5, height = 2.75)


ggplot(percap.w.pca.bg[percap.w.pca.bg$phyto.FunGroup == "native_forb",], aes(x= phyto.PC2, y= percap.growthrate, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~bkgrd.FunGroup) +
  scale_color_manual(values = c("#003366", "#FFA630")) +
  ggtitle("Native Forb Phytos by FG background")

#ggsave("NF_by_FG_bgs.png", width = 5, height = 2.75)

