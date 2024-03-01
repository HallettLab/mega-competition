## the purpose of this script is to explore the greenhouse trait data and get PCA values for each species

# Set up ####
library(ggfortify)
library(ggpubr)
library(tidyverse)

## Read in data ####
# specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Greenhouse_Traits/")){
  # Carmen
  lead.traits <- "/Users/carme/Dropbox (University of Oregon)/Greenhouse_Traits/Data/Cleaned_Data/"
  
} else {
  # Marina
  #lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Processing/Background-Processing/Background-Processing_entered/"
} 


## specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Traits/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Traits/"
  
} else {
  # Marina
  lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Traits/"
} 

## load trait data
trait.seed <- read.csv(paste0(lead, "20230202_Seed-Traits_cleaning.csv"))

traits <- read.csv(paste0(lead.traits, "GreenhouseTraits.csv"))

# Clean ####
## AG/BG ####
## subset mega-comp species
## make vector of species
MC.sp <- c("LOPU", "AMME", "ANAR", "BRHOp", "BRNIp", "CESO", "GITRp", "LENIp", "LOMU", "MAELp", "MICA", "PLERp", "PLNO", "TACA", "TRHI", "TRWIp")
  ## "AVEBAR", "ERBO", "CLPUp",

nonnative <- c("ANAR", "BRHO", "BRNI", "CESO", "LOMU", "TACA", "THIR")
grass <- c("BRHO", "TACA", "LOMU")
legume <- c("ACAM", "THIR", "TWIL")

facilitator <- c("ACAM", "AMME", "MAEL", "PLNO", "THIR", "TWIL")

## filter
MC.traits <- traits %>%
  filter(ID %in% MC.sp,
         !is.na(RMF), ## remove NAs
         !is.na(LDMC)) %>% ## remove NAs
  mutate(phyto = ifelse(ID == "LOPU", "ACAM", 
                        ifelse(ID == "BRHOp", "BRHO",
                               ifelse(ID == "BRNIp", "BRNI", 
                                      ifelse(ID == "GITRp", "GITR",
                                             ifelse(ID == "LENIp", "LENI",
                                                    ifelse(ID == "MAELp", "MAEL",
                                                           ifelse(ID == "PLERp", "PLER", 
                                                                  ifelse(ID == "TRHI", "THIR", 
                                                                         ifelse(ID == "TRWIp","TWIL", ID)))))))))) %>%
  mutate(origin = ifelse(phyto %in% nonnative, "non-native", "native"),
         funct_group = ifelse(phyto %in% grass, "grass",
                              ifelse(phyto %in% legume, "legume", "forb")),
         fg_origin = paste(origin, funct_group, sep = "_"),
         interaction_type = ifelse(phyto %in% facilitator, "facilitative", "competitive"))


colnames(MC.traits) <- c("Taxon", "ID", "Rep", "Date.harvest", "Height.cm", "Fresh.leaf.mass.g", "Dry.leaf.mass.g", "LDMC", "Leaf.Area.cm2", "SLA.cm2.g", "Shoot.dry.biomass.g", "Root.dry.biomass.g", "Total.biomass.g", "RMF", "Root.volume.cm3", "Root.density.g.cm3", "Coarse.root.diameter.mm", "Length.mm", "Fine.root.length.mm", "Coarse.root.length.mm", "Coarse.root.specific.length.cm.g", "Fine.root.specific.length.cm.g", "Proportion.fine.roots", "phyto", "origin", "funct_group", "fg_origin", "interaction_type")

## Seed Data ####
trait.seed <- trait.seed[,-10] # get rid of height because we have better height data for adults 
colnames(trait.seed)[17] <- "cn.seed"

#trait <- merge(trait.seed, trait.adult, by.x = "code", by.y = "Code", all.x = F, all.y = T)

MC.sp2 <- c("ACMAME", "AMSMEN", "ANAARV", "BROHOR", "BRANIG", "CENSOL", "GILTRI", "LEPNIT", "FESPER", "MADELE", "MICCAL", "PLAERE", "PLANOT", "ELYCAP", "TRIHIR", "TRIWIL")

trait.seed2 <- trait.seed %>%
  filter(code %in% MC.sp2)
  
trait.seed2[trait.seed2$code == "PLAERE",]$E.S <- 0.8946559
trait.seed2[trait.seed2$code == "PLAERE",]$coat.thick <- 0.0104


# PCAs ####
## AG - BG traits ####
all.traits <- c("Height.cm", "LDMC", "SLA.cm2.g", "RMF", "Root.density.g.cm3", "Coarse.root.specific.length.cm.g", "Fine.root.specific.length.cm.g", "Proportion.fine.roots")

MC.pca <- prcomp(MC.traits[,all.traits], scale = T)
summary(MC.pca)

MC.pca.ID <- cbind(MC.traits, MC.pca$x[,1:8])

autoplot(MC.pca, x = 1, y = 2, data = MC.pca.ID, frame = F, loadings = T, loadings.label = T, label = F, col = "fg_origin", size = 1.75, loadings.colour = "black",
         loadings.label.colour="black", loadings.label.repel=TRUE) +
  theme_classic() +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  
  #geom_text(aes(label = code, col = Rating)) +
   #stat_ellipse(aes(group = phyto, col = phyto)) + 
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    legend.title = element_blank())

#ggsave("analyses/traits/preliminary_figures/pca_alltraits_fg.png", width = 7, height = 4)

autoplot(MC.pca, x = 1, y = 2, data = MC.pca.ID, frame = F, loadings = T, loadings.label = T, label = F, col = "interaction_type", size = 1.75, loadings.colour = "black",
         loadings.label.colour="black", loadings.label.repel=TRUE) +
  theme_classic() +
  scale_color_manual(values = c("#5D69B1","#52BCA3", "#E58606", "#99C945","#CC3A8E")) +
  
  #geom_text(aes(label = code, col = Rating)) +
  stat_ellipse(aes(group = interaction_type, col = interaction_type)) + 
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    legend.title = element_blank())
#E58606,#5D69B1,#52BCA3,#99C945,#CC61B0,#24796C,#DAA51B,#2F8AC4,#764E9F,#ED645A,#CC3A8E,#A5AA99
#ggsave("analyses/traits/preliminary_figures/pca_alltraits_interaction_type.png", width = 7, height = 4)

## Seed Trait PCAs ####
seeds <- c("code_4", "nativity", "growth_form", "FunGroup", "ldd2", "wing.loading", "coat.perm", "E.S", "coat.thick",  "mass.mg", "set.time.mpsec", "shape",  "size", "appendage.type", "prop.C", "prop.N", "cn.seed")

seeds.pca <- c("wing.loading", "coat.perm", "E.S", "coat.thick",  "mass.mg", "set.time.mpsec", "shape",  "size", "cn.seed")

pca <- prcomp(trait.seed2[, seeds.pca], scale = T)
summary(pca)

seeds.pca <- cbind(trait.seed2, pca$x[,1:4])


autoplot(pca, x = 1, y = 2, data = seeds.pca, frame = F, loadings = T, loadings.label = T, label = F,  size = 2, col = "Species", loadings.colour = "black", loadings.label.colour="black") + #col = "FunGroup",
  theme_classic() +
  #geom_text(aes(label = code, col = Rating)) +
  #stat_ellipse(aes(group = group)) + 
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    legend.title = element_blank())

#ggsave("analyses/traits/preliminary_figures/seed-trait_pca.png", height = 4, width = 7)
