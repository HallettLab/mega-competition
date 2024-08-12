## the purpose of this script is to explore the greenhouse trait data and get PCA values for each species

# Set up ####
library(tidyverse)
library(ggfortify)
library(ggpubr)

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

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

adult_trait_field_dat = read.csv(paste0(lead, "Megacomp_adult-traits.csv"))

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
  mutate(origin = ifelse(phyto %in% nonnative, "Non-native", "Native"),
         funct_group = ifelse(phyto %in% grass, "Grass",
                              ifelse(phyto %in% legume, "Legume", "Forb")),
         fg_origin = paste(origin, funct_group, sep = "_"),
         interaction_type = ifelse(phyto %in% facilitator, "facilitative", "competitive"))


colnames(MC.traits) <- c("Taxon", "ID", "Rep", "Date.harvest", "Height.cm", "Fresh.leaf.mass.g", "Dry.leaf.mass.g", "LDMC", "Leaf.Area.cm2", "SLA.cm2.g", "Shoot.dry.biomass.g", "Root.dry.biomass.g", "Total.biomass.g", "RMF", "Root.volume.cm3", "Root.density.g.cm3", "Coarse.root.diameter.mm", "Length.mm", "Fine.root.length.mm", "Coarse.root.length.mm", "Coarse.root.specific.length.cm.g", "Fine.root.specific.length.cm.g", "Proportion.fine.roots", "phyto", "origin", "funct_group", "fg_origin", "interaction_type")

height = adult_trait_field_dat %>%
  select(code_4, Height_cm) %>%
  mutate(phyto = code_4)

MC.traits2 = left_join(MC.traits, height[2:3], by = c("phyto"))

## Seed Data ####
trait.seed <- trait.seed[,-10] # get rid of height because we have better height data for adults 
colnames(trait.seed)[17] <- "cn.seed"

MC.sp2 <- c("ACMAME", "AMSMEN", "ANAARV", "BROHOR", "BRANIG", "CENSOL", "GILTRI", "LEPNIT", "FESPER", "MADELE", "MICCAL", "PLAERE", "PLANOT", "ELYCAP", "TRIHIR", "TRIWIL")

trait.seed2 <- trait.seed %>%
  filter(code %in% MC.sp2)
  
trait.seed2[trait.seed2$code == "PLAERE",]$E.S <- 0.8946559
trait.seed2[trait.seed2$code == "PLAERE",]$coat.thick <- 0.0104

seed.mass <- trait.seed2 %>%
  select(Species, code, mass.mg)

# Run PCAs ####
# PCAs ####
## AG - BG traits ####
all.traits <- c("Height_cm", "LDMC", "SLA.cm2.g", "RMF",  "Coarse.root.specific.length.cm.g", "Proportion.fine.roots", "Coarse.root.diameter.mm")

temp <- MC.traits2 %>%
  select(phyto, origin, fg_origin, funct_group, Height_cm, LDMC, SLA.cm2.g, RMF, Coarse.root.specific.length.cm.g, Proportion.fine.roots, Coarse.root.diameter.mm)

names(temp) <- c("phyto", "origin", "fg_origin", "fg", "Height", "LDMC", "SLA", "RMF", "CRSL", "PF", "D")

MC.pca <- prcomp(temp[,5:11], scale = T)
summary(MC.pca)

MC.pca.ID <- cbind(temp, MC.pca$x[,1:7])

autoplot(MC.pca, x = 1, y = 2, data = MC.pca.ID, frame = F, loadings = T, loadings.label = T, label = F, col = "origin", size = 1.75, loadings.colour = "black",
         loadings.label.colour="black", loadings.label.repel=TRUE) +
  theme_classic() +
  scale_color_manual(values = c("#5D69B1", "#E58606")) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    legend.title = element_blank())

ggsave("analyses/traits/preliminary_figures/pca_updatedheight_fg.png", width = 7, height = 5)

### POSTER FIG ####
autoplot(MC.pca, x = 1, y = 2, data = MC.pca.ID, frame = F, loadings = T, loadings.label = F, label = F, col = "origin", size = 3, loadings.colour = "black") +
  theme_classic() +
  scale_color_manual(values = c("#5D69B1", "#fab14f")) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    legend.title = element_blank()) +
  theme(text = element_text(size = 20)) +
  theme(legend.position="bottom")

ggsave("analyses/traits/preliminary_figures/ESA_pca_updatedheight_origin.png", width = 7, height = 5.5)


autoplot(MC.pca, x = 1, y = 2, data = MC.pca.ID, frame = F, loadings = T,  col = "fg", size = 3.5, loadings.colour = "black",
) +
  theme_classic() +
  scale_color_manual(values = c( "#ECB159", "#8CB9BD", "#156882")) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    legend.title = element_blank()) +
  theme(text = element_text(size = 20)) +
  theme(legend.position="bottom")

ggsave("analyses/traits/preliminary_figures/pca_newcolors_updatedheight_fg.png", width = 7, height = 5.5)

## Seed Trait PCAs ####
seeds <- c("code_4", "nativity", "growth_form", "FunGroup", "ldd2", "wing.loading", "coat.perm", "E.S", "coat.thick",  "mass.mg", "set.time.mpsec", "shape",  "size", "appendage.type", "prop.C", "prop.N", "cn.seed")

seeds.pca <- c("wing.loading", "coat.perm", "E.S", "coat.thick",  "mass.mg", "set.time.mpsec", "shape",  "size", "cn.seed")

pca <- prcomp(trait.seed2[, seeds.pca], scale = T)
summary(pca)

seeds.pca <- cbind(trait.seed2, pca$x[,1:4])


autoplot(pca, x = 1, y = 2, data = seeds.pca, frame = F, loadings = T, loadings.label = T, label = F,  size = 2, col = "Species", loadings.colour = "black", loadings.label.colour="black") + 
  theme_classic() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    legend.title = element_blank())

# Summarise ####
## calculate mean trait values
trait_sums <- MC.pca.ID %>%
  mutate(species = phyto) %>%
 # select(-PC3, -PC4, -PC5, -PC6, -PC7) %>%
  group_by(fg_origin, fg, species) %>%
  summarise(mean.height = median(Height), se.height = NA, ## no reps, just an average already
            mean.LDMC = mean(LDMC), se.LDMC = calcSE(LDMC),
            mean.SLA = mean(SLA), se.SLA = calcSE(SLA),
            mean.RMF = mean(RMF), se.RMF = calcSE(RMF),
            mean.CRSL = mean(CRSL), se.CRSL = calcSE(CRSL),
            mean.D = mean(D), se.D = calcSE(D),
            mean.PF = mean(PF), se.PF = calcSE(PF),
            mean.PC1 = mean(PC1), se.PC1 = calcSE(PC1),
            mean.PC2 = mean(PC2), se.PC2 = calcSE(PC2))

## seed mass cleaning
seed.sums = seed.mass %>%
  mutate(phyto = paste0(substr(code, start = 1, stop = 2), substr(code, start = 4, stop = 5)),
         phyto = ifelse(phyto == "TRHI", "THIR", phyto),
         phyto = ifelse(phyto == "TRWI", "TWIL", phyto),
         phyto = ifelse(phyto == "FEPE", "LOMU", phyto),
         phyto = ifelse(phyto == "ELCA", "TACA", phyto)) %>%
  mutate(mass.per.cap = mass.mg/10)

# Clean up Env ####
rm(height, trait.seed, trait.seed2, traits, facilitator, grass, lead, lead.traits, legume, MC.sp, MC.sp2, nonnative, adult_trait_field_dat, MC.pca, MC.traits, pca, seed.mass, seeds.pca, temp, all.traits, seeds)