## the purpose of this script is to explore the greenhouse trait data and get PCA values for each species

# Set up ####
library(ggfortify)
library(ggpubr)

# specify dropbox pathway 
if(file.exists("/Users/carme/Dropbox (University of Oregon)/Greenhouse_Traits/")){
  # Carmen
  lead.traits <- "/Users/carme/Dropbox (University of Oregon)/Greenhouse_Traits/Data/Cleaned_Data/"
  
} else {
  # Marina
  #lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Processing/Background-Processing/Background-Processing_entered/"
} 

## load trait data
traits <- read.csv(paste0(lead.traits, "GreenhouseTraits.csv"))

# Clean ####
## subset mega-comp species
## make vector of species
MC.sp <- c("LOPU", "AMME", "ANAR", "BRHOp", "BRNIp", "CESO", "GITRp", "LENIp", "LOMU", "MAELp", "MICA", "PLERp", "PLNO", "TACA", "TRHI", "TRWIp")
  ## "AVEBAR", "ERBO", "CLPUp",

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
                                                                         ifelse(ID == "TRWIp","TWIL", ID))))))))))


colnames(MC.traits) <- c("Taxon", "ID", "Rep", "Date.harvest", "Height.cm", "Fresh.leaf.mass.g", "Dry.leaf.mass.g", "LDMC", "Leaf.Area.cm2", "SLA.cm2.g", "Shoot.dry.biomass.g", "Root.dry.biomass.g", "Total.biomass.g", "RMF", "Root.volume.cm3", "Root.density.g.cm3", "Coarse.root.diameter.mm", "Length.mm", "Fine.root.length.mm", "Coarse.root.length.mm", "Coarse.root.specific.length.cm.g", "Fine.root.specific.length.cm.g", "Proportion.fine.roots", "phyto")


# PCAs ####
all.traits <- c("Height.cm", "LDMC", "SLA.cm2.g", "RMF", "Root.density.g.cm3", "Coarse.root.specific.length.cm.g", "Fine.root.specific.length.cm.g", "Proportion.fine.roots")

MC.pca <- prcomp(MC.traits[,all.traits], scale = T)
summary(MC.pca)

MC.pca.ID <- cbind(MC.traits, MC.pca$x[,1:8])

autoplot(MC.pca, x = 1, y = 2, data = MC.pca.ID, frame = F, loadings = T, loadings.label = T, label = F, col = "phyto", size = 1.5, loadings.colour = "black",
         loadings.label.colour="black", loadings.label.repel=TRUE) +
  theme_classic() +
  #scale_color_manual(values = c("#24796C","#99C945"), labels=c('E. elymoides', 'P. spicata'), name = "") +
  
  #geom_text(aes(label = code, col = Rating)) +
  # stat_ellipse(aes(group = ID, col = ID)) + 
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    legend.title = element_blank())
