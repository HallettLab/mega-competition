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

## get seed survival data
source("data_cleaning/seed-survival_data-cleaning/seed-survival_data-cleaning.R")

## get germination data
source("data_cleaning/germination_data-cleaning/germination_rates.R")


sort(unique(traits$ID))
colnames(MC.traits)

str(MC.traits)
MC.sp <- c("LOPU", "AMME", "ANAR", "AVEBAR", "BRHOp", "BRNIp", "CESO", "CLPUp", "ERBO", "GITRp", "LENIp", "LOMU", "MAELp", "MICA", "PLERp", "PLNO", "TACA", "TRHI", "TRWIp")

MC.traits <- traits %>%
  filter(ID %in% MC.sp,
         !is.na(RMF),
         !is.na(LDMC))

all.traits <- c("Height..cm.", "LDMC", "SLA..cm2.g.", "RMF", "Root.density..g.cm3.", "Coarse.root.specific.length..cm.g.", "Fine.root.specific.length..cm.g.", "Proportion.fine.roots")

MC.pca <- prcomp(MC.traits[,all.traits], scale = T)
summary(MC.pca)

MC.pca.ID <- cbind(MC.traits, MC.pca$x[,1:8])

autoplot(MC.pca, x = 1, y = 2, data = MC.pca.ID, frame = F, loadings = T, loadings.label = T, label = F, col = "ID", size = 1.5, loadings.colour = "black",
         loadings.label.colour="black", loadings.label.repel=TRUE) +
  theme_classic() +
  #scale_color_manual(values = c("#24796C","#99C945"), labels=c('E. elymoides', 'P. spicata'), name = "") +
  
  #geom_text(aes(label = code, col = Rating)) +
  stat_ellipse(aes(group = ID, col = ID)) + 
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    legend.title = element_blank())


GH.sp <- c("LOPU", "ANAR", "BRHOp", "ERBO", "GITRp", "LENIp", "LOMU", "MICA", "PLERp", "PLNO", "TACA", "TRHI", "TRWIp")

GH.traits <- traits %>%
  filter(ID %in% GH.sp,
         !is.na(RMF),
         !is.na(LDMC))

GH.pca <- prcomp(GH.traits[,all.traits], scale = T)
summary(GH.pca)

GH.pca.ID <- cbind(GH.traits, GH.pca$x[,1:8])

autoplot(GH.pca, x = 1, y = 2, data = GH.pca.ID, frame = F, loadings = T, loadings.label = T, label = F, col = "ID", size = 3, loadings.colour = "black",
         loadings.label.colour="black", loadings.label.repel=TRUE) +
  theme_classic() +
  scale_color_manual(values = c("#E58606","#5D69B1","#52BCA3","#99C945","#CC61B0","#24796C","#DAA51B","#2F8AC4","#764E9F","#ED645A","#CC3A8E","#A5AA99", "#F6CF71")) +
  
  #geom_text(aes(label = code, col = Rating)) +
  stat_ellipse(aes(group = ID, col = ID, size = 0.05)) + 
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    legend.title = element_blank())
#66C5CC,#F6CF71,#F89C74,#DCB0F2,#87C55F,#9EB9F3,#FE88B1,#C9DB74,#8BE0A4,#B497E7,#D3B484,#B3B3B3
#E58606,#5D69B1,#52BCA3,#99C945,#CC61B0,#24796C,#DAA51B,#2F8AC4,#764E9F,#ED645A,#CC3A8E,#A5AA99




GH.forbs <- c("LOPU", "ANAR", "ERBO", "GITRp", "LENIp", "MICA", "PLERp", "TRHI", "TRWIp")

GH.forbs.traits <- traits %>%
  filter(ID %in% GH.forbs,
         !is.na(RMF),
         !is.na(LDMC))

GH.forbs.pca <- prcomp(GH.forbs.traits[,all.traits], scale = T)
summary(GH.forbs.pca)

GH.forbs.pca.ID <- cbind(GH.forbs.traits, GH.forbs.pca$x[,1:8])

autoplot(GH.forbs.pca, x = 1, y = 2, data = GH.forbs.pca.ID, frame = F, loadings = T, loadings.label = T, label = F, col = "ID", size = 3, loadings.colour = "black",
         loadings.label.colour="black", loadings.label.repel=TRUE) +
  theme_classic() +
  scale_color_manual(values = c("#E58606","#5D69B1","#52BCA3","#99C945","#CC61B0","#24796C","#DAA51B","#2F8AC4","#764E9F","#ED645A","#CC3A8E","#A5AA99", "#F6CF71")) +
  
  #geom_text(aes(label = code, col = Rating)) +
  stat_ellipse(aes(group = ID, col = ID), linewidth = 2) + 
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    legend.title = element_blank())
