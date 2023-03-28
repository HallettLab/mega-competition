# Seed survival
## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

if(file.exists("/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Seed-Survival/")){
  # Carmen
  lead <- "/Users/carme/Dropbox (University of Oregon)/Mega_Competition/Data/Seed-Survival/"
  
} else {
  # Marina
  lead <- "/Users/Marina/Documents/Dropbox/Mega_Competition/Data/Seed-Survival/"
} 
surv <- read.csv(paste0(lead, "Seed-bag-survival_Year1.csv"))

library(tidyverse)

# remove uninoculated trifoliums and species we are no longer using
rm <- c("Stipa pulchra", "Vicia villosa", "Erodium botrys")
surv <- filter(surv, X != "uninoculated", !Species %in% rm)

# look at distribution
ggplot(surv, aes(x = Species, y = n.viable)) +
  geom_boxplot() # few big outliers, looks decent

surv.sum <- surv %>%
  group_by(Species) %>%
  summarize(
    surv.mean.p = mean(n.viable)/100,
    surv.se.p = calcSE(n.viable)/100
  )

  surv.sum$Species <- factor(surv.sum$Species, levels = surv.sum$Species[order(surv.sum$surv.mean.p)])
  
ggplot(surv.sum, aes(x = Species, y = surv.mean.p)) +
  geom_point() +
  geom_errorbar(aes(ymin = surv.mean.p - surv.se.p, ymax = surv.mean.p + surv.se.p)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

##### Traits ####
trait.adult <- read.csv("/users/Marina/Documents/Dropbox/Mega_Competition/Data/Traits/Megacomp_adult-traits.csv")

trait.seed <- read.csv("/users/Marina/Documents/USDA-PostDoc/Projects/Seed-Traits/Data/20230324_Seed-Traits_cleaning.csv")

trait.seed <- trait.seed[,-9] # get rid of height because we have better height data for adults 
colnames(trait.seed)[20] <- "cn.seed"
trait <- merge(trait.seed, trait.adult[,-c(1:7,13:25)], by.x = "code", by.y = "Code", all.x = F, all.y = T)

surv.trait <- merge(surv[,c(3,5)], trait, by = "Species", all.y = T, all.x = F)
surv.sum <- merge(surv.sum, trait, by = "Species", all.y = T, all.x = F)

ggplot(surv.trait, aes(y = n.viable/100, x = log(coat.perm.perc))) +
  geom_point()

ggplot(surv.trait, aes(y = n.viable/100, x = coat.thick/morph.mass.mg)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(filter(surv.trait, nat.inv == "native"), aes(y = n.viable/100, x = coat.thick/chem.mass.mg)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~group)

ggplot(surv.trait, aes(y = n.viable/100, x = coat.perm)) +
  geom_point()

# Together PCA ####
hist(log(trait$coat.perm))
hist(log(trait$Height_cm))
hist(log(trait$coat.thick))
hist(trait$E.S) # need to look at this more
hist(log(trait$SLA_cm2.g))
hist(log(trait$cn.seed))
hist(sqrt(trait$shape.c))
hist(sqrt(trait$shape.m))
hist(log(trait$size))
hist(trait$set.time.mpsec)
hist(log(trait$mass.mg))
hist(trait$LWC)
hist(trait$CN)

trait$coat.perm.log <- log(trait$coat.perm)
trait$Height_cm.log <- log(trait$Height_cm)
trait$coat.thick.log <- log(trait$coat.thick)
trait$E.S.log <- sqrt(trait$E.S)
trait$SLA_cm2.g.log <- log(trait$SLA_cm2.g)
trait$shape.sqrt <- sqrt(trait$shape)
trait$size.log <- log(trait$size)
trait$mass.mg.log <- log(trait$mass.mg)
trait$cn.seed.log <- log(trait$cn.seed)

all.pca <- c("Height_cm.log", "SLA_cm2.g.log", "LWC", "CN", "coat.perm.log", "E.S.log", "coat.thick",  "mass.mg.log", "set.time.mpsec", "shape.sqrt",  "size.log", "cn.seed.log")

pca <- prcomp(trait[, c(6:14,18:21,24:27,31,34:37)], scale = T)
summary(pca)

all.pca <- cbind(trait, pca$x[,1:4])
all.pca$FunGroup <- paste(all.pca$nat.inv, all.pca$group, sep = " ")

library(ggfortify)

autoplot(pca, x = 1, y = 2, data = all.pca, frame = F, loadings = T, loadings.label = T, label = T, col = "FunGroup", size = 1) +
  theme_classic() +
  #geom_text(aes(label = code, col = Rating)) +
  #stat_ellipse(aes(group = group)) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    legend.title = element_blank())


surv.sum <- merge(surv.sum, all.pca, by = "Species", all.y = T, all.x = F)

ggplot(surv.sum, aes(y = surv.mean.p, x = PC1)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(surv.sum, aes(y = surv.mean.p, x = PC2)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(surv.sum[surv.sum$coat.perm < 100,], aes(y = surv.mean.p, x = coat.perm)) +
  geom_point()

ggplot(surv.sum, aes(y = surv.mean.p, x = coat.thick)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(surv.sum, aes(y = surv.mean.p, x = mass.mg)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(surv.sum, aes(y = surv.mean.p, x = shape)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(surv.sum, aes(y = surv.mean.p, x = prop.C)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(surv.sum, aes(y = surv.mean.p, x = prop.N)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(surv.sum, aes(y = surv.mean.p, x = E.S)) +
  #geom_point() +
  geom_text(aes(label = code_4)) +
  geom_smooth(method = "lm")

ggplot(surv.sum, aes(y = surv.mean.p, x = cn.seed)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(surv.sum, aes(y = surv.mean.p, x = Height_cm)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(surv.sum, aes(y = surv.mean.p, x = coat.thick/size)) +
  #geom_point() +
  geom_text(aes(label = code_4)) +
  geom_smooth(method = "lm")
