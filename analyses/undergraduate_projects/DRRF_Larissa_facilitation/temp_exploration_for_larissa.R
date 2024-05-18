library(tidyverse)

source("data_cleaning/phyto-processing_data-cleaning/BRHO_phyto.R")


## explore density & facilitation

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

# Clean ####
brho.tmp <- left_join(brho.phyto, unique.key, by = c("unique.ID", "phyto"))

n.fixer <- c("ACAM", "TWIL", "THIR")
non.n <- c("PLER", "LENI", "GITR")
grass <- c("BRHO", "LOMU", "TACA")

brho_filtered <- brho.tmp %>%
  filter(bkgrd == "Control" | bkgrd == "PLER" | bkgrd == "BRHO" | bkgrd == "TWIL" | bkgrd == "THIR" | bkgrd == "ACAM" | bkgrd == "LENI" | bkgrd == "GITR" | bkgrd == "TACA" | bkgrd == "LOMU") %>%
  mutate(percap.gr = phyto.seed.out/phyto.seed.in,
         FG = ifelse(bkgrd %in% n.fixer, "N-Fixer", 
                     ifelse(bkgrd %in% non.n, "Other Forb", 
                            ifelse(bkgrd %in% grass, "Grass", "Control"))), 
         dens = ifelse(dens == "H", "High", "Low"), 
         dens = ifelse(bkgrd == "Control", "None", dens))



# Mk summary dfs ####
## BG & Trt ####
bg.trt.sum <- brho_filtered %>%
  group_by(bkgrd, treatment) %>%
  summarise(mean.seeds = mean(phyto.seed.out), se.seeds = calcSE(phyto.seed.out),
            mean.percap.gr = mean(percap.gr), se.percap.gr = calcSE(percap.gr))
## FG ####
FG.sum <- brho_filtered %>%
  group_by(FG) %>%
  summarise(mean.seeds = mean(phyto.seed.out), se.seeds = calcSE(phyto.seed.out),
            mean.percap.gr = mean(percap.gr), se.percap.gr = calcSE(percap.gr))
FG.sum$FG <- as.factor(FG.sum$FG)

FG.sum <- FG.sum %>%
  mutate(FG = fct_relevel(FG, c("Grass", "Other Forb", "Control", "N-Fixer")))

## BG Only ####
bg.sum <- brho_filtered %>%
  group_by(bkgrd) %>%
  summarise(mean.seeds = mean(phyto.seed.out), se.seeds = calcSE(phyto.seed.out),
            mean.percap.gr = mean(percap.gr), se.percap.gr = calcSE(percap.gr))

FG.trt.sum <- brho_filtered %>%
  group_by(FG, treatment) %>%
  summarise(mean.seeds = mean(phyto.seed.out), se.seeds = calcSE(phyto.seed.out),
            mean.percap.gr = mean(percap.gr), se.percap.gr = calcSE(percap.gr))
FG.trt.sum$FG <- as.factor(FG.trt.sum$FG)

FG.trt.sum <- FG.trt.sum %>%
  mutate(FG = fct_relevel(FG, c("Grass", "Other Forb", "Control", "N-Fixer")))

ggplot(FG.trt.sum, aes(x=FG, y=mean.percap.gr)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean.percap.gr - se.percap.gr, ymax = mean.percap.gr + se.percap.gr), width = 0.25) +
  
  ylab("Bromus Growth Rate") + xlab("Background Species") +
  theme(text = element_text(size = 15)) +
  geom_point(aes(fill=treatment), 
             colour="black",pch=21, size=4) +
  scale_fill_manual(values = c("#009392", "#e88471"), name = "Precipitation") +
  theme(legend.position = "bottom")

## Density ####
dens.sum <- brho_filtered %>%
  filter(FG == "N-Fixer") %>%
  group_by(bkgrd, dens) %>%
  summarise(mean.seeds = mean(phyto.seed.out), se.seeds = calcSE(phyto.seed.out),
            mean.percap.gr = mean(percap.gr), se.percap.gr = calcSE(percap.gr))

### N-fix dens only ####
dens.bkgrd.sum <- brho_filtered %>%
  filter(FG == "N-Fixer" | FG == "Control") %>%
  mutate(dens = ifelse(FG == "Control", "None", dens)) %>%
  group_by(dens) %>%
  summarise(mean.seeds = mean(phyto.seed.out), se.seeds = calcSE(phyto.seed.out),
            mean.percap.gr = mean(percap.gr), se.percap.gr = calcSE(percap.gr))

dens.bkgrd.sum$dens <- as.factor(dens.bkgrd.sum$dens)

dens.bkgrd.sum <- dens.bkgrd.sum %>%
  mutate(dens = fct_relevel(dens, c("None", "Low", "High")))

### N-fix Dens & Trt ####
dens.bkgrd.trt.sum <- brho_filtered %>%
  filter(FG == "N-Fixer" | FG == "Control") %>%
  mutate(dens = ifelse(FG == "Control", "None", dens)) %>%
  group_by(dens, treatment) %>%
  summarise(mean.seeds = mean(phyto.seed.out), se.seeds = calcSE(phyto.seed.out),
            mean.percap.gr = mean(percap.gr), se.percap.gr = calcSE(percap.gr))

dens.bkgrd.trt.sum$dens <- as.factor(dens.bkgrd.trt.sum$dens)

dens.bkgrd.trt.sum <- dens.bkgrd.trt.sum %>%
  mutate(dens = fct_relevel(dens, c("None", "Low", "High")))


all.dens.fg.sum <- brho_filtered %>%
  group_by(FG, dens) %>%
  summarise(mean.seeds = mean(phyto.seed.out), se.seeds = calcSE(phyto.seed.out),
            mean.percap.gr = mean(percap.gr), se.percap.gr = calcSE(percap.gr))


# Visualize ####
ggplot(bg.trt.sum, aes(x=bkgrd, y=mean.seeds, color = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean.seeds - se.seeds, ymax = mean.seeds + se.seeds), width = 0.25)

ggplot(bg.trt.sum, aes(x=bkgrd, y=mean.percap.gr, color = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean.percap.gr - se.percap.gr, ymax = mean.percap.gr + se.percap.gr), width = 0.25)

ggplot(bg.sum, aes(x=bkgrd, y=mean.seeds)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean.seeds - se.seeds, ymax = mean.seeds + se.seeds), width = 0.25)
  
## FG Summary ####
### seeds ####
ggplot(FG.sum, aes(x=FG, y=mean.seeds)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean.seeds - se.seeds, ymax = mean.seeds + se.seeds), width = 0.25) +
  ylab("Mean Seed Output") + xlab("Background Species") +
  theme(text = element_text(size = 15))

ggsave("fg.mean.seeds.png", width = 4, height = 3)

### pc growth rate ####
ggplot(FG.sum, aes(x=FG, y=mean.percap.gr)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean.percap.gr - se.percap.gr, ymax = mean.percap.gr + se.percap.gr), width = 0.25) +
  ylab("Bromus Growth Rate") + xlab("Background Species") +
  theme(text = element_text(size = 15))
ggsave("percapGR_by_bkgrd.png", width = 4, height = 3)


## Density
ggplot(dens.sum, aes(x=bkgrd, y= mean.percap.gr, color = dens)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean.percap.gr - se.percap.gr, ymax = mean.percap.gr + se.percap.gr), width = 0.25) +
  scale_color_manual(values = c("#074050", "#6cc08b"))

ggplot(dens.sum, aes(x=bkgrd, y= mean.seeds, color = dens)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean.seeds - se.seeds, ymax = mean.seeds + se.seeds), width = 0.25) +
  scale_color_manual(values = c("#074050", "#6cc08b"))
#d3f2a3,#97e196,#6cc08b,#4c9b82,#217a79,#105965,#074050
  

ggplot(dens.bkgrd.sum, aes(x=dens, y= mean.percap.gr)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean.percap.gr - se.percap.gr, ymax = mean.percap.gr + se.percap.gr), width = 0.25) +
  
  ylab("Bromus Growth Rate") + xlab("N-Fixer Density") +
  theme(text = element_text(size = 15)) #+
  #geom_point(aes(fill=dens), 
  #           colour="black",pch=21, size=4) +
  #scale_fill_manual(values = c("#d3f2a3", "#6cc08b", "#217a79"), name = "Density")

ggsave("n-fixer_density.png", width = 4, height = 3)


ggplot(dens.bkgrd.trt.sum, aes(x=dens, y= mean.percap.gr)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean.percap.gr - se.percap.gr, ymax = mean.percap.gr + se.percap.gr), width = 0.25) +
  
  ylab("Bromus Growth Rate") + xlab("N-Fixer Density") +
  theme(text = element_text(size = 15)) +
  geom_point(aes(fill=treatment), 
             colour="black",pch=21, size=4) +
  scale_fill_manual(values = c("#009392", "#e88471"), name = "Precipitation") +
  theme(legend.position = "bottom")

ggsave("n-fixer_density_trt.png", width = 4, height = 3.5)

  #009392,#39b185,#9ccb86,#e9e29c,#eeb479,#e88471,#cf597e#009392,#39b185,#9ccb86,#e9e29c,#eeb479,#e88471,#cf597e




ggplot(all.dens.fg.sum, aes(x=FG, y= mean.percap.gr, color = dens)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean.percap.gr - se.percap.gr, ymax = mean.percap.gr + se.percap.gr), width = 0.25) +
  scale_color_manual(values = c("#00718b", "#b7e6a5")) # +
  #ylab("Bromus Growth Rate") + xlab("N-Fixer Density")
#f7feae,#b7e6a5,#7ccba2,#46aea0,#089099,#00718b,#045275


# Model ####
m1 <- aov(phyto.seed.out ~ bkgrd+treatment, data = brho_filtered)
summary(m1)
## significant effect of bkgrd, no effect of treatment and no interaction
TukeyHSD(m1)

m2 <- aov(percap.gr ~ bkgrd*treatment, data = brho_filtered)
summary(m2)


m2 <- lmer(percap.gr ~ bkgrd*treatment, data = brho_filtered)
## include species as a random effect, block as a random effect?



m3 <- aov(percap.gr ~ FG, data = brho_filtered)
summary(m3)
TukeyHSD(m3)


n_fixer <- brho_filtered %>%
  filter(FG == "N-Fixer" | FG == "Control")

m4 <- aov(percap.gr ~ dens*treatment, data = n_fixer)
summary(m4)



## signif comparisons
## Control-BRHO
## TWIL-BRHO
## TWIL-PLER




