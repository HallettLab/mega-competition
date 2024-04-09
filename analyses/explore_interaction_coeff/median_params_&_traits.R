## explore trait relationships with parameter values

# Set up Env ####
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

library(tidyverse)

theme_set(theme_classic())

psums <- read.csv("data/parameter_summaries_20231218_models.csv")
source("analyses/traits/trait_pcas_exploration.R")

# Clean data ####
trait_sums <- MC.pca.ID %>%
  mutate(species = phyto) %>%
  group_by(fg_origin, species) %>%
  summarise(mean.height = mean(Height.cm), se.height = calcSE(Height.cm),
            mean.LDMC = mean(LDMC), se.LDMC = calcSE(LDMC),
            mean.SLA = mean(SLA.cm2.g), se.SLA = calcSE(SLA.cm2.g),
            mean.RMF = mean(RMF), se.RMF = calcSE(RMF),
            mean.CRSL = mean(Coarse.root.specific.length.cm.g), se.CRSL = calcSE(Coarse.root.specific.length.cm.g),
            mean.FRSL = mean(Fine.root.specific.length.cm.g), se.FRSL = calcSE(Fine.root.specific.length.cm.g))

psums_t <- left_join(psums, trait_sums, by = c("species")) %>%
  mutate(phyto.fg.origin = fg_origin,
         phyto.mean.height = mean.height,
         phyto.se.height = se.height,
         phyto.mean.LDMC = mean.LDMC,
         phyto.se.LDMC = se.LDMC,
         phyto.mean.SLA = mean.SLA,
         phyto.se.SLA = se.SLA,
         phyto.mean.RMF = mean.RMF,
         phyto.se.RMF = se.RMF,
         phyto.mean.CRSL = mean.CRSL,
         phyto.se.CRSL = se.CRSL,
         phyto.mean.FRSL = mean.FRSL,
         phyto.se.FRSL = se.FRSL) %>%
  select(species, treatment, parameter_type, median_parameter, hdi_lo, hdi_hi, phyto.fg.origin, phyto.mean.height, phyto.se.height, phyto.mean.LDMC, phyto.se.LDMC, phyto.mean.SLA, phyto.se.SLA, phyto.mean.RMF, phyto.se.RMF, phyto.mean.CRSL, phyto.se.CRSL, phyto.mean.FRSL, phyto.se.FRSL)
  
psums_t_alphas <- psums_t %>%
  filter(parameter_type != "lambda") %>%
  mutate(phyto = species,
         resident = toupper(substr(parameter_type, start = 7, stop = 10)),
         species = resident)

psums_t2_alphas <- left_join(psums_t_alphas, trait_sums, by = c("species"))

# Visualize ####
## Lambda ####
ggplot(psums_t[psums_t$parameter_type == "lambda",], aes(x=mean.SLA, y=median_parameter, color = fg_origin)) +
  geom_point() +
  geom_errorbarh(aes(xmin = mean.SLA - se.SLA, xmax = mean.SLA + se.SLA)) +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  ylab("Lambda") + xlab("SLA") +
  geom_smooth(method = "lm", color = "black", alpha = 0.15) +
  geom_errorbar(aes(ymin = hdi_lo, ymax = hdi_hi))

ggsave("analyses/explore_interaction_coeff/preliminary_figures/lambda_SLA.png", width = 6, height = 3)

ggplot(psums_t[psums_t$parameter_type == "lambda",], aes(x=mean.LDMC, y=median_parameter, color = fg_origin)) +
  
  geom_errorbarh(aes(xmin = mean.LDMC - se.LDMC, xmax = mean.LDMC + se.LDMC)) +
  ylab("Lambda") + xlab("LDMC") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_point(aes(), size = 2) +
  geom_smooth(method = "lm", color = "black", alpha = 0.15) +
  geom_errorbar(aes(ymin = hdi_lo, ymax = hdi_hi))

ggsave("analyses/explore_interaction_coeff/preliminary_figures/lambda_LDMC.png", width = 6, height = 3)




#E58606,#5D69B1,#52BCA3,#99C945,#CC61B0,#24796C,#DAA51B,#2F8AC4,#764E9F,#ED645A,#CC3A8E,#A5AA99

ggplot(psums_t[psums_t$parameter_type == "lambda",], aes(x=mean.RMF, y=median_parameter, color = fg_origin)) +
  geom_point() +
  geom_errorbarh(aes(xmin = mean.RMF - se.RMF, xmax = mean.RMF + se.RMF)) +
  ylab("Lambda") + xlab("RMF") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_smooth(method = "lm", color = "black", alpha = 0.15) +
  geom_errorbar(aes(ymin = hdi_lo, ymax = hdi_hi))
ggsave("analyses/explore_interaction_coeff/preliminary_figures/lambda_RMF.png", width = 6, height = 3)

ggplot(psums_t[psums_t$parameter_type == "lambda",], aes(x=mean.CRSL, y=median_parameter, color = fg_origin)) +
  geom_point() +
  geom_errorbarh(aes(xmin = mean.CRSL - se.CRSL, xmax = mean.CRSL + se.CRSL)) +
  ylab("Lambda") + xlab("CRSL") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_smooth(method = "lm", color = "black", alpha = 0.15) +
  geom_errorbar(aes(ymin = hdi_lo, ymax = hdi_hi))

ggsave("analyses/explore_interaction_coeff/preliminary_figures/lambda_CRSL.png", width = 6, height = 3)

ggplot(psums_t[psums_t$parameter_type == "lambda",], aes(x=mean.FRSL, y=median_parameter, color = fg_origin)) +
  geom_point() +
  geom_errorbarh(aes(xmin = mean.FRSL - se.FRSL, xmax = mean.FRSL + se.FRSL)) +
  ylab("Lambda") + xlab("FRSL") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_smooth(method = "lm", color = "black", alpha = 0.15)

ggsave("analyses/explore_interaction_coeff/preliminary_figures/lambda_FRSL.png", width = 6, height = 3)


## Interaction Output ####
ldmc <- ggplot(psums_t2_alphas, aes(x=mean.LDMC, y=median_parameter)) +
  ylab("Alpha") + xlab("Resident LDMC") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_point(aes(color = fg_origin), size = 1.5) +
  geom_smooth(method = "lm", color = "black", alpha = 0.5) +
  labs(color = "Resident FG") +
  geom_hline(yintercept = 0, linetype = "dashed")

height <- ggplot(psums_t2_alphas, aes(x=mean.height, y=median_parameter)) +
  ylab("Alpha") + xlab("Resident Height") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_point(aes(color = fg_origin), size = 1.5) +
  geom_smooth(method = "lm", color = "black", alpha = 0.5) +
  labs(color = "Resident FG") +
  geom_hline(yintercept = 0, linetype = "dashed")

sla <- ggplot(psums_t2_alphas, aes(x=mean.SLA, y=median_parameter)) +
  ylab("Alpha") + xlab("Resident SLA") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_point(aes(color = fg_origin), size = 1.5) +
  geom_smooth(method = "lm", color = "black", alpha = 0.5) +
  labs(color = "Resident FG") +
  geom_hline(yintercept = 0, linetype = "dashed")

crsl <- ggplot(psums_t2_alphas, aes(x=mean.CRSL, y=median_parameter)) +
  ylab("Alpha") + xlab("Resident CRSL") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_point(aes(color = fg_origin), size = 1.5) +
  geom_smooth(method = "lm", color = "black", alpha = 0.5) +
  labs(color = "Resident FG") +
  geom_hline(yintercept = 0, linetype = "dashed")

rmf <- ggplot(psums_t2_alphas, aes(x=mean.RMF, y=median_parameter)) +
  ylab("Alpha") + xlab("Resident RMF") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_point(aes(color = fg_origin), size = 1.5) +
  geom_smooth(method = "lm", color = "black", alpha = 0.5) +
  labs(color = "Resident FG") +
  geom_hline(yintercept = 0, linetype = "dashed")

frsl <- ggplot(psums_t2_alphas, aes(x=mean.CRSL, y=median_parameter)) +
  ylab("Alpha") + xlab("Resident FRSL") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_point(aes(color = fg_origin), size = 1.5) +
  geom_smooth(method = "lm", color = "black", alpha = 0.5) +
  labs(color = "Resident FG") +
  geom_hline(yintercept = 0, linetype = "dashed")


ggarrange(height, sla, ldmc,
          rmf, crsl, frsl,
          ncol = 3, nrow = 2,
          common.legend = T,
          labels= "AUTO",
          legend = "bottom")

ggsave("analyses/explore_interaction_coeff/preliminary_figures/alpha_resident_traits.png", width = 8, height = 6)



## Interaction Input ####
ggplot(psums_t_alphas, aes(x=phyto.mean.LDMC, y=median_parameter)) +
 # geom_errorbarh(aes(xmin = mean.LDMC - se.LDMC, xmax = mean.LDMC + se.LDMC)) +
  ylab("Alpha") + xlab("Phytometer LDMC") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_point(aes(color = phyto.fg.origin), size = 2) +
  geom_smooth(method = "lm", color = "black", alpha = 0.15) +
  labs(color = "Phytometer FG") +
  ggtitle("Interaction Input")

ggsave("analyses/explore_interaction_coeff/preliminary_figures/alpha_input_LDMC.png", width = 6, height = 3)

ggplot(psums_t_alphas, aes(x=mean.RMF, y=median_parameter, color = fg_origin)) +
  geom_point() +
  #geom_errorbarh(aes(xmin = mean.RMF - se.RMF, xmax = mean.RMF + se.RMF)) +
  ylab("Alpha") + xlab("RMF") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_smooth(method = "lm", color = "black", alpha = 0.15) +
  labs(color = "Resident FG")


ggplot(psums_t[psums_t$parameter_type == "lambda",], aes(x=mean.CRSL, y=median_parameter, color = fg_origin)) +
  geom_point() +
  geom_errorbarh(aes(xmin = mean.CRSL - se.CRSL, xmax = mean.CRSL + se.CRSL)) +
  ylab("Lambda") + xlab("CRSL") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_smooth(method = "lm", color = "black", alpha = 0.15)

ggplot(psums_t[psums_t$parameter_type != "lambda",], aes(x=mean.FRSL, y=median_parameter, color = fg_origin)) +
  geom_point() +
  #geom_errorbarh(aes(xmin = mean.FRSL - se.FRSL, xmax = mean.FRSL + se.FRSL)) +
  ylab("Lambda") + xlab("FRSL") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_smooth(method = "lm", color = "black", alpha = 0.15)


## try one species at a time

ggplot(psums_t[psums_t$parameter_type != "lambda" & psums_t$species == "ACAM",], aes(x=mean.FRSL, y=median_parameter, color = fg_origin)) +
  geom_point() +
  #geom_errorbarh(aes(xmin = mean.FRSL - se.FRSL, xmax = mean.FRSL + se.FRSL)) +
  ylab("Alpha") + xlab("FRSL") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_smooth(method = "lm", color = "black", alpha = 0.15)





