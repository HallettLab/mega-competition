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
  ## calculate mean trait values
  summarise(mean.height = mean(Height), se.height = calcSE(Height),
            mean.LDMC = mean(LDMC), se.LDMC = calcSE(LDMC),
            mean.SLA = mean(SLA), se.SLA = calcSE(SLA),
            mean.RMF = mean(RMF), se.RMF = calcSE(RMF),
            mean.CRSL = mean(CRSL), se.CRSL = calcSE(CRSL),
            mean.D = mean(D), se.D = calcSE(D),
            mean.PF = mean(PF), se.PF = calcSE(PF),
            mean.PC1 = mean(PC1), se.PC1 = calcSE(PC1),
            mean.PC2 = mean(PC2), se.PC2 = calcSE(PC2))
  
psums2 <- psums %>%
  filter(parameter_type != "lambda") %>% ## remove lambda
  mutate(phyto = species,
         resident = toupper(substr(parameter_type, start = 7, stop = 10)), ## create resident species column
         species = resident)

## join traits again to get resident species trait values
psums_t2_alphas <- left_join(psums2, trait_sums, by = c("species"))

## Scale alphas ####
lambda.temp <- psums %>%
  filter(parameter_type == "lambda")%>%
  mutate(lambda = median_parameter, 
         phyto = species) %>%
  select(phyto, treatment, lambda)

alpha_sc <- left_join(psums2, lambda.temp, by = c("phyto", "treatment")) %>%
  mutate(alpha_scaled = median_parameter/lambda)

alpha_sc2 <- left_join(alpha_sc, trait_sums, by = c("species"))

# Visualize ####
## Median Lambdas ####
lambda <- psums %>%
  filter(parameter_type == "lambda")

ggplot(lambda, aes(x=species, y=median_parameter, color = treatment)) +
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = hdi_lo, ymax = hdi_hi)) +
  ylab("Lambda") +
  xlab(NULL) + 
  scale_color_manual(values = c("#70a494", "#de8a5a")) +
  labs(color = "Rainfall") +
  theme(axis.text.x = element_text(angle = 45,  hjust=1))

ggsave("analyses/explore_interaction_coeff/preliminary_figures/median_lambda_by_rainfall.png", width = 6, height = 4)

## Median Alpha by FG ####
### raw alphas ####
intra <- psums_t2_alphas %>%
  filter(phyto == resident)

inter <- psums_t2_alphas %>%
  filter(phyto != resident)

### intra specific interactions
ggplot(intra, aes(x=parameter_type, y=median_parameter, color = fg_origin, shape = treatment)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 3) +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_errorbar(aes(ymin = hdi_lo, ymax = hdi_hi), width = 0.25) +
  xlab(NULL) + ylab("Intraspecific Median Alpha Values") +
  theme(axis.text.x = element_text(angle = 45,  hjust=1)) +
  labs(color = "Functional Group") +
  scale_shape_manual(values = c(16, 1))

ggsave("analyses/explore_interaction_coeff/preliminary_figures/intra_alpha_byFG.png", width = 6.5, height = 4)

### inter specific interactions
ggplot(inter, aes(x=parameter_type, y=median_parameter, color = fg_origin)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot() +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) + xlab(NULL) + ylab("Interspecific Median Alpha Values") +
  theme(axis.text.x = element_text(angle = 45,  hjust=1)) +
  labs(color = "Functional Group")
  
ggsave("analyses/explore_interaction_coeff/preliminary_figures/inter_alpha_byFG.png", width = 6.5, height = 4)

### scaled alphas ####
intra_sc <- alpha_sc2 %>%
  filter(phyto == resident)

inter_sc <- alpha_sc2 %>%
  filter(phyto != resident)

#### not transformed
ggplot(inter_sc, aes(x=parameter_type, y=alpha_scaled, color = fg_origin)) +
  #  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot() +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) + xlab(NULL) + ylab("Interspecific Scaled Median Alphas") +
  theme(axis.text.x = element_text(angle = 45,  hjust=1)) +
  labs(color = "Functional Group")

ggsave("analyses/explore_interaction_coeff/preliminary_figures/inter_alpha_scaled_byFG.png", width = 6.5, height = 4)

ggplot(intra_sc, aes(x=parameter_type, y=alpha_scaled, color = fg_origin, shape = treatment)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 3) +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
 # geom_errorbar(aes(ymin = hdi_lo, ymax = hdi_hi), width = 0.25) +
  xlab(NULL) + ylab("Intraspecific Median Alpha Values") +
  theme(axis.text.x = element_text(angle = 45,  hjust=1)) +
  labs(color = "Functional Group") +
  scale_shape_manual(values = c(16, 1))

ggsave("analyses/explore_interaction_coeff/preliminary_figures/intra_alpha_scaled_byFG.png", width = 6.5, height = 4)

## Traits v Alphas ####
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

d <- ggplot(psums_t2_alphas, aes(x=mean.D, y=median_parameter)) +
  ylab("Alpha") + xlab("Resident D") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_point(aes(color = fg_origin), size = 1.5) +
  geom_smooth(method = "lm", color = "black", alpha = 0.5) +
  labs(color = "Resident FG") +
  geom_hline(yintercept = 0, linetype = "dashed")

pf <- ggplot(psums_t2_alphas, aes(x=mean.PF, y=median_parameter)) +
  ylab("Alpha") + xlab("Resident PF") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_point(aes(color = fg_origin), size = 1.5) +
  geom_smooth(method = "lm", color = "black", alpha = 0.5) +
  labs(color = "Resident FG") +
  geom_hline(yintercept = 0, linetype = "dashed")

## arrange all separate traits into one figure
ggarrange(height, sla, ldmc,
          #rmf, crsl, d, pf,
          ncol = 3, nrow = 1,
          common.legend = T,
          labels= "AUTO",
          legend = "bottom")

## save output
ggsave("analyses/explore_interaction_coeff/preliminary_figures/alpha_resident_AGtraits.png", width = 8, height = 3.5)

ggarrange(#height, sla, ldmc,
          rmf, crsl, d, pf,
          ncol = 4, nrow = 1,
          common.legend = T,
          labels= "AUTO",
          legend = "bottom")

ggsave("analyses/explore_interaction_coeff/preliminary_figures/alpha_resident_BGtraits.png", width = 10, height = 3.25)

## PCA scores v Alphas ####
pc1 <- ggplot(psums_t2_alphas, aes(x=mean.PC1, y=median_parameter)) +
  ylab("Alpha") + xlab("PC1") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_point(aes(color = fg_origin), size = 1.5) +
  geom_smooth(method = "lm", color = "black", alpha = 0.5) +
  labs(color = "Resident FG") +
  geom_hline(yintercept = 0, linetype = "dashed")

pc2 <- ggplot(psums_t2_alphas, aes(x=mean.PC2, y=median_parameter)) +
  ylab("Alpha") + xlab("PC2") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_point(aes(color = fg_origin), size = 1.5) +
  geom_smooth(method = "lm", color = "black", alpha = 0.5) +
  labs(color = "Resident FG") +
  geom_hline(yintercept = 0, linetype = "dashed")

ggarrange(pc1, pc2,
  ncol = 2, nrow = 1,
  common.legend = T,
  labels= "AUTO",
  legend = "right")

ggsave("analyses/explore_interaction_coeff/preliminary_figures/alpha_resident_PCscores.png", width = 7, height = 3)

ggplot(psums_t2_alphas, aes(x=mean.PC2, y=median_parameter)) +
  ylab("Alpha") + xlab("PC2") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_point(aes(color = fg_origin), size = 1.5) +
  geom_smooth(method = "lm", color = "black", alpha = 0.5) +
  labs(color = "Resident FG") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~treatment)

ggplot(psums_t2_alphas, aes(x=mean.PC1, y=median_parameter)) +
  ylab("Alpha") + xlab("PC1") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_point(aes(color = fg_origin), size = 1.5) +
  geom_smooth(method = "lm", color = "black", alpha = 0.5) +
  labs(color = "Resident FG") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~treatment)








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




