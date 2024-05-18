## explore trait relationships with parameter values

# Set up Env ####
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

library(tidyverse)
library(ggpattern)

theme_set(theme_classic())

psums <- read.csv("data/parameter_summaries_20231218_models.csv")
source("analyses/traits/trait_pcas_exploration.R")

# Clean data ####

## calculate mean trait values
trait_sums <- MC.pca.ID %>%
  mutate(species = phyto) %>%
  group_by(fg_origin, fg, species) %>%
  summarise(mean.height = mean(Height), se.height = calcSE(Height),
            mean.LDMC = mean(LDMC), se.LDMC = calcSE(LDMC),
            mean.SLA = mean(SLA), se.SLA = calcSE(SLA),
            mean.RMF = mean(RMF), se.RMF = calcSE(RMF),
            mean.CRSL = mean(CRSL), se.CRSL = calcSE(CRSL),
            mean.D = mean(D), se.D = calcSE(D),
            mean.PF = mean(PF), se.PF = calcSE(PF),
            mean.PC1 = mean(PC1), se.PC1 = calcSE(PC1),
            mean.PC2 = mean(PC2), se.PC2 = calcSE(PC2))

## separate out alphas from lambdas in summary df
psums2 <- psums %>%
  filter(parameter_type != "lambda") %>% ## remove lambda
  mutate(phyto = species,
         resident = toupper(substr(parameter_type, start = 7, stop = 10)), ## create resident species column
         species = resident)

## create temporary lambda df to join later for scaling
lambda.temp <- psums %>%
  filter(parameter_type == "lambda")%>%
  mutate(lambda = median_parameter, 
         phyto = species) %>%
  select(phyto, treatment, lambda)

## scale alphas by lambdas
alpha_sc <- left_join(psums2, lambda.temp, by = c("phyto", "treatment")) %>%
  mutate(alpha_scaled = median_parameter/lambda,
         hdi_hi_scaled = hdi_hi/lambda,
         hdi_lo_scaled = hdi_lo/lambda)

## join with mean traits
alpha_sc2 <- left_join(alpha_sc, trait_sums, by = c("species"))

intra_sc <- alpha_sc2 %>%
  filter(phyto == resident) #%>%
  #mutate(resident = fct_relevel(resident, "ACAM", "THIR", "TWIL", "AMME", "MAEL", "PLNO", "ANAR", "MICA", "GITR", "LENI", "PLER", "BRNI", "CESO", "BRHO", "LOMU", "TACA"))

inter_sc <- alpha_sc2 %>%
  filter(phyto != resident) %>%
  mutate(parameter_type = fct_relevel(parameter_type, "alpha_brho", "alpha_lomu", "alpha_taca", "alpha_thir", "alpha_acam",  "alpha_twil", "alpha_amme", "alpha_mael", "alpha_plno", "alpha_mica", "alpha_gitr", "alpha_leni", "alpha_pler", "alpha_anar", "alpha_brni", "alpha_ceso"),
         resident = fct_relevel(resident, "BRHO", "LOMU", "TACA", "THIR", "ACAM",  "TWIL", "AMME", "MAEL", "PLNO", "MICA", "GITR", "LENI", "PLER", "ANAR", "BRNI", "CESO"),
         Origin = ifelse(resident %in% c("BRHO", "LOMU", "TACA", "THIR","ANAR", "BRNI", "CESO"), "Non-native", "Native"),
         fg = ifelse(fg == "grass", "Grass",
                     ifelse(fg == "forb", "Forb", "Legume")))

# Visualize ####
## Fig S2:Med Lambdas  ####
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
ggplot(inter_sc, aes(x=parameter_type, y=alpha_scaled, color = fg_origin)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot() +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) + xlab(NULL) + ylab("Interspecific Scaled Median Alphas") +
  theme(axis.text.x = element_text(angle = 45,  hjust=1)) +
  labs(color = "Functional Group")

ggsave("analyses/explore_interaction_coeff/preliminary_figures/storyboard_v2/inter_alpha_scaled_byFG.png", width = 6.5, height = 4)


ggplot(inter_sc, aes(x=resident, y=median_parameter, fill = fg)) +
  geom_boxplot_pattern(aes(pattern = Origin, fill = fg)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("#ECB159", "#8CB9BD", "#156882")) +
  scale_pattern_manual(values = c("Native" = "none", "Non-native" = "stripe")) +
  xlab(NULL) + ylab("Interaction Coefficient") +
  theme(axis.text.x = element_text(angle = 45,  hjust=1)) +
  labs(fill = NULL, pattern = NULL) +
  annotate(geom="text", x =-0.15, y=0.25, label = "competition", size = 4, angle='90') +
  annotate(geom="text", x =-0.15, y=-0.25, label = "facilitation", size = 4, angle='90') +
  coord_cartesian(xlim = c(-0.3,16)) +
  theme(plot.background = element_rect(fill = "#FEFBF6"),
        panel.background = element_rect(fill = "#FEFBF6",
                                        colour = "#FEFBF6"),
        legend.key = element_rect(fill = "#FEFBF6"),
        legend.background = element_rect(fill = "#FEFBF6"),
        legend.position = "right") +
  theme(text = element_text(size = 13)) +
  theme(legend.position="bottom")

  
ggsave("analyses/explore_interaction_coeff/preliminary_figures/storyboard_v2/inter_alpha_byFG.png", width = 7, height = 4)


  

ggplot(intra_sc, aes(x=parameter_type, y=alpha_scaled, color = fg_origin, shape = treatment)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 3) +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
 # geom_errorbar(aes(ymin = hdi_lo, ymax = hdi_hi), width = 0.25) +
  xlab(NULL) + ylab("Intraspecific Median Alpha Values") +
  theme(axis.text.x = element_text(angle = 45,  hjust=1)) +
  labs(color = "Functional Group") +
  scale_shape_manual(values = c(16, 1))

#ggsave("analyses/explore_interaction_coeff/preliminary_figures/intra_alpha_scaled_byFG.png", width = 6.5, height = 4)

## Traits v Alphas ####
ldmc <- ggplot(inter_sc, aes(x=mean.LDMC, y=alpha_scaled)) +
  ylab("Intersp Scaled Alpha") + xlab("Resident LDMC") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_point(aes(color = fg_origin), size = 1.5) +
  geom_smooth(method = "lm", color = "black", alpha = 0.5) +
  labs(color = "Resident FG") +
  geom_hline(yintercept = 0, linetype = "dashed")

height <- ggplot(inter_sc, aes(x=mean.height, y=alpha_scaled)) +
  ylab("Intersp Scaled Alpha") + xlab("Resident Height") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_point(aes(color = fg_origin), size = 1.5) +
  geom_smooth(method = "lm", color = "black", alpha = 0.5) +
  labs(color = "Resident FG") +
  geom_hline(yintercept = 0, linetype = "dashed")

sla <- ggplot(inter_sc, aes(x=mean.SLA, y=alpha_scaled)) +
  ylab("Intersp Scaled Alpha") + xlab("Resident SLA") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_point(aes(color = fg_origin), size = 1.5) +
  geom_smooth(method = "lm", color = "black", alpha = 0.5) +
  labs(color = "Resident FG") +
  geom_hline(yintercept = 0, linetype = "dashed")

crsl <- ggplot(inter_sc, aes(x=mean.CRSL, y=alpha_scaled)) +
  ylab("Intersp Scaled Alpha") + xlab("Resident CRSL") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_point(aes(color = fg_origin), size = 1.5) +
  geom_smooth(method = "lm", color = "black", alpha = 0.5) +
  labs(color = "Resident FG") +
  geom_hline(yintercept = 0, linetype = "dashed")

rmf <- ggplot(inter_sc, aes(x=mean.RMF, y=alpha_scaled)) +
  ylab("Intersp Scaled Alpha") + xlab("Resident RMF") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_point(aes(color = fg_origin), size = 1.5) +
  geom_smooth(method = "lm", color = "black", alpha = 0.5) +
  labs(color = "Resident FG") +
  geom_hline(yintercept = 0, linetype = "dashed")

d <- ggplot(inter_sc, aes(x=mean.D, y=alpha_scaled)) +
  ylab("Intersp Scaled Alpha") + xlab("Resident D") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_point(aes(color = fg_origin), size = 1.5) +
  geom_smooth(method = "lm", color = "black", alpha = 0.5) +
  labs(color = "Resident FG") +
  geom_hline(yintercept = 0, linetype = "dashed")

pf <- ggplot(inter_sc, aes(x=mean.PF, y=alpha_scaled)) +
  ylab("Intersp Scaled Alpha") + xlab("Resident PF") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_point(aes(color = fg_origin), size = 1.5) +
  geom_smooth(method = "lm", color = "black", alpha = 0.5) +
  labs(color = "Resident FG") +
  geom_hline(yintercept = 0, linetype = "dashed")

## arrange all separate traits into one figure
ggarrange(height, sla, ldmc,
          rmf, crsl, d, pf,
          ncol = 4, nrow = 2,
          common.legend = T,
          labels= "AUTO",
          legend = "bottom")

## save output
ggsave("analyses/explore_interaction_coeff/preliminary_figures/storyboard_v2/sc_alpha_resident_traits.png", width = 9, height = 6)


## PCA scores v Alphas ####
pc1 <- ggplot(inter_sc, aes(x=mean.PC1, y=alpha_scaled)) +
  ylab("Intersp Scaled Alpha") + xlab("PC1") +
  scale_color_manual(values = c("#5D69B1","#CC61B0", "#E58606", "#99C945","#CC3A8E")) +
  geom_point(aes(color = fg_origin), size = 1.5) +
  geom_smooth(method = "lm", color = "black", alpha = 0.5) +
  labs(color = "Resident FG") +
  geom_hline(yintercept = 0, linetype = "dashed")

pc2 <- ggplot(inter_sc, aes(x=mean.PC2, y=alpha_scaled)) +
  ylab("Intersp Scaled Alpha") + xlab("PC2") +
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

ggsave("analyses/explore_interaction_coeff/preliminary_figures/storyboard_v2/alpha_resident_PCscores.png", width = 7, height = 3)

# Clean up ####
rm()