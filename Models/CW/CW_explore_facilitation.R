## set up env
library(ggpubr)
library(ggfortify)
library(stats)

theme_set(theme_bw())

# read in data ####
## model outputs
source("Models/CW/CW_import_posteriors.R")

## traits
source("data_cleaning/trait_data-cleaning/adult_traits/adult_traits_cleaning.R")



## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}
# Explore All Interactions ####

## Create summary data frame of posterior values
posteriors_long_sum <- posteriors_long %>%
  group_by(treatment, species, alpha_name) %>%
  summarise(mean_alpha = mean(alpha_value), se_alpha = calcSE(alpha_value))

## mean interaction coeff for each focal species
ggplot(posteriors_long_sum, aes(x=alpha_name, y=mean_alpha, color = treatment)) +
  geom_point() +
  facet_wrap(~species, ncol = 3) +
  geom_errorbar(aes(ymin = mean_alpha-se_alpha, ymax = mean_alpha + se_alpha)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = c("#003366", "#FFA630"))

#ggsave("models/CW/preliminary_figures/model_posteriors_updated_lambdas/mean_interaction_coeff_all_sp.png", width = 8, height = 7)
  


# Dial into Facilitation ####
facilitation <- posteriors_long_sum %>%
  filter(mean_alpha < -0.05)

ggplot(facilitation[facilitation$alpha_name != "alpha_avba",], aes(x=alpha_name, y=mean_alpha, color = species)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_errorbar(aes(ymin = mean_alpha-se_alpha, ymax = mean_alpha + se_alpha), width = 0.25) +
  facet_wrap(~treatment) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 90))



facilitation_traits <- merge(facilitation, trait, by.x = "species", by.y = "code_4") %>%
  mutate(invader.FG = paste(nativity, growth_form, sep = "_"))

all_alphas_traits <- merge(posteriors_long_sum, trait, by.x = "species", by.y = "code_4") %>%
  mutate(invader.FG = paste(nativity, growth_form, sep = "_")) %>%
  mutate(resident = toupper(strsplit(alpha_name, "_") %>%
           sapply(tail, 1)))

all_alphas_resident_traits <- merge(all_alphas_traits, trait, by.x="resident", by.y = "code_4")

## species traits by their interaction coefficients?  
  
# Facilitation ####
ggplot(facilitation_traits, aes(x=CN, y=mean_alpha)) +
  geom_point()
ggplot(facilitation_traits, aes(x=Height_cm, y=mean_alpha)) +
  geom_point()


native_forbs <- ggplot(facilitation_traits[facilitation_traits$alpha_name != "alpha_avba" & facilitation_traits$invader.FG == "native_forb",], aes(y=alpha_name, x=mean_alpha, color = treatment)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_errorbarh(aes(xmin = mean_alpha-se_alpha, xmax = mean_alpha + se_alpha), height = 1, color = "black") +
  geom_point(size = 2.75) +
  facet_wrap(~invader.FG) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#003366", "#FFA630")) +
  
  facet_wrap(~treatment) +
  ylab("Native Forbs") +
  coord_cartesian(xlim = c(0.05, -5.6)) +
  scale_x_reverse() +
  xlab("") #+
  #ggtitle("Native Forb Facilitation")


exotic_forbs <- ggplot(facilitation_traits[facilitation_traits$alpha_name != "alpha_avba" & facilitation_traits$invader.FG == "exotic_forb",], aes(y=alpha_name, x=mean_alpha, color = treatment)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_errorbarh(aes(xmin = mean_alpha-se_alpha, xmax = mean_alpha + se_alpha), height = 0.75, color = "black") +
  geom_point(size = 2.75) +
  facet_wrap(~invader.FG) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#003366", "#FFA630")) +
  
  facet_wrap(~treatment) +
  ylab("Exotic Forbs") +
  coord_cartesian(xlim = c(0.05, -5.6)) +
  scale_x_reverse() +
  xlab("") #+
  #ggtitle("Exotic Forb Facilitation")


exotic_grasses <- ggplot(facilitation_traits[facilitation_traits$alpha_name != "alpha_avba" & facilitation_traits$invader.FG == "exotic_grass",], aes(y=alpha_name, x=mean_alpha, color = treatment)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_errorbarh(aes(xmin = mean_alpha-se_alpha, xmax = mean_alpha + se_alpha), height = 0.75, color = "black") +
  geom_point(size = 2.75) +
  facet_wrap(~invader.FG) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#003366", "#FFA630")) +
  
  facet_wrap(~treatment) +
  coord_cartesian(xlim = c(0.05, -5.6)) +
  scale_x_reverse() +
  ylab("Exotic Grasses") #+
  #ggtitle("Exotic Grass Facilitation")


ggarrange(native_forbs, exotic_forbs, exotic_grasses, 
          ncol = 1, 
          nrow = 3, 
          common.legend = T)


ggsave("models/CW/preliminary_figures/facilitation.png", height = 7, width = 9.5)

# All Interxns ####

ggplot(all_alphas_resident_traits[all_alphas_resident_traits$alpha_name != "alpha_avba",], aes(y=mean_alpha, x=Height_cm.y, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, linetype = "dashed")


check <- all_alphas_resident_traits %>%
  filter(mean_alpha > 10)



# PCA ####
adults.pca <- c("Height_cm", "SLA_cm2.g", "LWC", "CN")
## the columns that actually go in the pca

pca <- prcomp(trait[, adults.pca], scale = T) ## give pca function the relevant columns of adult traits, but no id info
summary(pca)

adults.pca <- cbind(trait, pca$x[,1:4])


autoplot(pca, x = 1, y = 2, data = adults.pca, frame = F, loadings = T, loadings.label = T, label = F, size = 2) +
  theme_classic() +
  #geom_text(aes(label = code, col = Rating)) +
  #stat_ellipse(aes(group = group)) + 
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    legend.title = element_blank())
# ) +
# scale_color_manual(values = c(adhesive = "#1B9E77", ant = "red4", unassisted = "darkgoldenrod3", ingestion = "#7570B3", wind = "#F17236"))



interaction.w.pca <- merge(all_alphas_resident_traits, adults.pca, by.x="resident", by.y = "code_4")

ggplot(interaction.w.pca, aes(x=PC1, y=mean_alpha)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(interaction.w.pca, aes(x=PC2, y=mean_alpha)) +
  geom_point() +
  geom_smooth(method = "lm")


mean_interaction <- all_alphas_resident_traits %>%
  group_by(alpha_name, treatment, invader.FG) %>%
  summarise(overall_alpha = mean(mean_alpha), se_overall_alpha = calcSE(mean_alpha)) 

ggplot(mean_interaction[mean_interaction$alpha_name != "alpha_avba",], aes(x=alpha_name, y=overall_alpha, color = invader.FG)) +
  geom_point() +
  geom_errorbar(aes(ymin = overall_alpha-se_overall_alpha, ymax = overall_alpha + se_overall_alpha), width = 0.25) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~treatment) +
  theme(axis.text.x = element_text(angle = 90))
  






