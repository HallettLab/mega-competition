
# Set up ####
library(fundiversity)
library(ggpubr)
#source()

# Calc Dissim ####
## calculate trait dissimilarities
trait_sums <- MC.pca.ID %>%
  mutate(species = phyto) %>%
  group_by(species) %>%
  ## calculate mean trait values
  summarise(mean.height = mean(Height), 
            mean.LDMC = mean(LDMC),
            mean.SLA = mean(SLA), 
            mean.RMF = mean(RMF), 
            mean.CRSL = mean(CRSL), 
            mean.D = mean(D),
            mean.PF = mean(PF))

## change data to matrix format
trait.matrix <- as.matrix(trait_sums[,-1])

## set rownames
rownames(trait.matrix) <- unique(trait_sums$species) 

## scale trait data
trait.matrix.sc <- scale(trait.matrix)

#fd_raoq(trait.matrix.sc[1:2,])
#fd_raoq(trait.matrix.sc[2:3,])

## calculate the dissimilarity matrix b/w species
dissim <- dist(trait.matrix.sc)

## save as a dataframe
temp.dissim <- as.matrix(dissim)
dissim.matrix <- as.data.frame(temp.dissim)
dissim.matrix$species <- row.names(dissim.matrix)

## reformat
dissim.long <- dissim.matrix %>%
  pivot_longer(cols = c(1:16), names_to = "species2", values_to = "dissimilarity") %>%
  mutate(combo = paste(species, species2, sep = "_")) %>%
  select(combo, dissimilarity)

# Join with params data ####
alpha_sc_tmp <- alpha_sc %>%
  mutate(combo = paste(phyto, resident, sep = "_")) %>%
  select(combo, phyto, resident, treatment, median_parameter, parameter_type, alpha_scaled)

dissim.params <- left_join(alpha_sc_tmp, dissim.long, by = c("combo")) %>%
  mutate(resident.fg = ifelse(resident %in% c("TACA", "BRHO", "LOMU"), "Grass",
                     ifelse(resident %in% c("TWIL", "THIR", "ACAM"), "Legume", "Forb")),
         phyto.fg = ifelse(phyto %in% c("TACA", "BRHO", "LOMU"), "Grass",
                             ifelse(phyto %in% c("TWIL", "THIR", "ACAM"), "Legume", "Forb")),
         resident = fct_relevel(resident, "ACAM", "THIR", "TWIL", "BRHO", "LOMU", "TACA", "AMME", "GITR", "LENI", "MAEL", "MICA", "PLER", "PLNO", "ANAR", "BRNI", "CESO"), 
         phyto = fct_relevel(phyto, "ACAM", "THIR", "TWIL", "BRHO", "LOMU", "TACA", "AMME", "GITR", "LENI", "MAEL", "MICA", "PLER", "PLNO", "ANAR", "BRNI", "CESO")) %>%
  filter(dissimilarity != 0) ## remove intra-specific alphas

# Visualize ####
## raw alpha v dissim ####
ggplot(dissim.params, aes(x=dissimilarity, y=median_parameter)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm") +
  ylab("Intersp alpha value") + xlab("Trait Dissimilarity") +
  facet_wrap(~treatment)
ggsave("analyses/explore_interaction_coeff/preliminary_figures/dissim_inter_alpha_trt.png", width = 6, height = 3.5)

## scaled alpha v dissim ####
ggplot(dissim.params, aes(x=dissimilarity, y=alpha_scaled)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm") +
  ylab("Scaled intersp alpha value") + xlab("Trait Dissimilarity") +
  facet_wrap(~treatment, scales = "free")
ggsave("analyses/explore_interaction_coeff/preliminary_figures/dissim_inter_alpha_sc_trt.png", width = 6, height = 3.5)

## raw alpha facet by resident sp ####
ggplot(dissim.params, aes(x=dissimilarity, y=median_parameter, color = resident.fg)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm") +
  facet_wrap(~resident, scales = "free") +
  ylab("Raw intersp alpha value") + xlab("Trait Dissimilarity") +
  ggtitle("Faceted by 'Resident' Species") +
  scale_color_manual(values = c("#5D69B1","#99C945", "#CC61B0"))
ggsave("analyses/explore_interaction_coeff/preliminary_figures/dissim_alpha_residentfacet_fg.png", width = 7, height = 6)

## faceted by resident, points colored by phyto fg
ggplot(dissim.params, aes(x=dissimilarity, y=median_parameter)) +
  geom_point(aes(color = phyto.fg)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black") +
  facet_wrap(~resident, scales = "free") +
  ylab("Alpha value") + xlab("Trait Dissimilarity") +
  ggtitle("Faceted by 'Resident' Species") +
  scale_color_manual(values = c("#5D69B1","#99C945", "#CC61B0"))

## raw alpha facet by invader sp ####
ggplot(dissim.params, aes(x=dissimilarity, y=median_parameter,  color = phyto.fg)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm") +
  facet_wrap(~phyto, scales = "free") +
  ylab("Raw intersp alpha value") + xlab("Trait Dissimilarity") +
  ggtitle("Faceted by 'Invader' Species") +
  labs(color = "Invader FG") +
  scale_color_manual(values = c("#5D69B1","#99C945", "#CC61B0"))
ggsave("analyses/explore_interaction_coeff/preliminary_figures/dissim_alpha_invaderfacet_fg.png", width = 7, height = 6)

## scaled alpha facet by resident sp ####
ggplot(dissim.params, aes(x=dissimilarity, y=alpha_scaled, color = resident.fg)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm") +
  facet_wrap(~resident, scales = "free") +
  ylab("Scaled intersp alpha value") + xlab("Trait Dissimilarity") +
  ggtitle("Faceted by 'Resident' Species") +
  scale_color_manual(values = c("#5D69B1","#99C945", "#CC61B0"))
ggsave("analyses/explore_interaction_coeff/preliminary_figures/dissim_scaled_alpha_residentfacet_fg.png", width = 7, height = 6)

ggplot(dissim.params, aes(x=dissimilarity, y=alpha_scaled)) +
  geom_point(aes(color = resident.fg)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", alpha = 0.15) +
  facet_wrap(~phyto, scales = "free") +
  ylab("Scaled intersp alpha value") + xlab("Trait Dissimilarity") +
  ggtitle("Faceted by 'Invader' Species") +
  scale_color_manual(values = c("#5D69B1","#99C945", "#CC61B0"))

ggsave("analyses/explore_interaction_coeff/preliminary_figures/dissim_scaled_alpha_invaderfacet_residentfg.png", width = 7, height = 6)

## FG FACET ####
forb <- ggplot(dissim.params[dissim.params$phyto.fg == "Forb",], aes(x=dissimilarity, y=alpha_scaled, shape = treatment)) +
  geom_point(aes(color = resident.fg), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", aes(linetype = treatment), color = "black", alpha = 0.15) +
  facet_wrap(~phyto.fg, scales = "free") +
  ylab("Interaction Coefficient") + xlab("") +
 # ggtitle("Faceted by 'Invader' FG") +
  scale_color_manual(values = c("#ECB159", "#8CB9BD", "#156882")) +
  scale_shape_manual(values = c(15,16)) +
  theme(legend.position="bottom") +
  labs(color = "Resident FG", shape = "Rainfall", linetype = "Rainfall") +
  theme(text = element_text(size = 15)) +
  theme(plot.background = element_rect(fill = "#FEFBF6"),
        panel.background = element_rect(fill = "#FEFBF6",
                                        colour = "#FEFBF6"),
        legend.key = element_rect(fill = "#FEFBF6"),
        legend.background = element_rect(fill = "#FEFBF6")) + 
  annotate(geom="text", x =1.05, y=0.001, label = "competition", size = 4, angle='90') +
  annotate(geom="text", x =1.05, y=-0.001, label = "facilitation", size = 4, angle='90') 

grass <- ggplot(dissim.params[dissim.params$phyto.fg == "Grass",], aes(x=dissimilarity, y=alpha_scaled, shape = treatment)) +
  geom_point(aes(color = resident.fg), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", aes(linetype = treatment), color = "black", alpha = 0.15) +
  facet_wrap(~phyto.fg, scales = "free") +
  ylab(NULL) + xlab("Trait Dissimilarity") +
  # ggtitle("Faceted by 'Invader' FG") +
  scale_color_manual(values = c("#ECB159", "#8CB9BD", "#156882")) +
  scale_shape_manual(values = c(15,16)) +
  theme(legend.position="bottom") +
  labs(color = "Resident FG", shape = "Rainfall", linetype = "Rainfall") +
  theme(text = element_text(size = 15)) +
  theme(plot.background = element_rect(fill = "#FEFBF6"),
        panel.background = element_rect(fill = "#FEFBF6",
                                        colour = "#FEFBF6"),
        legend.key = element_rect(fill = "#FEFBF6"),
        legend.background = element_rect(fill = "#FEFBF6"))# + 
  #annotate(geom="text", x =1.25, y=0.0008, label = "competition", size = 4, angle='90') +
  #annotate(geom="text", x =1.25, y=-0.0008, label = "facilitation", size = 4, angle='90') 

legume <- ggplot(dissim.params[dissim.params$phyto.fg == "Legume",], aes(x=dissimilarity, y=alpha_scaled, shape = treatment)) +
  geom_point(aes(color = resident.fg), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", aes(linetype = treatment), color = "black", alpha = 0.15) +
  facet_wrap(~phyto.fg, scales = "free") +
  ylab(NULL) + xlab("") +
  # ggtitle("Faceted by 'Invader' FG") +
  scale_color_manual(values = c("#ECB159", "#8CB9BD", "#156882")) +
  scale_shape_manual(values = c(15,16)) +
  theme(legend.position="bottom") +
  labs(color = NULL, shape = "Rainfall", linetype = "Rainfall") +
  theme(text = element_text(size = 15)) +
  theme(plot.background = element_rect(fill = "#FEFBF6"),
        panel.background = element_rect(fill = "#FEFBF6",
                                        colour = "#FEFBF6"),
        legend.key = element_rect(fill = "#FEFBF6"),
        legend.background = element_rect(fill = "#FEFBF6")) #+ 
  #annotate(geom="text", x =1.25, y=0.009, label = "competition", size = 4, angle='90') +
  #annotate(geom="text", x =1.25, y=-0.009, label = "facilitation", size = 4, angle='90') 

ggarrange(forb, grass, legume,
          nrow = 1, ncol = 3,
          common.legend = TRUE,
          legend = "bottom",
          labels = "AUTO")

ggsave("analyses/explore_interaction_coeff/preliminary_figures/storyboard_v2/dissim_v_scaled_median_alphas_fg_facet_newcolor.png", width = 10, height = 4)

t <- lm(alpha_scaled ~ dissimilarity + phyto.fg, data = dissim.params)
summary(t)

## FG x TRT FACET ####
ggplot(dissim.params, aes(x=dissimilarity, y=alpha_scaled, shape = treatment)) +
  geom_point(aes(color = resident.fg)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", aes(linetype = treatment), alpha = 0.15) +
  facet_grid(treatment~phyto.fg, scales = "free") +
  ylab("Scaled intersp median alpha value") + xlab("Trait Dissimilarity") +
  ggtitle("Faceted by 'Invader' FG") +
  scale_color_manual(values = c("#ECB159", "#8CB9BD", "#156882")) +
  labs(color = "Resident FG")

ggsave("analyses/explore_interaction_coeff/preliminary_figures/storyboard_v2/dissim_v_scaled_median_alphas_trt_fg_facet.png", width = 9, height = 3)


ggplot(dissim.params, aes(x=dissimilarity, y=median_parameter, shape = treatment)) +
  geom_point(aes(color = resident.fg)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", aes(linetype = treatment), alpha = 0.15) +
  facet_grid(treatment~phyto.fg, scales = "free") +
  ylab("Scaled intersp median alpha value") + xlab("Trait Dissimilarity") +
  ggtitle("Faceted by 'Invader' FG") +
  scale_color_manual(values = c("#ECB159", "#8CB9BD", "#156882")) +
  labs(color = "Resident FG")

ggplot(dissim.params, aes(x=dissimilarity, y=median_parameter, shape = treatment)) +
  geom_point(aes(color = resident.fg)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", aes(linetype = treatment), color = "black", alpha = 0.15) +
  facet_wrap(~phyto.fg, scales = "free") +
  ylab("Raw intersp median alpha value") + xlab("Trait Dissimilarity") +
  ggtitle("Faceted by 'Invader' FG") +
  scale_color_manual(values = c("#5D69B1","#99C945", "#CC61B0")) +
  labs(color = "Resident FG")

ggsave("analyses/explore_interaction_coeff/preliminary_figures/storyboard_v2/dissim_v_raw_median_alphas_trt_fg_facet.png", width = 9, height = 3)
