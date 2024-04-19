
# Set up ####
library(fundiversity)

source()

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
  mutate(resident.fg = ifelse(resident %in% c("TACA", "BRHO", "LOMU"), "grass",
                     ifelse(resident %in% c("TWIL", "THIR", "ACAM"), "legume", "forb")),
         phyto.fg = ifelse(phyto %in% c("TACA", "BRHO", "LOMU"), "grass",
                             ifelse(phyto %in% c("TWIL", "THIR", "ACAM"), "legume", "forb")),
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

ggplot(dissim.params, aes(x=dissimilarity, y=alpha_scaled, color = phyto.fg)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm") +
  facet_wrap(~phyto, scales = "free") +
  ylab("Scaled intersp alpha value") + xlab("Trait Dissimilarity") +
  ggtitle("Faceted by 'Invader' Species") +
  scale_color_manual(values = c("#5D69B1","#99C945", "#CC61B0"))

ggsave("analyses/explore_interaction_coeff/preliminary_figures/dissim_scaled_alpha_invaderfacet_fg.png", width = 7, height = 6)


