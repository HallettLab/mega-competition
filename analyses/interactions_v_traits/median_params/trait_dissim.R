# Set up ####
library(ggpubr)

source("analyses/traits/trait_pcas_exploration.R")
source("analyses/interactions_v_traits/median_params/median_params_&_traits.R")

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
  select(combo, phyto, resident, treatment, median_parameter, parameter_type, alpha_scaled, hdi_hi_scaled, hdi_lo_scaled)

dissim.params <- left_join(alpha_sc_tmp, dissim.long, by = c("combo")) %>%
  mutate(resident.fg = ifelse(resident %in% c("TACA", "BRHO", "LOMU"), "Grass",
                     ifelse(resident %in% c("TWIL", "THIR", "ACAM"), "Legume", "Forb")),
         phyto.fg = ifelse(phyto %in% c("TACA", "BRHO", "LOMU"), "Grass",
                             ifelse(phyto %in% c("TWIL", "THIR", "ACAM"), "Legume", "Forb")),
         resident = fct_relevel(resident, "ACAM", "THIR", "TWIL", "BRHO", "LOMU", "TACA", "AMME", "GITR", "LENI", "MAEL", "MICA", "PLER", "PLNO", "ANAR", "BRNI", "CESO"), 
         phyto = fct_relevel(phyto, "ACAM", "THIR", "TWIL", "BRHO", "LOMU", "TACA", "AMME", "GITR", "LENI", "MAEL", "MICA", "PLER", "PLNO", "ANAR", "BRNI", "CESO")) %>%
  filter(dissimilarity != 0) %>% ## remove intra-specific alphas
  mutate(phyto.fg = ifelse(phyto %in% c("AMME", "BRNI", "CESO", "MAEL", "PLNO"), "Large Forb", phyto.fg), 
         resident.fg = ifelse(resident %in% c("AMME", "BRNI", "CESO", "MAEL", "PLNO"), "Large Forb", phyto.fg))

# Visualize ####
## Figure 1: ####
ggplot(dissim.params, aes(x=dissimilarity, y=alpha_scaled, shape = treatment, color = resident.fg)) +
  geom_errorbar(aes(ymin = hdi_lo_scaled, ymax = hdi_hi_scaled), width = 0.15) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", aes(linetype = treatment), color = "black", alpha = 0.15) +
  facet_wrap(~phyto.fg, scales = "free") +
  ylab("Interaction Coefficient") + xlab("") +
  
  scale_color_manual(values = c("#ECB159", "#8CB9BD", "#156882", "gray")) +
  scale_shape_manual(values = c(15,16)) +
  theme(legend.position="bottom") +
  labs(color = "Resident FG", shape = "Rainfall", linetype = "Rainfall") +
  theme(text = element_text(size = 15)) +
  facet_wrap(~phyto.fg, scales = "free")

ggsave(paste0(fig_loc, "dissim_v_scaled_median_alphas_fg_facet_w_error.png"), width = 10, height = 4)

## POSTER VERSION ####
forb <- ggplot(dissim.params[dissim.params$phyto.fg == "Forb",], aes(x=dissimilarity, y=alpha_scaled, shape = treatment)) +
  geom_point(aes(color = resident.fg), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", aes(linetype = treatment), color = "black", alpha = 0.15) +
  facet_wrap(~phyto.fg, scales = "free") +
  ylab("Interaction Coefficient") + xlab("") +
  scale_color_manual(values = c("#ECB159", "#8CB9BD", "#156882")) +
  scale_shape_manual(values = c(15,16)) +
  theme(legend.position="bottom") +
  labs(color = "Resident FG", shape = "Rainfall", linetype = "Rainfall") +
  theme(text = element_text(size = 15)) +
  theme(plot.background = element_rect(fill = "#FEFBF6"),
        panel.background = element_rect(fill = "#FEFBF6",
                                        colour = "#FEFBF6"),
        legend.key = element_rect(fill = "#FEFBF6"),
        legend.background = element_rect(fill = "#FEFBF6"))

grass <- ggplot(dissim.params[dissim.params$phyto.fg == "Grass",], aes(x=dissimilarity, y=alpha_scaled, shape = treatment)) +
  geom_point(aes(color = resident.fg), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", aes(linetype = treatment), color = "black", alpha = 0.15) +
  facet_wrap(~phyto.fg, scales = "free") +
  ylab(NULL) + xlab("Trait Dissimilarity") +
  scale_color_manual(values = c("#ECB159", "#8CB9BD", "#156882")) +
  scale_shape_manual(values = c(15,16)) +
  theme(legend.position="bottom") +
  labs(color = "Resident FG", shape = "Rainfall", linetype = "Rainfall") +
  theme(text = element_text(size = 15)) +
  theme(plot.background = element_rect(fill = "#FEFBF6"),
        panel.background = element_rect(fill = "#FEFBF6",
                                        colour = "#FEFBF6"),
        legend.key = element_rect(fill = "#FEFBF6"),
        legend.background = element_rect(fill = "#FEFBF6"))

legume <- ggplot(dissim.params[dissim.params$phyto.fg == "Legume",], aes(x=dissimilarity, y=alpha_scaled, shape = treatment)) +
  geom_point(aes(color = resident.fg), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", aes(linetype = treatment), color = "black", alpha = 0.15) +
  facet_wrap(~phyto.fg, scales = "free") +
  ylab(NULL) + xlab("") +
  scale_color_manual(values = c("#ECB159", "#8CB9BD", "#156882")) +
  scale_shape_manual(values = c(15,16)) +
  theme(legend.position="bottom") +
  labs(color = NULL, shape = "Rainfall", linetype = "Rainfall") +
  theme(text = element_text(size = 15)) +
  theme(plot.background = element_rect(fill = "#FEFBF6"),
        panel.background = element_rect(fill = "#FEFBF6",
                                        colour = "#FEFBF6"),
        legend.key = element_rect(fill = "#FEFBF6"),
        legend.background = element_rect(fill = "#FEFBF6")) 

ggarrange(forb, grass, legume,
          nrow = 1, ncol = 3,
          common.legend = TRUE,
          legend = "bottom",
          labels = "AUTO")

ggsave(paste0(fig_loc, "dissim_v_scaled_median_alphas_fg_facet_newcolor.png"), width = 10, height = 4)

t <- lm(alpha_scaled ~ dissimilarity + phyto.fg, data = dissim.params)
summary(t)
