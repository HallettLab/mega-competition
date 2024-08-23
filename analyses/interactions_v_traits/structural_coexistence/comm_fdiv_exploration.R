# Set up ####
library(ggpubr)
library(wesanderson)
library(khroma)

fig_loc = "analyses/interactions_v_traits/structural_coexistence/prelim_figs/"
theme_set(theme_classic())

# Read in data ####
source("analyses/interactions_v_traits/structural_coexistence/calc_comm_fdiv.R")

# Summarise data ####
allcomm_sum = allcomm_fdiv %>%
  group_by(comp, treatment, origin, num.inv, fdiv, ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL) %>%
  summarise(num_feas = sum(feasibility),
            prop_feasible = num_feas/n(),
            mean_niche = mean(niche_diff),
            mean_fitness = mean(fitness_diff),
            se_niche = calcSE(niche_diff),
            se_fitness = calcSE(fitness_diff)) %>%
  filter(!(origin == "Mixed" & num.inv == 4))

allcomm_sum$origin = as.factor(allcomm_sum$origin)

allcomm_sum = allcomm_sum %>%
  mutate(origin = fct_relevel(origin, "Mixed", "Native", "Invasive"))

## double check ####
#check = allcomm_sum %>%
 # filter(origin == "Mixed", num.inv == 4)

# Visualize ####
## prop feas by trt & origin ####
ggplot(allcomm_sum, aes(x=treatment, y=prop_feasible)) +
  geom_jitter(size = 3, aes(color = origin, shape = treatment)) +
  geom_boxplot(alpha = .001, linewidth = 0.75) +
  ylab("Proportion of Coexistence") +
  scale_color_manual(values = c("#52BCA3", "#5D69B1", "#fab14f")) +
  scale_shape_manual(values = c(16, 1)) +
  theme(text = element_text(size = 15)) +
  xlab("Rainfall Treatment") +
  facet_wrap(~origin) +
  labs(color = "Origin", shape = "Rainfall") +
  theme(legend.position="bottom")
ggsave(paste0(fig_loc, "prop_feas_rainfall_origin.png"), width = 8, height = 4.5)

ggplot(allcomm_sum, aes(x=treatment, y=prop_feasible)) +
  geom_jitter(size = 3, aes(color = origin, shape = treatment)) +
  geom_boxplot(alpha = .001, linewidth = 0.75) +
  ylab("Proportion of Coexistence") +
  scale_color_manual(values = c("#52BCA3", "#5D69B1", "#fab14f")) +
  scale_shape_manual(values = c(16, 1)) +
  theme(text = element_text(size = 15)) +
  xlab("Rainfall Treatment") +
  facet_wrap(~num.inv, ncol = 5) +
  labs(color = "Origin", shape = "Rainfall") +
  theme(legend.position="bottom")

ggplot(allcomm_sum, aes(x=prop_feasible, y = after_stat(density), fill = treatment)) +
  geom_histogram() +
  facet_wrap(~num.inv) +
  xlab("Proportion of Coexistence")

## all 3 together ####
pf = ggplot(allcomm_sum, aes(x=fdiv, y=prop_feasible, linetype = treatment, shape = treatment))+
  geom_point(size = 2, color = "lightgray") +
  geom_smooth(method = "lm", alpha = 0.15, color = "black") +
  scale_color_batlow() +
  xlab("Functional Diversity") +
  ylab("Coexistence Proportion")  +
  theme(text = element_text(size = 15)) +
  scale_shape_manual(values = c(16, 1)) +
  labs(shape = "Rainfall Treatment", linetype = NULL) +
  facet_wrap(~num.inv, nrow = 1, ncol = 5) +
  theme(legend.position="bottom")

ndiff = ggplot(allcomm_sum, aes(x=fdiv, y=mean_niche, linetype = treatment, shape = treatment))+
  geom_point(size = 2, color = "lightgray") +
  geom_smooth(method = "lm", alpha = 0.15, color = "black") +
  scale_color_batlow() +
  xlab("Functional Diversity") +
  ylab("Niche Differences")  +
  theme(text = element_text(size = 15)) +
  scale_shape_manual(values = c(16, 1)) +
  labs(shape = "Rainfall Treatment", linetype = NULL) +
  facet_wrap(~num.inv, nrow = 1, ncol = 5) +
  theme(legend.position="bottom")

fdiff = ggplot(allcomm_sum, aes(x=fdiv, y=mean_fitness, linetype = treatment, shape = treatment))+
  geom_point(size = 2, color = "lightgray") +
  geom_smooth(method = "lm", alpha = 0.15, color = "black") +
  scale_color_batlow() +
  xlab("Functional Diversity") +
  ylab("Fitness Differences")  +
  theme(text = element_text(size = 15)) +
  scale_shape_manual(values = c(16, 1)) +
  labs(shape = "Rainfall Treatment", linetype = NULL) +
  facet_wrap(~num.inv, nrow = 1, ncol = 5) +
  theme(legend.position="bottom")

ggarrange(pf, ndiff, fdiff, ncol = 1, nrow = 3, common.legend = T, legend = "bottom")

ggsave(paste0(fig_loc, "pf_ndiff_fdiff_fdiv.png"), width = 10, height = 9)

## prop feasible by fdiv ####
ggplot(allcomm_sum, aes(x=fdiv, y=prop_feasible, linetype = treatment, shape = treatment))+
  geom_point(size = 2, color = "lightgray") +
  geom_smooth(method = "lm", alpha = 0.15, color = "black") +
  scale_color_batlow() +
  xlab("Functional Diversity") +
  ylab("Niche Differences")  +
  theme(text = element_text(size = 15)) +
  scale_shape_manual(values = c(16, 1)) +
  labs(shape = "Rainfall Treatment", linetype = NULL) +
  facet_wrap(~num.inv, nrow = 1, ncol = 5, scales = "free") +
  theme(legend.position="bottom")
ggsave(paste0(fig_loc, "fdiv_propfeas_rainfall_num_inv.png"), width = 10, height = 4)

ggplot(allcomm_sum, aes(x=fdiv, y=prop_feasible, color = origin))+
  geom_point(size = 2) +
  geom_smooth(method = "lm", alpha = 0.15, color = "black") +
  scale_color_manual(values = c("#52BCA3", "#5D69B1", "#fab14f")) +
  ylab("Proportion of Coexistence") +
  xlab("Functional Diversity")  +
  scale_shape_manual(values = c(16, 1)) +
  theme(text = element_text(size = 15)) +
  labs(color = "Origin", shape = "Rainfall", linetype = NULL) +
  facet_grid(treatment~origin) +
  theme(legend.position="bottom")
ggsave(paste0(fig_loc, "prop_feas_fdiv_rainfall_origin.png"), width = 8, height = 4.5)

ggplot(allcomm_sum, aes(x=fdiv, y=prop_feasible, color = origin))+
  geom_point(size = 2) +
  geom_smooth(method = "lm", alpha = 0.15, color = "black") +
  scale_color_manual(values = c("#52BCA3", "#5D69B1", "#fab14f")) +
  ylab("Proportion of Coexistence") +
  xlab("Functional Diversity")  +
  scale_shape_manual(values = c(16, 1)) +
  theme(text = element_text(size = 15)) +
  labs(color = "Origin", shape = "Rainfall", linetype = NULL) +
  facet_grid(treatment~num.inv) +
  theme(legend.position="bottom")

## niche diff by fdiv ####
ggplot(allcomm_sum, aes(x=fdiv, y=mean_niche, linetype = treatment, shape = treatment))+
  geom_point(size = 2, color = "lightgray") +
  geom_smooth(method = "lm", alpha = 0.15, color = "black") +
  scale_color_batlow() +
  xlab("Functional Diversity") +
  ylab("Niche Differences")  +
  theme(text = element_text(size = 15)) +
  scale_shape_manual(values = c(16, 1)) +
  labs(shape = "Rainfall Treatment", linetype = NULL) +
  facet_wrap(~num.inv, nrow = 1, ncol = 5) +
  theme(legend.position="bottom")
ggsave(paste0(fig_loc, "fdiv_ndiff_rainfall_num_inv.png"), width = 10, height = 4.5)

ggplot(allcomm_sum, aes(x=fdiv, y=mean_niche, color = origin))+
  geom_point(size = 2) +
  geom_smooth(method = "lm", alpha = 0.15, color = "black") +
  scale_color_manual(values = c("#52BCA3", "#5D69B1", "#fab14f")) +
  ylab("Niche Differences") +
  xlab("Functional Diversity")  +
  scale_shape_manual(values = c(16, 1)) +
  theme(text = element_text(size = 15)) +
  labs(color = "Origin", shape = "Rainfall", linetype = NULL) +
  facet_grid(treatment~origin) +
  theme(legend.position="bottom")
ggsave(paste0(fig_loc, "ndiff_fdiv_rainfall_origin.png"), width = 8, height = 4.5)

ggplot(allcomm_sum, aes(x=fdiv, y=mean_niche, color = origin))+
  geom_point(size = 2) +
  geom_smooth(method = "lm", alpha = 0.15, color = "black") +
  scale_color_manual(values = c("#52BCA3", "#5D69B1", "#fab14f")) +
  ylab("Niche Differences") +
  xlab("Functional Diversity")  +
  scale_shape_manual(values = c(16, 1)) +
  theme(text = element_text(size = 15)) +
  labs(color = "Origin", shape = "Rainfall", linetype = NULL) +
  facet_grid(treatment~num.inv) +
  theme(legend.position="bottom")

## fitness diff by fdiv ####
ggplot(allcomm_sum, aes(x=fdiv, y=mean_fitness, linetype = treatment, shape = treatment))+
  geom_point(size = 2, color = "lightgray") +
  geom_smooth(method = "lm", alpha = 0.15, color = "black") +
  scale_color_batlow() +
  xlab("Functional Diversity") +
  ylab("Fitness Differences")  +
  theme(text = element_text(size = 15)) +
  scale_shape_manual(values = c(16, 1)) +
  labs(shape = "Rainfall Treatment", linetype = NULL) +
  facet_wrap(~num.inv, nrow = 1, ncol = 5) +
  theme(legend.position="bottom")
ggsave(paste0(fig_loc, "fdiv_fdiff_rainfall_num_inv.png"), width = 10, height = 4.5)

ggplot(allcomm_sum, aes(x=fdiv, y=mean_fitness, color = origin))+
  geom_point(size = 2) +
  geom_smooth(method = "lm", alpha = 0.15, color = "black") +
  scale_color_manual(values = c("#52BCA3", "#5D69B1", "#fab14f")) +
  ylab("Fitness Differences") +
  xlab("Functional Diversity")  +
  scale_shape_manual(values = c(16, 1)) +
  theme(text = element_text(size = 15)) +
  labs(color = "Origin", shape = "Rainfall", linetype = NULL) +
  facet_grid(treatment~origin) +
  theme(legend.position="bottom")
ggsave(paste0(fig_loc, "fdiff_fdiv_rainfall_origin.png"), width = 8, height = 4.5)

## ndiff v fdiff ####
ggplot(allcomm_sum, aes(x=mean_niche, y=mean_fitness, color = prop_feasible))+
  geom_point(size = 2) +
 # geom_smooth(method = "lm", alpha = 0.15, color = "black") +
  #scale_color_manual(values = c("#52BCA3", "#5D69B1", "#fab14f")) +
  scale_color_batlow() +
  ylab("Fitness Differences") +
  xlab("Niche Differences")  +
  #scale_shape_manual(values = c(16, 1)) +
  theme(text = element_text(size = 15)) +
  labs(color = "Proportion of Coexistence") +
  facet_grid(treatment~origin) +
  theme(legend.position="bottom")
ggsave(paste0(fig_loc, "fdiff_ndiff_rainfall_origin.png"), width = 8, height = 4.5)

ggplot(allcomm_sum, aes(x=mean_niche, y=mean_fitness, color = fdiv))+
  geom_point(size = 2) +
  scale_color_batlow() +
  ylab("Fitness Differences") +
  xlab("Niche Differences")  +
  theme(text = element_text(size = 15)) +
  labs(color = "Functional Diversity") +
  facet_grid(treatment~num.inv) +
  theme(legend.position="bottom")
ggsave(paste0(fig_loc, "fdiff_ndiff_rainfall_num_inv.png"), width = 10, height = 4.5)



ggplot(allcomm_sum, aes(x=fdiv, y=prop_feasible))+
  geom_point(size = 2) +
  geom_smooth(method = "lm", alpha = 0.15, color = "black") +
  #scale_color_manual(values = c("#52BCA3", "#5D69B1", "#fab14f")) +
  scale_color_batlow() +
  xlab("Functional Diversity") +
  ylab("Coexistence Proportion")  +
  #scale_shape_manual(values = c(16, 1)) +
  theme(text = element_text(size = 15)) +
  labs(color = "Proportion of Coexistence") +
  #facet_grid(treatment~origin) +
  facet_wrap(~num.inv) +
  theme(legend.position="bottom")



ggplot(allcomm_sum, aes(x=fdiv, y=mean_fitness))+
  geom_point(size = 2, alpha = 0.05) +
  geom_smooth(method = "lm", alpha = 0.15, color = "black") +
  scale_color_batlow() +
  xlab("Functional Diversity") +
  ylab("Coexistence Proportion")  +
  theme(text = element_text(size = 15)) +
  labs(color = "Proportion of Coexistence") +
  facet_wrap(~num.inv) +
  theme(legend.position="bottom")

# Sp P/A ####
legume = allcomm_sum %>%
  mutate(with_legume = ifelse((TWIL + THIR + ACAM) > 0, 1, 0))

ggplot(legume, aes(x=as.factor(with_legume), y=prop_feasible)) +
  geom_jitter(aes(color = origin)) +
  geom_boxplot(alpha = .001, linewidth = 0.75) +
  facet_wrap(~origin) +
  scale_color_manual(values = c("#52BCA3", "#5D69B1", "#fab14f")) +
  xlab("Legume Presence") +
  theme(text = element_text(size = 15)) +
  ylab("Proportion of Coexistence") +
  labs(color = "Origin")
ggsave(paste0(fig_loc, "legume_prop_feas.png"), width = 8, height = 4.5)

pa_sums = allcomm_sum %>%
  filter(!is.na(prop_feasible)) %>%
  mutate(feas_comm = ifelse(prop_feasible > 0, "Yes", "No")) %>%
  group_by(feas_comm, treatment) %>%
  summarise(AMME_pres = sum(AMME)/n(),
            GITR_pres = sum(GITR)/n(),
            LENI_pres = sum(LENI)/n(),
            MAEL_pres = sum(MAEL)/n(),
            MICA_pres = sum(MICA)/n(),
            PLER_pres = sum(PLER)/n(),
            PLNO_pres = sum(PLNO)/n(),
            ACAM_pres = sum(ACAM)/n(),
            TWIL_pres = sum(TWIL)/n(),
            THIR_pres = sum(THIR)/n(),
            ANAR_pres = sum(ANAR)/n(),
            BRHO_pres = sum(BRHO)/n(),
            BRNI_pres = sum(BRNI)/n(),
            CESO_pres = sum(CESO)/n(),
            LOMU_pres = sum(LOMU)/n(),
            TACA_pres = sum(TACA)/n()) %>%
  pivot_longer(3:18, names_to = "species", values_to = "perc_pres")

pa_sums$species = as.factor(pa_sums$species)

pa_sums = pa_sums %>%
  mutate(species = fct_relevel(species, "AMME_pres", "MAEL_pres", "PLNO_pres", "GITR_pres", "LENI_pres",  "MICA_pres", "PLER_pres",  "ACAM_pres", "TWIL_pres", "THIR_pres", "ANAR_pres", "BRHO_pres", "LOMU_pres", "TACA_pres", "BRNI_pres", "CESO_pres"))

ggplot(pa_sums, aes(x=feas_comm, y=species, fill = perc_pres)) +
  geom_tile(color = "black") +
  scale_fill_batlow() +
  facet_wrap(~treatment) +
  ylab(NULL) +
  xlab("Community Coexists") +
  labs(fill = "% Presence")
ggsave(paste0(fig_loc, "sp_percent_presence_heatmap.png"), width = 7, height = 6)

## for each species, find all communities that it is present in and then calculate the percent of these that are feasible (to any degree) and the percent that are not
pa_sums_alt = allcomm_sum %>%
  filter(!is.na(prop_feasible)) %>%
  mutate(feas_comm = ifelse(prop_feasible > 0, "Yes", "No")) %>%
  pivot_longer(6:21, names_to = "species", values_to = "pres_abs") %>%
  group_by(species, treatment) %>% 
  mutate(ncomm = sum(pres_abs)) %>%
  group_by(species, treatment, feas_comm) %>%
  summarise(sp_percent = sum(pres_abs)/ncomm) %>%
  distinct()

pa_sums_alt$species = as.factor(pa_sums_alt$species)

pa_sums_alt = pa_sums_alt %>%
  mutate(species = fct_relevel(species, "AMME", "MAEL", "PLNO", "GITR", "LENI",  "MICA", "PLER",  "ACAM", "TWIL", "THIR", "ANAR", "BRHO", "LOMU", "TACA", "BRNI", "CESO"))


ggplot(pa_sums_alt, aes(x=feas_comm, y=species, fill = sp_percent)) +
  geom_tile(color = "black") +
  scale_fill_batlow() +
  facet_wrap(~treatment) +
  ylab(NULL) +
  xlab("Community Coexists") +
  labs(fill = "% of Communities")
ggsave(paste0(fig_loc, "pres_abs_heatmap_updated.png"), width = 7, height = 6)










ggplot(pa_sums[!is.na(pa_sums$feas_comm),], aes(x=feas_comm, y=species, fill = perc_pres)) +
  geom_tile(color = "black") +
  scale_fill_batlow() +
  facet_wrap(~treatment)

ggsave(paste0(fig_loc, "pres_abs_heatmap_noNAs.png"), width = 7, height = 6)

## invasive sp ####
ggplot(inv_prop_feas, aes(x=fdiv, y=prop_feasible))+
  geom_point(aes(color = as.factor(BRNI))) +
  geom_smooth(method = "lm") 


bn = ggplot(inv_prop_feas, aes(x=as.factor(BRNI), y=prop_feasible)) +
  geom_boxplot() +
  ggtitle("Brassica Nigra") +
  ylab(NULL) +
  xlab(NULL) +
  theme(text = element_text(size = 10))
ta = ggplot(inv_prop_feas, aes(x=as.factor(TACA), y=prop_feasible)) +
  geom_boxplot() +
  ggtitle("Taeniatherum caput-medusae") +
  ylab(NULL) +
  xlab(NULL) +
  theme(text = element_text(size = 10))

ggarrange(bn, ta,
          ncol = 1, nrow = 2)
ggsave(paste0(fig_loc, "inv_4sp_PA_prop_feasible.png"), width = 2.65, height = 3.25)


a = ggplot(inv_prop_feas, aes(x=as.factor(ANAR), y=prop_feasible)) +
  geom_boxplot() +
  ggtitle("Anagallis arvensis") +
  ylab(NULL) +
  xlab(NULL)+
  theme(text = element_text(size = 10))
th = ggplot(inv_prop_feas, aes(x=as.factor(THIR), y=prop_feasible)) +
  geom_boxplot() +
  ggtitle("Trifolium hirtum") +
  ylab(NULL) +
  xlab(NULL)+
  theme(text = element_text(size = 10)) 

ggarrange(a, th,
          ncol = 1, nrow = 2)
ggsave(paste0(fig_loc, "inv_4sp_PA_prop_feasible2.png"), width = 2.65, height = 3.25)







