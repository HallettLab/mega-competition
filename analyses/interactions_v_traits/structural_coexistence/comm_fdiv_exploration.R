
# Set up ####
library(ggpubr)
library(wesanderson)

wes_palettes

fig_loc = "analyses/interactions_v_traits/structural_coexistence/prelim_figs/"
theme_set(theme_classic())

# Read in data ####
source("analyses/interactions_v_traits/calc_comm_fdiv.R")

# Summarise data ####
inv_prop_feas = allinv_fdiv %>%
  group_by(comp, treatment, fdiv) %>%
  summarise(num_feas = sum(feasibility),
            prop_feasible = num_feas/n(),
            mean_niche = mean(niche_diff),
            mean_fitness = mean(fitness_diff),
            se_niche = calcSE(niche_diff),
            se_fitness = calcSE(fitness_diff)) %>%
  mutate(origin = "invasive", 
         num.inv = 4)

nat_prop_feas = allnat_fdiv %>%
  filter(!is.na(feasibility)) %>%
  group_by(comp, treatment, fdiv) %>%
  summarise(num_feas = sum(feasibility),
            prop_feasible = num_feas/n(),
            mean_niche = mean(niche_diff),
            mean_fitness = mean(fitness_diff),
            se_niche = calcSE(niche_diff),
            se_fitness = calcSE(fitness_diff)) %>%
  mutate(origin = "native", 
         num.inv = 0)

mix_prop_feas = mix_fdiv %>%
  filter(!is.na(feasibility)) %>%
  mutate(num.inv = ANAR + BRHO + BRNI + CESO + LOMU + TACA + THIR) %>%
  group_by(comp, treatment, fdiv, num.inv) %>%
  summarise(num_feas = sum(feasibility),
            prop_feasible = num_feas/n(),
            mean_niche = mean(niche_diff),
            mean_fitness = mean(fitness_diff),
            se_niche = calcSE(niche_diff),
            se_fitness = calcSE(fitness_diff)) %>%
  filter(num.inv < 4) %>%
  mutate(origin = "mixed")

## join together ####
allcomm = rbind(nat_prop_feas, inv_prop_feas, mix_prop_feas) %>%
  mutate(origin = ifelse(origin == "native", "Native", 
                         ifelse(origin == "invasive", "Non-Native",
                                "Mixed")),
         treatment = ifelse(treatment == "C", "Ambient", "Drought"))

# Visualize ####
## prop feasible by treatment & origin
ggplot(allcomm, aes(x=treatment, y=prop_feasible)) +
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

## prop feasible by fdiv
ggplot(allcomm, aes(x=fdiv, y=prop_feasible, color = origin))+
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

## niche differences by fdiv
ggplot(allcomm, aes(x=fdiv, y=mean_niche, color = origin))+
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

## fitness differences by fdiv
ggplot(allcomm, aes(x=fdiv, y=mean_fitness, color = origin))+
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

ggplot(inv_prop_feas, aes(x=fdiv, y=prop_feasible))+
  geom_point(aes(color = as.factor(THIR))) +
  geom_smooth(method = "lm") 
ggplot(inv_prop_feas, aes(x=fdiv, y=prop_feasible))+
  geom_point(aes(color = as.factor(BRNI))) +
  geom_smooth(method = "lm") 


ggplot(inv_prop_feas, aes(x=fdiv, y=mean_niche))+
  geom_point() +
  geom_smooth(method = "lm")

ggplot(inv_prop_feas, aes(x=fdiv, y=mean_fitness))+
  geom_point() +
  geom_smooth(method = "lm")


## native sp ####
ggplot(nat_prop_feas, aes(x=fdiv, y=prop_feasible))+
  geom_point()+
  geom_smooth(method = "lm") +
  ylab("Proportion of Feasible Communities") +
  xlab("Functional Diversity") +
  ggtitle("Native Species")

ggplot(nat_prop_feas, aes(x=fdiv, y=mean_niche))+
  geom_point() +
  geom_smooth(method = "lm")

ggplot(nat_prop_feas, aes(x=fdiv, y=mean_fitness))+
  geom_point() +
  geom_smooth(method = "lm")



ggplot(allcomm, aes(x=fdiv, y=prop_feasible, color = origin))+
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("#5D69B1", "#E58606")) +
  ylab("Proportion of Feasible Communities") +
  xlab("Functional Diversity") #+
  #facet_wrap(~treatment)
ggsave(paste0(fig_loc, "fdiv_propfeas.png"), width = 6, height = 4)

### POSTER FIGURE ####
ggplot(allcomm, aes(x=fdiv, y=prop_feasible, color = origin, shape = treatment))+
  geom_point(size = 3) +
  geom_smooth(method = "lm", alpha = 0.15) +
  scale_color_manual(values = c("#5D69B1", "#fab14f")) +
  ylab("Coexistence Probability") +
  xlab("Functional Diversity")  +
  scale_shape_manual(values = c(16, 1)) +
  theme(text = element_text(size = 15)) +
  labs(color = "Origin", shape = "Rainfall", linetype = NULL) +
  facet_wrap(~treatment) +
  theme(legend.position="bottom")

ggsave(paste0(fig_loc, "ESA_fdiv_propfeas_trt.png"), width = 8, height = 4.5)

ggplot(allcomm, aes(x=fdiv, y=mean_niche, color = origin, shape = treatment))+
  geom_point(size = 3) +
  geom_smooth(method = "lm", alpha = 0.15) +
  scale_color_manual(values = c("#5D69B1", "#fab14f")) +
  xlab("Functional Diversity")  +
  ylab("Mean Community Niche Differences") +
  
  scale_shape_manual(values = c(16, 1)) +
  theme(text = element_text(size = 15)) +
  labs(color = "Origin", shape = "Rainfall", linetype = NULL) +
  #facet_wrap(~treatment) +
  theme(legend.position="bottom")

ggsave(paste0(fig_loc, "ESA_ndiff_propfeas_trt.png"), width = 6.1, height = 4.5)

ggplot(allcomm, aes(x=fdiv, y=mean_fitness, color = origin, shape = treatment))+
  geom_point(size = 3) +
  geom_smooth(method = "lm", alpha = 0.15) +
  scale_color_manual(values = c("#5D69B1", "#fab14f")) +
  xlab("Functional Diversity")  +
  ylab("Mean Community Fitness Differences") +
  
  scale_shape_manual(values = c(16, 1)) +
  theme(text = element_text(size = 15)) +
  labs(color = "Origin", shape = "Rainfall", linetype = NULL) +
  theme(legend.position="bottom")

ggsave(paste0(fig_loc, "ESA_fdiff_propfeas_trt.png"), width = 6.2, height = 4.5)


ggplot(allcomm, aes(x=mean_niche, y=mean_fitness, color = origin))+
  geom_point() +
  scale_color_manual(values = c("#fab14f","#5D69B1")) +
  xlab("Mean Community Niche Differences") +
  ylab("Mean Community Fitness Differences") +
  labs(color = NULL) +
  facet_wrap(~origin)

ggsave(paste0(fig_loc, "ESA_n_fdiff_origin.png"), width = 8, height = 4.5)


#"#E6A0C4" "#C6CDF7" "#D8A499" "#7294D4"

#E58606,#5D69B1,#52BCA3,#99C945,#CC61B0,#24796C,#DAA51B,#2F8AC4,#764E9F,#ED645A,#CC3A8E,#A5AA99
ggplot(allcomm, aes(x=fdiv, y=mean_niche, color = origin))+
  geom_point() +
  scale_color_manual(values = c("#E58606","#5D69B1")) +
  geom_smooth(method = "lm")+
  ylab("Mean Community Niche Differences") +
  xlab("Functional Diversity") +
  labs(color = NULL) #+
  #facet_wrap(~treatment)
ggsave(paste0(fig_loc, "fdiv_nichediff.png"), width = 6, height = 4)

ggplot(allcomm, aes(x=fdiv, y=mean_fitness, color = origin))+
  geom_point() +
  scale_color_manual(values = c("#E58606","#5D69B1")) +
  geom_smooth(method = "lm") +
  ylab("Mean Community Fitness Differences") +
  xlab("Functional Diversity") +
  labs(color = NULL) #+
  #facet_wrap(~treatment)

ggsave(paste0(fig_loc, "fdiv_fitness.png"), width = 6, height = 4)

## mixed communities ####
ggplot(mix_prop_feas, aes(x=as.factor(num.inv), y=prop_feasible)) +
  geom_jitter() +
  geom_boxplot()
  

ggplot(mix_prop_feas, aes(x=fdiv, y=prop_feasible))+
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~num.inv)

ggplot(mix_prop_feas, aes(x=fdiv, y=mean_niche))+
  geom_point() +
  geom_smooth(method = "lm")

ggplot(mix_prop_feas, aes(x=fdiv, y=mean_fitness))+
  geom_point() +
  geom_smooth(method = "lm")




