
# Set up ####
library(tidyverse)
library(mFD)
library(ggpubr)

library(wesanderson)

wes_palettes

fig_loc = "analyses/interactions_v_traits/structural_coexistence/prelim_figs/"
theme_set(theme_classic())

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

source("analyses/traits/clean_trait_data.R")


# Read in data ####
## native ####
natcommD = read.csv("analyses/interactions_v_traits/structural_coexistence/run_structural/nat_only_D_structural_results_20240729.csv")

natcommC = read.csv("analyses/interactions_v_traits/structural_coexistence/run_structural/nat_only_C_structural_results_20240729.csv")

## invasive ####
invcommD = read.csv("analyses/interactions_v_traits/structural_coexistence/run_structural/inv_only_D_structural_results_20240730.csv")

invcommC = read.csv("analyses/interactions_v_traits/structural_coexistence/run_structural/inv_only_C_structural_results_20240730.csv")

# Clean data ####
## native ####
natcommD_vis = natcommD %>%
  filter(!is.na(GITR), MAEL == 0)%>%
  mutate(comp = paste0(ACAM, AMME, GITR, LENI, MAEL, MICA, PLER, PLNO, TWIL),
         treatment = "D")

natcommC_vis = natcommC %>%
  filter(!is.na(GITR), MAEL == 0)%>%
  mutate(comp = paste0(ACAM, AMME, GITR, LENI, MAEL, MICA, PLER, PLNO, TWIL),
         treatment = "C")

## join together
allnat = rbind(natcommC_vis, natcommD_vis) %>%
  select(-X)

## invasive ####
invcommD_vis = invcommD %>%
  filter(!is.na(ANAR))%>%
  mutate(comp = paste0(ANAR, BRHO, BRNI, CESO, LOMU, TACA, THIR),
         treatment = "D")

invcommC_vis = invcommC %>%
  filter(!is.na(BRHO))%>%
  mutate(comp = paste0(ANAR, BRHO, BRNI, CESO, LOMU, TACA, THIR),
         treatment = "C")

## join together
allinv = rbind(invcommC_vis, invcommD_vis) %>%
  select(-X)


## following instructions from this https://cmlmagneville.github.io/mFD/articles/Continuous_traits_framework.html#load-dataset to calculate functional diversity. 
## associated with paper: Magneville et al. 2022; Ecography

## summarise trait & assemblage data
## calculate trait based distances b/w species pairs
## compute multidimensional functional space

## data taken from trait pcas exploration script
# Clean Trait Data ####
## select traits
all.traits <- c("Height_cm", "LDMC", "SLA.cm2.g", "RMF",  "Coarse.root.specific.length.cm.g", "Proportion.fine.roots", "Coarse.root.diameter.mm")

traits <- MC.traits2 %>%
  select(phyto, fg_origin, funct_group, Height_cm, 
         LDMC, SLA.cm2.g, RMF, Coarse.root.specific.length.cm.g, Proportion.fine.roots, Coarse.root.diameter.mm)

names(traits) <- c("phyto", "fg_origin", "fg", "Height", 
                   "LDMC", "SLA", "RMF", "CRSL", "PF", "D")

## summarise to prepare matrix of traits
trait_sum = traits %>%
  group_by(phyto) %>%
  summarise(m.height = mean(Height),
         m.ldmc = mean(LDMC), 
         m.sla = mean(SLA),
         m.rmf = mean(RMF), 
         m.crsl = mean(CRSL),
         m.pf = mean(PF),
         m.d = mean(D)) 

## create matrix of traits with species as row-names
#sp = sort(unique(trait_sum$phyto))
#trait_sum = trait_sum[,-1]

test = data.frame(m.height = as.numeric(trait_sum$m.height),
           m.ldmc = as.numeric(trait_sum$m.ldmc),
           m.sla = as.numeric(trait_sum$m.sla),
           m.rmf = as.numeric(trait_sum$m.rmf),
           m.crsl = as.numeric(trait_sum$m.crsl),
           m.pf = as.numeric(trait_sum$m.pf),
           m.d = as.numeric(trait_sum$m.d))
rownames(test) = trait_sum$phyto

## create dataframe of trait types, that is needed for functions in mFD package
trait_mdat = data.frame(trait_name = c("m.height", "m.ldmc", "m.sla", "m.rmf", "m.crsl", "m.pf", "m.d"),
           trait_type = rep("Q", 7))

# Calc FD ####
## calculate functional distance between pairs of species
sp_dist = funct.dist(sp_tr = test,
                     tr_cat = trait_mdat,
                     metric = "euclidean")

## calculate functional space - not quite sure what this step is?
fspace = tr.cont.fspace(sp_tr = test,
               pca = TRUE,
               nb_dim = 7)

## explore quality metrics
fspace$"quality_metrics"
fspace$"eigenvalues_percentage_var"

## extract distance matrices for both PCA data & raw trait data
dist_mat_pca <- as.matrix(fspace$sp_dist_multidim$"6D")
dist_mat_trait <- as.matrix(fspace$sp_dist_init)


#alpha.fd.multidim(
 # sp_faxes_coord = , ## not sure what to put in for this argument
#)

## Bring in comm data ####
### invasive sp ####
## select distinct community compositions
invcomms = invcommC_vis[,c(2:8, 13)] %>%
  distinct()

## get names of compositions to save as rownames
cn = invcomms$comp

## create a matrix of P/A data with rownames of diff compositions
invcmat = as.matrix(invcomms[1:7])
rownames(invcmat) = cn

## run final functional diversity calculation for each community
inv_fdiv = alpha.fd.hill(
  asb_sp_w = invcmat,
  sp_dist = dist_mat_trait,
  q=0, ## defines relative importance given to species weights compared to species distances; increasing q gives increasing importance to species weights rather than trait-based distances
  tau = "mean"  ## defines threshold level applied to functional distances b/w species to determine functionally distinct sets of species
  ## minimum would yield taxonomic div; maximum & q=2 would yield Rao's Q
)

## extract functional div metric
invcommfdiv = inv_fdiv$asb_FD_Hill

## view
hist(invcommfdiv)

## save into data frame
invcomms$fdiv = invcommfdiv

## join with structural output
allinv_fdiv = left_join(allinv, invcomms[, 8:9], by = "comp")

names(allinv_fdiv)

inv_prop_feas = allinv_fdiv %>%
  group_by(comp, treatment, fdiv, CESO, BRNI, TACA, LOMU, THIR, ANAR, BRHO) %>%
  summarise(num_feas = sum(feasibility),
            prop_feasible = num_feas/n(),
            mean_niche = mean(niche_diff),
            mean_fitness = mean(fitness_diff),
            se_niche = calcSE(niche_diff),
            se_fitness = calcSE(fitness_diff)) %>%
  mutate(origin = "invasive")

#### plot ####
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


### native sp ####
ncomms = natcommC_vis[,c(2:10, 15)] %>%
  distinct()

cn = ncomms$comp

ncmat = as.matrix(ncomms[1:9])
rownames(ncmat) = cn

nat_fdiv = alpha.fd.hill(
  asb_sp_w = ncmat,
  sp_dist = dist_mat_trait,
  q=0,
  tau = "mean"
)

ncommfdiv = nat_fdiv$asb_FD_Hill

hist(ncommfdiv)

ncomms$fdiv = ncommfdiv

allnat_fdiv = left_join(allnat, ncomms[, 10:11], by = "comp")

names(allnat_fdiv)

nat_prop_feas = allnat_fdiv %>%
  filter(!is.na(feasibility)) %>%
  group_by(comp, treatment, fdiv) %>%
  summarise(num_feas = sum(feasibility),
            prop_feasible = num_feas/n(),
            mean_niche = mean(niche_diff),
            mean_fitness = mean(fitness_diff),
            se_niche = calcSE(niche_diff),
            se_fitness = calcSE(fitness_diff)) %>%
  mutate(origin = "native")

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

## join together ####
allcomm = rbind(nat_prop_feas, inv_prop_feas) %>%
  mutate(origin = ifelse(origin == "native", "Native", "Non-Native"),
         treatment = ifelse(treatment == "C", "Ambient", "Drought"))

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


ggplot(allcomm, aes(x=mean_niche, y=mean_fitness, color = origin))+
  geom_point() +
  scale_color_manual(values = c("#fab14f","#5D69B1")) +
  #geom_smooth(method = "lm")+
  xlab("Mean Community Niche Differences") +
  ylab("Mean Community Fitness Differences") +
  #xlab("Functional Diversity") +
  labs(color = NULL) +
  facet_wrap(~origin)
  

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
mixcommC_vis = mixcommC %>%
  filter(!is.na(GITR))%>%
  mutate(comp = paste0(ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL),
         treatment = "C")

## select distinct community compositions
mixcomms = mixcommC_vis[,c(1:16, 21)] %>%
  distinct()

## get names of compositions to save as rownames
mix_cn = mixcomms$comp

## create a matrix of P/A data with rownames of diff compositions
mix_cmat = as.matrix(mixcomms[1:16])
rownames(mix_cmat) = mix_cn

## run final functional diversity calculation for each community
mix_fdiv = alpha.fd.hill(
  asb_sp_w = mix_cmat,
  sp_dist = dist_mat_trait,
  q=0, ## defines relative importance given to species weights compared to species distances; increasing q gives increasing importance to species weights rather than trait-based distances
  tau = "mean"  ## defines threshold level applied to functional distances b/w species to determine functionally distinct sets of species
  ## minimum would yield taxonomic div; maximum & q=2 would yield Rao's Q
)

## extract functional div metric
mixcommfdiv = mix_fdiv$asb_FD_Hill

## view
hist(mixcommfdiv)

## save into data frame
mixcomms$fdiv = mixcommfdiv

## join with structural output
mix_fdiv = left_join(mixcommC_vis, mixcomms[, 17:18], by = "comp")

names(mix_fdiv)

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
  filter(num.inv < 4)

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




