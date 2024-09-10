## Calc community functional trait diversity
## for all possible community compositions at richness levels 4-16

# Set up ####
library(tidyverse)
library(mFD)
library(RcppAlgos)

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

# Read in Data ####
source("analyses/traits/clean_trait_data.R")
#source("analyses/interactions_v_traits/structural_coexistence/clean_structural_outputs.R")

## following instructions from this https://cmlmagneville.github.io/mFD/articles/Continuous_traits_framework.html#load-dataset to calculate functional diversity. 
## associated with paper: Magneville et al. 2022; Ecography

# Prep Trait Data ####
## select traits
all.traits <- c("Height_cm", "LDMC", "SLA.cm2.g", "RMF",  "Coarse.root.specific.length.cm.g", "Proportion.fine.roots", "Coarse.root.diameter.mm")

traits <- MC.traits2 %>%
  select(phyto, fg_origin, funct_group, Height_cm, 
         LDMC, SLA.cm2.g, RMF, Coarse.root.specific.length.cm.g, Proportion.fine.roots, Coarse.root.diameter.mm)

## rename columns
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
t_mat = data.frame(m.height = as.numeric(trait_sum$m.height),
                  m.ldmc = as.numeric(trait_sum$m.ldmc),
                  m.sla = as.numeric(trait_sum$m.sla),
                  m.rmf = as.numeric(trait_sum$m.rmf),
                  m.crsl = as.numeric(trait_sum$m.crsl),
                  m.pf = as.numeric(trait_sum$m.pf),
                  m.d = as.numeric(trait_sum$m.d))
rownames(t_mat) = trait_sum$phyto

## create dataframe of trait types, that is needed for functions in mFD package
trait_mdat = data.frame(trait_name = c("m.height", "m.ldmc", "m.sla", "m.rmf", "m.crsl", "m.pf", "m.d"),
                        trait_type = rep("Q", 7))

# Calc FD ####
## calculate functional distance between pairs of species
sp_dist = funct.dist(sp_tr = t_mat,
                     tr_cat = trait_mdat,
                     metric = "euclidean")

## calculate functional space - not quite sure what this step is?
fspace = tr.cont.fspace(sp_tr = t_mat,
                        pca = TRUE,
                        nb_dim = 7)

## explore quality metrics
fspace$"quality_metrics"
fspace$"eigenvalues_percentage_var"

## extract distance matrices for both PCA data & raw trait data
dist_mat_pca <- as.matrix(fspace$sp_dist_multidim$"6D")
dist_mat_trait <- as.matrix(fspace$sp_dist_init)

## Select community comps ####
## create a vector of all species
all.sp = unique(MC.pca.ID$phyto)

## create a vector of all richness levels
richness = c(4:16)

## create an empty array? 
comp_lists = list()

## loop through all richness levels & create df with all combinations of possible community compositions
for(r in 1:length(richness)) {
  
  ## select a richness level
  rich = richness[r]
  
  ## calculate all combos of composition
  comp <- data.frame(comboGeneral(all.sp, m=rich, freqs = 1)) %>% 
    rowwise() %>%
    mutate(comp_list = list(c_across(starts_with("X")))) %>%
    mutate(ACAM = ifelse("ACAM" %in% comp_list, 1, 0), ## change to 1's and 0's for P/A
           AMME = ifelse("AMME" %in% comp_list, 1, 0),
           ANAR = ifelse("ANAR" %in% comp_list, 1, 0),
           BRHO = ifelse("BRHO" %in% comp_list, 1, 0),
           BRNI = ifelse("BRNI" %in% comp_list, 1, 0),
           CESO = ifelse("CESO" %in% comp_list, 1, 0),
           GITR = ifelse("GITR" %in% comp_list, 1, 0),
           LENI = ifelse("LENI" %in% comp_list, 1, 0),
           LOMU = ifelse("LOMU" %in% comp_list, 1, 0),
           MAEL = ifelse("MAEL" %in% comp_list, 1, 0),
           MICA = ifelse("MICA" %in% comp_list, 1, 0),
           PLER = ifelse("PLER" %in% comp_list, 1, 0),
           PLNO = ifelse("PLNO" %in% comp_list, 1, 0),
           TACA = ifelse("TACA" %in% comp_list, 1, 0),
           THIR = ifelse("THIR" %in% comp_list, 1, 0),
           TWIL = ifelse("TWIL" %in% comp_list, 1, 0),
           comp = paste0(ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL),
           richness = rich) %>%
    select(ACAM, AMME, ANAR, BRHO, BRNI, CESO, GITR, LENI, LOMU, MAEL, MICA, PLER, PLNO, TACA, THIR, TWIL, comp, richness)
  
  ## append to list
  comp_lists[[r]] = comp
  
}

## Create Matrix & Calc ####
## create empty list
fdiv_list = list()

## iterate thru each richness level & calc FD
for(r in 1:length(richness)) {
  
  ## select a richness level
  rich = richness[r]
  
  ## get names of compositions to save as rownames
  cn = comp_lists[[r]]$comp
  
  ## create a matrix of P/A data with rownames of diff compositions
  commsmat = as.matrix(comp_lists[[r]][1:16])
  
  ## save rownames
  rownames(commsmat) = cn
  
  ## run final functional diversity calculation for each community
  comms_fdiv = alpha.fd.hill(
    asb_sp_w = commsmat,
    sp_dist = dist_mat_trait,
    q=0, ## defines relative importance given to species weights compared to species distances; increasing q gives increasing importance to species weights rather than trait-based distances
    tau = "mean"  ## defines threshold level applied to functional distances b/w species to determine functionally distinct sets of species
    ## minimum would yield taxonomic div; maximum & q=2 would yield Rao's Q
  )
  
  ## extract functional div metric
  fdiv = comms_fdiv$asb_FD_Hill

  ## save into data frame
  t_comms = as.data.frame(comp_lists[[r]])
  t_comms$fdiv = fdiv

  ## append to list
  fdiv_list[[r]] = t_comms
  
}

# Clean Env ####
rm(comms_fdiv, commsmat, dist_mat_pca, dist_mat_trait, fdiv, fspace, MC.pca.ID, MC.traits2, seed.sums, trait_mdat, trait_sum, trait_sums, traits, all.traits, cn, sp_dist)

# Save Outputs to CSV ####
path = "analyses/interactions_v_traits/structural_coexistence/calc_comm_attributes/"

write.csv(fdiv_list[[1]], paste0(path, "sp4_fdiv.csv"))
write.csv(fdiv_list[[2]], paste0(path, "sp5_fdiv.csv"))
write.csv(fdiv_list[[3]], paste0(path, "sp6_fdiv.csv"))
write.csv(fdiv_list[[4]], paste0(path, "sp7_fdiv.csv"))
write.csv(fdiv_list[[5]], paste0(path, "sp8_fdiv.csv"))
write.csv(fdiv_list[[6]], paste0(path, "sp9_fdiv.csv"))
write.csv(fdiv_list[[7]], paste0(path, "sp10_fdiv.csv"))
write.csv(fdiv_list[[8]], paste0(path, "sp11_fdiv.csv"))
write.csv(fdiv_list[[9]], paste0(path, "sp12_fdiv.csv"))
write.csv(fdiv_list[[10]], paste0(path, "sp13_fdiv.csv"))
write.csv(fdiv_list[[11]], paste0(path, "sp14_fdiv.csv"))
write.csv(fdiv_list[[12]], paste0(path, "sp15_fdiv.csv"))
write.csv(fdiv_list[[13]], paste0(path, "sp16_fdiv.csv"))
