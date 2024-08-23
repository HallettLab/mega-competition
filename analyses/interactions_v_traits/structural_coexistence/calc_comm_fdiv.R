## Calc community functional trait diversity

# Set up ####
library(tidyverse)
library(mFD)

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

# Read in Data ####
source("analyses/traits/clean_trait_data.R")
source("analyses/interactions_v_traits/structural_coexistence/clean_structural_outputs.R")

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

## select distinct community compositions
comms = allcomm[,c(1:16, 24)] %>%
  distinct()

## get names of compositions to save as rownames
cn = comms$comp

## create a matrix of P/A data with rownames of diff compositions
commsmat = as.matrix(comms[1:16])
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

## view
hist(fdiv)

## save into data frame
comms$fdiv = fdiv

## join with structural output
allcomm_fdiv = left_join(allcomm, comms[, 17:18], by = "comp")

# Clean Env ####
rm(allcomm, comms, comms_fdiv, commsmat, dist_mat_pca, dist_mat_trait, fdiv, fspace, MC.pca.ID, MC.traits2, seed.sums, test, trait_mdat, trait_sum, trait_sums, traits, all.traits, cn, sp_dist)
