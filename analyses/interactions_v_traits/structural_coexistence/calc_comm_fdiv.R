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
source("analyses/interactions_v_traits/clean_structural_outputs.R")

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

## Invasive sp ####
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


## Native species ####
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

## Mixed Comms ####
## select distinct community compositions
mixcomms = allmix[,c(1:16, 21)] %>%
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
mix_fdiv = left_join(allmix, mixcomms[, 17:18], by = "comp")

names(mix_fdiv)
