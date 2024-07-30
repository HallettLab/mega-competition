
library(mFD)

## summarise trait & assemblage data
## calculate trait based distances b/w species pairs
## compute multidimensional functional space

MC.traits

all.traits <- c("Height.cm", "LDMC", "SLA.cm2.g", "RMF",  "Coarse.root.specific.length.cm.g", "Proportion.fine.roots", "Coarse.root.diameter.mm")

traits <- MC.traits %>%
  select(phyto, fg_origin, funct_group, Height.cm, LDMC, SLA.cm2.g, RMF, Coarse.root.specific.length.cm.g, Proportion.fine.roots, Coarse.root.diameter.mm)

names(traits) <- c("phyto", "fg_origin", "fg", "Height", "LDMC", "SLA", "RMF", "CRSL", "PF", "D")

trait_sum = traits %>%
  group_by(phyto) %>%
  summarise(m.height = mean(Height),
         m.ldmc = mean(LDMC), 
         m.sla = mean(SLA),
         m.rmf = mean(RMF), 
         m.crsl = mean(CRSL),
         m.pf = mean(PF),
         m.d = mean(D)) #%>%
  #select(-phyto)
sp = unique(trait_sum$phyto)
trait_sum = trait_sum[,-1]
rownames(trait_sum) = sp

trait_mdat = data.frame(trait_name = c("m.height", "m.ldmc", "m.sla", "m.rmf", "m.crsl", "m.pf", "m.d"),
           trait_type = rep("Q", 7)
           )


sp.tr.summary()


sp_dist = funct.dist(sp_tr = trait_sum,
                     tr_cat = trait_mdat,
                     metric = "euclidean"
                     )

fspace = tr.cont.fspace(sp_tr = trait_sum,
               pca = TRUE,
               nb_dim = 7)

fspace$"quality_metrics"
fspace$"eigenvalues_percentage_var"
dist_mat_pca <- as.matrix(fspace$sp_dist_multidim$"6D")
dist_mat_trait <- as.matrix(fspace$sp_dist_init)

alpha.fd.multidim(
  sp_faxes_coord = ,
  
)

quality.fspaces()

comms = invcommC_vis[,c(1:7, 12)] %>%
  distinct()

cn = comms$comp

cmat = as.matrix(comms[1:7])
rownames(cmat) = cn

test = alpha.fd.hill(
  asb_sp_w = cmat,
  sp_dist = dist_mat_trait,
  q=0,
  tau = "mean"
)

commfdiv = test$asb_FD_Hill

hist(commfdiv)

comms$fdiv = commfdiv

allinv_fdiv = left_join(allinv, comms[, 8:9], by = "comp")

names(allinv_fdiv)

prop_feas = allinv_fdiv %>%
  group_by(comp, treatment, fdiv) %>%
  summarise(num_feas = sum(feasibility),
            prop_feasible = num_feas/n(),
            mean_niche = mean(niche_diff),
            mean_fitness = mean(fitness_diff),
            se_niche = calcSE(niche_diff),
            se_fitness = calcSE(fitness_diff)) %>%
  mutate(w_legume = ifelse(substr(comp, start = 7, stop = 7) == 1, 1, 0))

ggplot(prop_feas, aes(x=fdiv, y=prop_feasible))+
  geom_point()

ggplot(prop_feas, aes(x=fdiv, y=mean_niche))+
  geom_point() +
  geom_smooth(method = "lm")

ggplot(prop_feas, aes(x=fdiv, y=mean_fitness))+
  geom_point() +
  geom_smooth(method = "lm")


# Native Sp ####
ncomms = natcommC_vis[,c(2:10, 15)] %>%
  distinct()

cn = ncomms$comp

ncmat = as.matrix(ncomms[1:9])
rownames(ncmat) = cn

test = alpha.fd.hill(
  asb_sp_w = ncmat,
  sp_dist = dist_mat_trait,
  q=0,
  tau = "mean"
)

ncommfdiv = test$asb_FD_Hill

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
            se_fitness = calcSE(fitness_diff))

ggplot(nat_prop_feas, aes(x=fdiv, y=prop_feasible))+
  geom_point()+
  geom_smooth(method = "lm")
ggsave("")

ggplot(prop_feas, aes(x=fdiv, y=mean_niche))+
  geom_point() +
  geom_smooth(method = "lm")

ggplot(prop_feas, aes(x=fdiv, y=mean_fitness))+
  geom_point() +
  geom_smooth(method = "lm")

