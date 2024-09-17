## Comm weighted mean traits

source("analyses/traits/clean_trait_data.R")

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

all.sp = unique(trait_sum$phyto)

comp4 = data.frame(comboGeneral(all.sp, m=4, freqs = 1))

comp15 = data.frame(comboGeneral(all.sp, m=15, freqs = 1))

## empty df for output
#output = data.frame(cwm.height = NA, cwm.ldmc = NA, cwm.sla = NA, cwm.rmf = NA, cwm.crsl = NA, cwm.pf = NA, cwm.d = NA,
                #       ACAM = NA, AMME = NA, ANAR = NA, BRHO = NA, BRNI = NA, CESO = NA, GITR = NA, LENI = NA, LOMU = NA, MAEL = NA, MICA = NA, PLER = NA, PLNO = NA, TACA = NA, THIR = NA, TWIL = NA)

for(j in 1:nrow(comp4)){

  cc = as.character(comp4[j,])

  cwm = trait_sum %>%
    filter(phyto %in% cc) %>%
    summarise(cwm.height = mean(m.height),
              cwm.ldmc = mean(m.ldmc),
              cwm.sla = mean(m.sla),
              cwm.rmf = mean(m.rmf),
              cwm.crsl = mean(m.crsl),
              cwm.pf = mean(m.pf),
              cwm.d = mean(m.d)) %>%
    mutate(ACAM = ifelse("ACAM" %in% cc, 1, 0), ## change to 1's and 0's for P/A
         AMME = ifelse("AMME" %in% cc, 1, 0),
         ANAR = ifelse("ANAR" %in% cc, 1, 0),
         BRHO = ifelse("BRHO" %in% cc, 1, 0),
         BRNI = ifelse("BRNI" %in% cc, 1, 0),
         CESO = ifelse("CESO" %in% cc, 1, 0),
         GITR = ifelse("GITR" %in% cc, 1, 0),
         LENI = ifelse("LENI" %in% cc, 1, 0),
         LOMU = ifelse("LOMU" %in% cc, 1, 0),
         MAEL = ifelse("MAEL" %in% cc, 1, 0),
         MICA = ifelse("MICA" %in% cc, 1, 0),
         PLER = ifelse("PLER" %in% cc, 1, 0),
         PLNO = ifelse("PLNO" %in% cc, 1, 0),
         TACA = ifelse("TACA" %in% cc, 1, 0),
         THIR = ifelse("THIR" %in% cc, 1, 0),
         TWIL = ifelse("TWIL" %in% cc, 1, 0))
  
  write.table(cwm, "analyses/interactions_v_traits/structural_coexistence/calc_comm_attributes/4_sp_cwm_20240916.csv", append = TRUE, row.names = FALSE, sep = ",", col.names = FALSE)
  
}



         