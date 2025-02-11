## Comm weighted mean traits

# Set up ####
library(RcppAlgos)

## load trait data
source("analyses/traits/clean_trait_data.R")

path = "analyses/interactions_v_traits/structural_coexistence/calc_comm_attributes/cwm/"

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

# Calc CWMs ####
## create vector of species
all.sp = unique(trait_sum$phyto)

## create vector of richness levels
#richness = c(4:16)

richness = 6

## create empty list
cwm_list = list()

## iterate thru each richness level & calc trait cwm
for(r in 1:length(richness)) {
  
  ## select a richness level
  rich = richness[r]
 
  ## get composition df
  comp = data.frame(comboGeneral(all.sp, m=rich, freqs = 1))

  ## create empty output df
  cwm_all = data.frame(matrix(nrow = nrow(comp), ncol = 31))
  
  ## rename columns
  names(cwm_all) = c("cwm.height", "cwm.ldmc", "cwm.sla", "cwm.rmf", "cwm.crsl", "cwm.pf", "cwm.d", "cv.height", "cv.ldmc", "cv.sla", "cv.rmf", "cv.crsl", "cv.pf", "cv.d", "ACAM", "AMME", "ANAR", "BRHO", "BRNI", "CESO", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL", "richness")
  
  ## loop thru each composition
  for(c in 1:nrow(comp)) {
    
    ## select a composition
    cc = as.character(comp[c,])
    
    ## calc cwm traits
    cwm = trait_sum %>%
      filter(phyto %in% cc) %>%
      
      ## mean traits
      summarise(cwm.height = mean(m.height),
                cwm.ldmc = mean(m.ldmc),
                cwm.sla = mean(m.sla),
                cwm.rmf = mean(m.rmf),
                cwm.crsl = mean(m.crsl),
                cwm.pf = mean(m.pf),
                cwm.d = mean(m.d),
                
                ## cv of traits
                cv.height = sd(m.height)/mean(m.height),
                cv.ldmc = sd(m.ldmc)/mean(m.ldmc),
                cv.sla = sd(m.sla)/mean(m.sla),
                cv.rmf = sd(m.rmf)/mean(m.rmf),
                cv.crsl = sd(m.crsl)/mean(m.crsl),
                cv.pf = sd(m.pf)/mean(m.pf),
                cv.d = sd(m.d)/mean(m.d)) %>%
      
      ## change sp comp to 1's and 0's for P/A
      mutate(ACAM = ifelse("ACAM" %in% cc, 1, 0), 
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
             TWIL = ifelse("TWIL" %in% cc, 1, 0),
             richness = rich)

    ## append to df in a set row
    cwm_all[c,] = cwm
  
  }
  
  ## append to list
  cwm_list[[r]] = cwm_all
  
}


# Save Outputs to CSV ####
path = "analyses/interactions_v_traits/structural_coexistence/calc_comm_attributes/cwm/"

write.csv(cwm_list[[1]], paste0(path, "sp4_cwm.csv"), row.names = FALSE)
write.csv(cwm_list[[2]], paste0(path, "sp5_cwm.csv"), row.names = FALSE)
#write.csv(cwm_list[[1]], paste0(path, "sp6_cwm_cv.csv"), row.names = FALSE)
write.csv(cwm_list[[4]], paste0(path, "sp7_cwm.csv"), row.names = FALSE)
write.csv(cwm_list[[5]], paste0(path, "sp8_cwm.csv"), row.names = FALSE)
write.csv(cwm_list[[6]], paste0(path, "sp9_cwm.csv"), row.names = FALSE)
write.csv(cwm_list[[7]], paste0(path, "sp10_cwm.csv"), row.names = FALSE)
write.csv(cwm_list[[8]], paste0(path, "sp11_cwm.csv"), row.names = FALSE)
write.csv(cwm_list[[9]], paste0(path, "sp12_cwm.csv"), row.names = FALSE)
write.csv(cwm_list[[10]], paste0(path, "sp13_cwm.csv"), row.names = FALSE)
write.csv(cwm_list[[11]], paste0(path, "sp14_cwm.csv"), row.names = FALSE)
write.csv(cwm_list[[12]], paste0(path, "sp15_cwm.csv"), row.names = FALSE)
write.csv(cwm_list[[13]], paste0(path, "sp16_cwm.csv"), row.names = FALSE)
