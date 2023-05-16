#source("data_cleaning/format_model_dat.R")
source("data_cleaning/phyto-processing_data-cleaning/compile_phyto-processing_data.R")

## per capita pop growth figure
## need to add control points into every dens gradient here

unique.key <- unique.key %>%
  mutate(bkgrd = ifelse(bkgrd == "THIR-I", "THIR", bkgrd), 
         bkgrd = ifelse(bkgrd == "TWIL-I", "TWIL", bkgrd), 
         phyto = ifelse(phyto == "THIR-I", "THIR", phyto),
         phyto = ifelse(phyto == "TWIL-I", "TWIL", phyto))


all.phytos.info <- left_join(all.phytos, unique.key, by = c("unique.ID", "phyto")) %>%
  mutate(bkgrd = ifelse(bkgrd == "ERBO", "Control", bkgrd))
  ## set ERBO backgrounds as controls


## we should change the value in dens so that we know what the controls are - perhaps dens should be 'zero, low, high'


test <- all.phytos.info %>%
  filter(bkgrd == "Control", block == 14)


## need to incorporate control backgrounds
## for each sp in each block, copy the control line

blocks <- unique(all.phytos.info$block)
species <- unique(all.phytos.info$phyto)
bkgrds <- unique(all.phytos.info$bkgrd)

bkgrd.df <- as.data.frame(bkgrds[bkgrds != "Control"])
colnames(bkgrd.df) <- "bkgrd"

## store outputs
repeated.controls <- data.frame()

for (i in 1:length(species)) {
  
  tmp.sp <- species[i] ## select phyto species
  
  all.blocks <- data.frame() ## make input dataframe
  
  for (j in 1:length(blocks)) {
    
    tmp.block <- blocks[j] ## select the block
    
    tmp.controls <- all.phytos.info %>%
      filter(phyto == tmp.sp,
             bkgrd == "Control",
             block == tmp.block)
    
    
    if (nrow(tmp.controls) > 0 ) {
    
    control.reps <- unique(tmp.controls$unique.ID) ## make vector of each rep in the block
    
    all.reps <- data.frame()
      
    for (k in 1:length(control.reps)) {
      
      tmp.rep <- control.reps[k] ## select rep
      
      tmp.repeated.reps <- tmp.controls %>%
        filter(unique.ID == tmp.rep) %>%
        slice(rep(1:n(), each = 18)) %>% ## repeat each row 18 times
        select(-bkgrd) %>% ## get rid of old bg column
        mutate(bkgrd = bkgrd.df[,1],
               dens = "none") ## fill out density column as 'none'
      
      all.reps <- rbind(all.reps, tmp.repeated.reps)
    }
    
    } else {
    
      all.reps <- data.frame()
      
    }
       
    ## add this back in
    all.blocks <- rbind(all.blocks, all.reps)
    
  }
  
  repeated.controls <- rbind(repeated.controls, all.blocks)
}


ggplot(repeated.controls, aes(x=unique.ID)) +
  geom_histogram()

unique(repeated.controls$unique.ID)


## seeds out/seeds in
## linear mixed effects models depending on the rainfall treatment, background species, density




with.controls <- rbind(all.phytos.info, repeated.controls)

percap.growth <- with.controls %>%
  mutate(percap.growthrate = phyto.seed.out/phyto.seed.in) %>%
  filter(bkgrd != "Control") %>%
  mutate(dens = fct_relevel(dens, "none", "L", "H"), 
         dens.num = ifelse(dens == "none", 0, NA),
         dens.num = ifelse(dens == "L", 4, dens.num),
         dens.num = ifelse(dens == "H", 8, dens.num)) #reorder dens treatments

ggplot(percap.growth, aes(x=dens, y=dens.num)) +
  geom_point()




ggplot(percap.growth[percap.growth$phyto == "ACAM",], aes(x=dens.num, y=percap.growthrate, color = treatment)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point() +
  facet_wrap(~bkgrd, scales = "free") +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("#003366", "#FFA630")) +
  ggtitle("ACAM")

#ggsave("models/raw_percap_GR_figures/ACAM_GR.png", width = 9, height = 7)

ggplot(percap.growth[percap.growth$phyto == "ANAR",], aes(x=dens.num, y=percap.growthrate, color = treatment)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point() +
  facet_wrap(~bkgrd, scales = "free") +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("#003366", "#FFA630")) +
  ggtitle("ANAR")
#ggsave("models/raw_percap_GR_figures/ANAR_GR.png", width = 9, height = 7)

ggplot(percap.growth[percap.growth$phyto == "AMME",], aes(x=dens.num, y=percap.growthrate, color = treatment)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point() +
  facet_wrap(~bkgrd, scales = "free") +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("#003366", "#FFA630")) +
  ggtitle("AMME")

#ggsave("models/raw_percap_GR_figures/AMME_GR.png", width = 9, height = 7)

ggplot(percap.growth[percap.growth$phyto == "BRHO",], aes(x=dens.num, y=percap.growthrate, color = treatment)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point() +
  facet_wrap(~bkgrd, scales = "free") +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("#003366", "#FFA630")) +
  ggtitle("BRHO")
#ggsave("models/raw_percap_GR_figures/BRHO_GR.png", width = 9, height = 7)

ggplot(percap.growth[percap.growth$phyto == "CESO",], aes(x=dens.num, y=percap.growthrate, color = treatment)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point() +
  facet_wrap(~bkgrd, scales = "free") +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("#003366", "#FFA630")) +
  ggtitle("CESO")

#ggsave("models/raw_percap_GR_figures/CESO_GR.png", width = 9, height = 7)

ggplot(percap.growth[percap.growth$phyto == "CLPU",], aes(x=dens.num, y=percap.growthrate, color = treatment)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point() +
  facet_wrap(~bkgrd, scales = "free") +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("#003366", "#FFA630")) +
  ggtitle("CLPU")

#ggsave("models/raw_percap_GR_figures/CLPU_GR.png", width = 9, height = 7)


ggplot(percap.growth[percap.growth$phyto == "GITR",], aes(x=dens.num, y=percap.growthrate, color = treatment)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point() +
  facet_wrap(~bkgrd, scales = "free") +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("#003366", "#FFA630")) +
  ggtitle("GITR")

#ggsave("models/raw_percap_GR_figures/GITR_GR.png", width = 9, height = 7)

ggplot(percap.growth[percap.growth$phyto == "LOMU",], aes(x=dens.num, y=percap.growthrate, color = treatment)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point() +
  facet_wrap(~bkgrd, scales = "free") +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("#003366", "#FFA630")) +
  ggtitle("LOMU")

#ggsave("models/raw_percap_GR_figures/LOMU_GR.png", width = 9, height = 7)

ggplot(percap.growth[percap.growth$phyto == "MAEL",], aes(x=dens.num, y=percap.growthrate, color = treatment)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point() +
  facet_wrap(~bkgrd, scales = "free") +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("#003366", "#FFA630")) +
  ggtitle("MAEL")

#ggsave("models/raw_percap_GR_figures/MAEL_GR.png", width = 9, height = 7)

ggplot(percap.growth[percap.growth$phyto == "MICA",], aes(x=dens.num, y=percap.growthrate, color = treatment)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point() +
  facet_wrap(~bkgrd, scales = "free") +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("#003366", "#FFA630")) +
  ggtitle("MICA")

#ggsave("models/raw_percap_GR_figures/MICA_GR.png", width = 9, height = 7)

ggplot(percap.growth[percap.growth$phyto == "PLER",], aes(x=dens.num, y=percap.growthrate, color = treatment)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point() +
  facet_wrap(~bkgrd, scales = "free") +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("#003366", "#FFA630")) +
  ggtitle("PLER")

#ggsave("models/raw_percap_GR_figures/PLER_GR.png", width = 9, height = 7)

ggplot(percap.growth[percap.growth$phyto == "PLNO",], aes(x=dens.num, y=percap.growthrate, color = treatment)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point() +
  facet_wrap(~bkgrd, scales = "free") +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("#003366", "#FFA630")) +
  ggtitle("PLNO")

#ggsave("models/raw_percap_GR_figures/PLNO_GR.png", width = 9, height = 7)

ggplot(percap.growth[percap.growth$phyto == "TACA",], aes(x=dens.num, y=percap.growthrate, color = treatment)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point() +
  facet_wrap(~bkgrd, scales = "free") +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("#003366", "#FFA630")) +
  ggtitle("TACA")

#ggsave("models/raw_percap_GR_figures/TACA_GR.png", width = 9, height = 7)

ggplot(percap.growth[percap.growth$phyto == "THIR",], aes(x=dens.num, y=percap.growthrate, color = treatment)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point() +
  facet_wrap(~bkgrd, scales = "free") +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("#003366", "#FFA630")) +
  ggtitle("THIR")

#ggsave("models/raw_percap_GR_figures/THIR_GR.png", width = 9, height = 7)

ggplot(percap.growth[percap.growth$phyto == "TWIL",], aes(x=dens.num, y=percap.growthrate, color = treatment)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point() +
  facet_wrap(~bkgrd, scales = "free") +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("#003366", "#FFA630")) +
  ggtitle("TWIL")

#ggsave("models/raw_percap_GR_figures/TWIL_GR.png", width = 9, height = 7)

rm(list = c("all.blocks","all.phytos","all.phytos.info","all.reps", "bkgrd.df", "bkgrds", "blocks", "control.reps", "i", "j", "k", "repeated.controls"))
