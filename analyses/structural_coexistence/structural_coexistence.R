

## load structural coexistence functions from Saavedra 2017
source("models/CW/structural_coexistence/Saavedra_2017_code/toolbox_coexistence.R")
source("models/CW/structural_coexistence/Saavedra_2017_code/toolbox_figure.R")

## load model outputs
source("models/CW/CW_import_posteriors.R")

## Mega-Comp Feasibility
## The loop calculating structural coexistence for every community combo is commented out as it takes ~2 hours to run. Only rerun when needed. The outputs are saved as a csv file that can be returned to later!

# Read in SC Data ####
#all_comm <- read.csv("models/CW/structural_coexistence_output.csv")

## filter out feasible communities
#f_comm <- all_comm %>%
  #filter(feasibility == 1)


# Prep DF ####

ok.combos <- unique(model.dat.filtered$combos)

## calc mean alpha values
mean_alphas <- posteriors_long %>%
  mutate(bg = toupper(substr(alpha_name, start = 7, stop = 11)),
         combos = paste(species, bg, sep = "_")) %>%
  select(-bg) %>%
  filter(combos %in% ok.combos) %>%
  group_by(treatment, species, alpha_name) %>%
  summarise(mean_alpha = mean(alpha_value, na.rm = T),
            lambda = mean(lambda, na.rm = T)) %>%
  pivot_wider(names_from = alpha_name, values_from = mean_alpha)


# Struct. Coexist Calc ####

## make a vector of all unique species
species <- unique(mean_alphas$species)

## change column names to make the for loop easier to write
colnames(mean_alphas) <- c("treatment", "species", "lambda", "ACAM", "AMME", "ANAR", "AVBA", "BRHO", "BRNI", "CESO", "CLPU", "GITR", "LENI", "LOMU", "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL")

## create empty dataframe
allcomm <- data.frame(ACAM = NA, AMME = NA, ANAR = NA, AVBA = NA, BRHO = NA, CESO=NA, GITR=NA, LOMU= NA, MAEL= NA, MICA= NA, PLER= NA, PLNO= NA, TACA= NA, THIR= NA, TWIL = NA, feasibility=NA, rich = NA, treatment = NA, niche_diff = NA, fitness_diff = NA, community = NA)

## create comm richness level
richness <- 2:18

## create vector of treatments
treat <- unique(mean_alphas$treatment)


## iterate calcs over every comm comp and treatment
for(k in 1:length(treat)){
  
  
  ## iterate at every richness level
  for(i in 1:length(richness)){
    
    ## create all possible combinations of composition at a given richness level
    comp <- data.frame(comboGeneral(species, m=richness[i], freqs = 1))
    
    
    ## iterate over each possible community composition
    for(j in 1:nrow(comp)){
      
      ## create a vector of community composition for each iteration
      cc <- as.character(comp[j,])
      
      trt <- treat[k]
      
      ## select precipitation treatment
      ## randomly sample rows
      tmp <- mean_alphas %>%
        filter(treatment %in% trt) %>%
        filter(species %in% cc)
      
      sp <- tmp$species
      
      ## select the matching columns
      tmp_alphas <- tmp %>%
        ungroup() %>%
        select(all_of(sp)) %>%
        as.matrix()
      
      ## pull out intrinsic growth rate vector
      tmp_igr <- as.numeric(tmp$lambda)
      
      ## test feasibility
      f <- test_feasibility(alpha = tmp_alphas, r = tmp_igr)
      
      ## calculate structural niche differences
      niche <- Omega(alpha=tmp_alphas)
      
      ## calculate structural fitness differences
      fitness <- theta(alpha = tmp_alphas, r=tmp_igr) 
      
      ## create dataframe
      temp <- data.frame(feasibility=NA)
      
      ## put back into dataframe
      temp$feasibility <- f
      temp$niche_diff <- niche
      temp$fitness_diff <- fitness
      
      ## get comm composition back into dataframe
      temp <- temp %>%
        mutate(ACAM = ifelse("ACAM" %in% colnames(tmp_alphas), 1, 0), 
               AMME = ifelse("AMME" %in% colnames(tmp_alphas), 1, 0),
               ANAR = ifelse("ANAR" %in% colnames(tmp_alphas), 1, 0),
               AVBA = ifelse("AVBA" %in% colnames(tmp_alphas), 1, 0),
               BRHO = ifelse("BRHO" %in% colnames(tmp_alphas), 1, 0),
               CESO = ifelse("CESO" %in% colnames(tmp_alphas), 1, 0), 
               GITR = ifelse("GITR" %in% colnames(tmp_alphas), 1, 0), 
               LOMU = ifelse("LOMU" %in% colnames(tmp_alphas), 1, 0),
               MAEL = ifelse("MAEL" %in% colnames(tmp_alphas), 1, 0),
               MICA = ifelse("MICA" %in% colnames(tmp_alphas), 1, 0),
               PLER = ifelse("PLER" %in% colnames(tmp_alphas), 1, 0),
               PLNO = ifelse("PLNO" %in% colnames(tmp_alphas), 1, 0), 
               TACA = ifelse("TACA" %in% colnames(tmp_alphas), 1, 0),
               THIR = ifelse("THIR" %in% colnames(tmp_alphas), 1, 0),
               TWIL = ifelse("TWIL" %in% colnames(tmp_alphas), 1, 0), 
               rich = richness[i], ## add richness column
               treatment = trt, 
               community = paste(i,j, sep = "")) ## add treatment column
      
      ## join with original dataframe
      allcomm <- rbind(allcomm, temp)
      
    }
    
  }
  
}

#all_comm <- allcomm[-1,]
#write.csv(all_comm, "models/CW/structural_coexistence_output.csv")

# Visualize ####
## Trt Facet ####
## faceted by treatment, colored by richness, and shape by feasibility 
ggplot(all_comm, aes(x=exp(niche_diff), y=fitness_diff, color = as.factor(rich), shape = as.factor(feasibility))) +
  geom_point() +
  #theme_few() +
  facet_wrap(~treatment) +
  #coord_cartesian(xlim = c(0,1)) +
  #scale_color_manual(values = vivid) +
  scale_shape_manual(values = c(1,16)) +
  #xlab("Structural Niche Differences") + ylab("Structural Fitness Differences") + labs(col = "Richness", shape = "Feasibility") +
  geom_abline(aes(intercept = 0, slope = 45)) + ## boundary of predicted coexistence or competitive exclusion
  theme(text = element_text(size = 14)) +
  theme_bw()

#ggsave("structural_coexistence.png", height = 5, width = 8)


## Rich-Trt Facet ####
ggplot(all_comm %>%
         arrange(feasibility), aes(x=exp(niche_diff), y=fitness_diff, shape = as.factor(feasibility), color = as.factor(feasibility))) +
  geom_point() +
  #theme_few() +
  facet_wrap(~as.factor(rich)*treatment, ncol = 8) +
  #coord_cartesian(xlim = c(0,1)) +
  scale_color_manual(values = c("gray", "black")) +
  scale_shape_manual(values = c(1,16)) +
  #xlab("Structural Niche Differences") + ylab("Structural Fitness Differences") + labs(col = "Richness", shape = "Feasibility") +
  geom_abline(aes(intercept = 0, slope = 45)) + ## boundary of predicted coexistence or competitive exclusion
  theme(text = element_text(size = 14)) +
  theme_bw()
#ggsave("structural_coexistence_rich_trt_updated.png", height = 10, width = 12)


## F Comm ONLY ####
## looking ONLY at feasible communities
ggplot(f_comm, aes(x=exp(niche_diff), y=fitness_diff, shape = as.factor(treatment), color = as.factor(rich))) +
  geom_point() +
  geom_abline(aes(intercept = 0, slope = 45)) + ## boundary of predicted coexistence or competitive exclusion
  scale_shape_manual(values = c(1,16)) +
  theme_bw()
#ggsave("structural_coexistence_feasible_comm_trt.png", height = 3, width = 5)