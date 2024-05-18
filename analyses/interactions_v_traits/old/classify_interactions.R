# GOALS
## % competitive interactions
## % facilitative interactions
## num interactions overlapping 0

# Set up env ####
library(tidyverse)
library(bayestestR)

theme_set(theme_classic())

# Read in data ####
params <- read.csv("data/posteriors_20231218_models.csv")
reps <- read.csv("data/replicate-info.csv")

# filter to good reps only
good_reps <- reps %>%
  filter(true.reps > 3)

# Format interaction data ####
interxn <- params %>%
  pivot_longer(5:38, names_to = "alpha_name", values_to = "alpha_value") %>% ## change to long format
  filter(!alpha_name %in% c("alpha_weeds_c", "alpha_weeds_d")) %>% ## take out weed alphas
  mutate(combo = paste0(species, toupper(substr(alpha_name, 6,12)))) ## create combo col

# Classify interactions ####
species <- unique(interxn$species)
alphas <- unique(interxn$alpha_name)

## Full posterior distrib ####
alpha_sums_full <- data.frame(species = NA, alpha_name = NA, interaction_type = NA, combo = NA)

for(i in 1:length(species)){
  
  sp <- species[i]
  tmp <- interxn %>%
    filter(species == sp)
  
  for(j in 1:length(alphas)) {
    
    al <- alphas[j]
    tmp2 <- tmp %>%
      filter(alpha_name == al)

    combo <- unique(tmp2$combo)
    
    tmp3 <- data.frame(species = sp, alpha_name = al, combo = combo)
    
    if (min(tmp2$alpha_value) > 0) {
      
      ## if the min value is greater than 0 -> all competitive
      tmp3$interaction_type <- "competitive"
      
    } else if (max(tmp2$alpha_value) < 0 ) {
      
      ## if the max value is less than 0 -> all facilitative
      tmp3$interaction_type <- "facilitative"
      
    } else {
      
      tmp3$interaction_type <- "overlaps zero"
      
    }
    
    alpha_sums_full <- rbind(alpha_sums_full, tmp3)
    
  }
  
}

alpha_sums_full.filt <- alpha_sums_full %>%
  filter(!is.na(species),
         combo %in% good_reps$combos) %>%
  mutate(ci = 1)

## With 95% hdi ####
alpha_sums_95hdi <- data.frame(species = NA, alpha_name = NA, interaction_type = NA, combo = NA)

for(i in 1:length(species)){
  
  sp <- species[i]
  tmp <- interxn %>%
    filter(species == sp)
  
  for(j in 1:length(alphas)) {
    
    al <- alphas[j]
    tmp2 <- tmp %>%
      filter(alpha_name == al)
    
    combo <- unique(tmp2$combo)
    
    tmp3 <- data.frame(species = sp, alpha_name = al, combo = combo)
    
    ci <- hdi(tmp2$alpha_value)
    
    if (min(ci$CI_low) > 0) {
      
      ## if the min value is greater than 0 -> all competitive
      tmp3$interaction_type <- "competitive"
      
    } else if (max(ci$CI_high) < 0 ) {
      
      ## if the max value is less than 0 -> all facilitative
      tmp3$interaction_type <- "facilitative"
      
    } else {
      
      tmp3$interaction_type <- "overlaps zero"
      
    }
    
    alpha_sums_95hdi <- rbind(alpha_sums_95hdi, tmp3)
    
  }
  
}

alpha_sums_95hdi.filt <- alpha_sums_95hdi %>%
  filter(!is.na(species),
         combo %in% good_reps$combos) %>%
  mutate(ci = 0.95)

## With 89% hdi ####
alpha_sums_89hdi <- data.frame(species = NA, alpha_name = NA, combo = NA, interaction_type = NA, ci_hi = NA, ci_lo = NA)

for(i in 1:length(species)){
  
  sp <- species[i]
  tmp <- interxn %>%
    filter(species == sp)
  
  for(j in 1:length(alphas)) {
    
    al <- alphas[j]
    tmp2 <- tmp %>%
      filter(alpha_name == al)
    
    combo <- unique(tmp2$combo)
    
    tmp3 <- data.frame(species = sp, alpha_name = al, combo = combo)
    
    ci <- hdi(tmp2$alpha_value, ci = 0.89)
    
    tmp3$ci_hi <- ci$CI_high
    tmp3$ci_lo <- ci$CI_low
    
    if (ci$CI_low > 0) {
      
      ## if the min value is greater than 0 -> all competitive
      tmp3$interaction_type <- "competitive"
      
    } else if (ci$CI_high < 0 ) {
      
      ## if the max value is less than 0 -> all facilitative
      tmp3$interaction_type <- "facilitative"
      
    } else if (ci$CI_low < 0 & ci$CI_high > 0) {
      
      tmp3$interaction_type <- "overlaps zero"
      
    } else {
      
      tmp3$interaction_type <- "check"
      
    }
    
    alpha_sums_89hdi <- rbind(alpha_sums_89hdi, tmp3)
    
  }
  
}

alpha_sums_89hdi.filt <- alpha_sums_89hdi %>%
  filter(!is.na(species),
         combo %in% good_reps$combos) %>%
  mutate(ci = 0.89)

# Join all together ####
all_alphas <- rbind(alpha_sums_89hdi.filt, alpha_sums_95hdi.filt, alpha_sums_full.filt) %>%
  mutate(treatment = ifelse(substr(alpha_name, 12, 13) == "d", "D", "C"),
         alpha_name = substr(alpha_name, 1, 10))

# Calc %'s ####
## separated by treatment
alpha_sums_trt <- all_alphas %>%
  group_by(ci, treatment) %>%
  mutate(tot = n()) %>%
  ungroup() %>%
  group_by(ci, treatment, interaction_type) %>%
  summarise(num = n(),
            prop = num/unique(tot))

alpha_types <- all_alphas %>%
  group_by(ci) %>%
  mutate(tot = n()) %>%
  ungroup() %>%
  group_by(ci, interaction_type) %>%
  summarise(num = n(),
            prop = num/unique(tot))

# Visualize ####
## overall interactions 
ggplot(alpha_types, aes(x=interaction_type, y=prop)) +
  geom_bar(stat="identity") +
  facet_wrap(~ci) +
  ylab("Proportion of interactions") +
  xlab("Interaction type")

ggsave("analyses/explore_interaction_coeff/preliminary_figures/interaction_type_by_ci.png", width = 7, height = 3)

## overall interactions faceted by treatment
ggplot(alpha_sums_trt[alpha_sums_trt$ci == 0.89,], aes(x=interaction_type, y=prop, fill = treatment)) +
  geom_col() +
  facet_wrap(~treatment) +
  ylab("Proportion of interactions") +
  xlab("Interaction type") +
  scale_fill_manual(values = c("#70a494", "#de8a5a"))

ggsave("analyses/explore_interaction_coeff/preliminary_figures/interaction_type_0.89_by_trt.png", width = 7, height = 3)

## faceted by sp / treatment
ggplot(all_alphas, aes(x= as.factor(ci), fill = interaction_type)) +
  geom_bar() +
  facet_wrap(~species*treatment, nrow = 4, ncol = 8) +
  xlab("Confidence interval") +
  ggtitle("Interactions experienced by faceted sp")

ggsave("analyses/explore_interaction_coeff/preliminary_figures/interaction_type_by_sp_trt_ci.png", width = 14, height = 7)

## faceted by alpha / treatment
ggplot(all_alphas, aes(x= as.factor(ci), fill = interaction_type)) +
  geom_bar() +
  facet_wrap(~alpha_name*treatment, nrow = 4, ncol = 8) +
  xlab("Confidence interval")

ggsave("analyses/explore_interaction_coeff/preliminary_figures/interaction_type_by_alpha_trt_ci.png", width = 14, height = 7)
