## Calculate Competitive and Facilitative input for each species

# Set up Env ####
library(tidyverse)
theme_set(theme_classic())

source("analyses/interactions_v_traits/random_draws/clean_posteriors.R")

# Instrength Notes: 
## instrength = all the interactions one species experiences
## calculated by selecting one run of the model and summing all of the interspecific alphas


# Calc Instrength ####
## create vector of species
species <- unique(posts_clean$species)

## create empty df
in_strength <- data.frame(species = NA, treatment = NA, instrength_scaled = NA, instrength_raw = NA, type = NA, post.draw = NA)

for(i in 1:length(species)){
  
  ## select sp
  sp <- species[i]
  
  ## filter to one sp
  tmp <- posts_clean %>%
    filter(species == sp)
  
  ## get length of posterior
  post_length <- nrow(tmp)
  
  ## select 200 random numbers from posterior
  draws <- sample(post_length, 200)
  
  ## create empty df
  tmp.in <- data.frame(species = NA, treatment = NA, instrength_scaled = NA, instrength_raw = NA, type = NA, post.draw = NA)
  
  for(j in 1:length(draws)){
    
    ## select one run of model
    iter <- draws[j]
    
    tmp2 <- tmp[iter,]
    
    ## reformat data
    posts.long <- tmp2 %>%
      select(-X, -alpha_weeds_d, -alpha_weeds_c) %>%
      pivot_longer(cols = c(4:35), names_to = "alpha_name", values_to = "alpha_value") %>%
      mutate(treatment = toupper(substr(alpha_name, start = 12, stop = 12)),
             alpha_name = substr(alpha_name, start = 1, stop = 10), 
             phyto = species, 
             resident = toupper(substr(alpha_name, start = 7, stop = 10))) %>%
      filter(phyto != resident) ## filter out intra specific interactions
    
    ## scale alphas
    scaled <- posts.long %>%
      group_by(phyto, resident, treatment) %>%
      mutate(alpha_scaled = ifelse(treatment == "C", alpha_value/lambda_c, alpha_value/lambda_d),
             alpha_raw = alpha_value)
    
    ## calculate outstrength
    ## absolute
    in.abs <- scaled %>%
      group_by(treatment) %>%
      summarise(instrength_scaled = sum(abs(alpha_scaled)),
                instrength_raw = sum(abs(alpha_raw))) %>%
      mutate(type = "abs_value")
    
    ## net
    in.net <- scaled %>%
      group_by(treatment) %>%
      summarise(instrength_scaled = sum(alpha_scaled),
                instrength_raw = sum(alpha_raw)) %>%
      mutate(type = "net_value")
     
    ## facilitative
    facil <- scaled %>%
      group_by(treatment) %>%
      filter(alpha_scaled < 0) %>%
      summarise(instrength_scaled = sum(alpha_scaled),
                instrength_raw = sum(alpha_raw)) %>%
      mutate(type = "net_facil")
    
    ## competitive
    comp <- scaled %>%
      group_by(treatment) %>%
      filter(alpha_scaled > 0) %>%
      summarise(instrength_scaled = sum(alpha_scaled),
                instrength_raw = sum(alpha_raw)) %>%
      mutate(type = "net_comp")
    
    ## join all together
    all.in <- rbind(in.abs, in.net, facil, comp) %>%
      mutate(species = sp,
             post.draw = iter)
    
    ## append
    tmp.in <- rbind(tmp.in, all.in) %>%
      filter(!is.na(treatment))
    
  }
  
  ## append
  in_strength <- rbind(in_strength, tmp.in) %>%
    filter(!is.na(treatment))
  
}

# Save output ####
write.csv(in_strength, "data/instrength_filtered_raw_&_scaled_20240521.csv")
