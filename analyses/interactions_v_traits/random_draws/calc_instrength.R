## Competitive and Facilitative output

# Set up Env ####
library(tidyverse)

## read in posterior data
posts <- read.csv("data/posteriors_20231218_models.csv")

reps <- read.csv("data/replicate-info.csv")

## need to filter outstrength data first... some of the alpha posterior distributions are quite wide and are causing weird outputs

bad.reps <- reps %>%
  filter(true.reps < 4) %>%
  filter(!bkgrd %in% c("CLPU", "AVBA", "ERBO"), 
         !phyto %in% c("CLPU", "AVBA", "ERBO"))

posts_filt <- posts %>%
  mutate()


# Calc Outstrength ####
species <- unique(posts$species)

species <- c("ANAR", "BRHO", "CESO", "GITR", "LENI", "MICA", "PLER", 
"TACA", "THIR")

## create empty df
interaction_out <- data.frame(species = NA, treatment = NA, outstrength_scaled = NA, outstrength_raw = NA, type = NA, post.draw = NA)

for(i in 1:length(species)){
  
  ## select sp
  sp <- species[i]
  
  ## filter to one sp
  tmp <- posts %>%
    filter(species == sp)
  
  ## get length of posterior
  post_length <- nrow(tmp)
  
  ## select 200 random numbers from posterior
  draws <- sample(post_length, 200)
  
  ## create empty df
  tmp.output <- data.frame(species = NA, treatment = NA, outstrength_scaled = NA, outstrength_raw = NA, type = NA, post.draw = NA)
  
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
             resident = toupper(substr(alpha_name, start = 7, stop = 10)))
    
    ## scale alphas
    scaled <- posts.long %>%
      group_by(phyto, resident, treatment) %>%
      mutate(alpha_scaled = ifelse(treatment == "C", alpha_value/lambda_c, alpha_value/lambda_d),
             alpha_raw = alpha_value)
    
    ## calculate outstrength
    ## absolute
    out.abs <- scaled %>%
      group_by(treatment) %>%
      summarise(outstrength_scaled = sum(abs(alpha_scaled)),
                outstrength_raw = sum(abs(alpha_raw))) %>%
      mutate(type = "abs_value")
    
    ## net
    out.net <- scaled %>%
      group_by(treatment) %>%
      summarise(outstrength_scaled = sum(alpha_scaled),
                outstrength_raw = sum(alpha_raw)) %>%
      mutate(type = "net_value")
     
    ## facilitative
    facil <- scaled %>%
      group_by(treatment) %>%
      filter(alpha_scaled < 0) %>%
      summarise(outstrength_scaled = sum(alpha_scaled),
                outstrength_raw = sum(alpha_raw)) %>%
      mutate(type = "net_facil")
    
    ## competitive
    comp <- scaled %>%
      group_by(treatment) %>%
      filter(alpha_scaled > 0) %>%
      summarise(outstrength_scaled = sum(alpha_scaled),
                outstrength_raw = sum(alpha_raw)) %>%
      mutate(type = "net_comp")
    
    ## join all together
    all.out <- rbind(out.abs, out.net, facil, comp) %>%
      mutate(species = sp,
             post.draw = iter)
    
    ## append
    tmp.output <- rbind(tmp.output, all.out) %>%
      filter(!is.na(treatment))
    
  }
  
  ## append
  interaction_out <- rbind(interaction_out, tmp.output) %>%
    filter(!is.na(treatment))
  
}

# Save output ####
write.csv(interaction_out, "data/outstrength_raw_&_scaled_20240514.csv")
