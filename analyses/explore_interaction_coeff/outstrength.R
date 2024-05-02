## Competitive and Facilitative output
library(tidyverse)

posts <- read.csv("data/posteriors_20231218_models.csv")


species <- unique(posts$species)

interaction_out <- data.frame(species = NA, treatment = NA, outstrength = NA, type = NA, post.draw = NA)

for(i in 1:length(species)){
  
  sp <- species[i] ## select sp
  
  ## filter to one sp
  tmp <- posts %>%
    filter(species == sp)
  
  ## get length of posterior
  post_length <- nrow(tmp)
  
  ## select 200 random numbers from posterior
  draws <- sample(post_length, 200)
  
  ## create empty df
  tmp.output <- data.frame(species = NA, treatment = NA, outstrength = NA, type = NA, post.draw = NA)
  
  for(j in 1:length(draws)){
    
    iter <- draws[j]
    
    tmp2 <- tmp[iter,]
    
    ## reformat
    posts.long <- tmp2 %>%
      select(-X, -alpha_weeds_d, -alpha_weeds_c) %>%
      pivot_longer(cols = c(4:35), names_to = "alpha_name", values_to = "alpha_value") %>%
      mutate(treatment = toupper(substr(alpha_name, start = 12, stop = 12)),
             alpha_name = substr(alpha_name, start = 1, stop = 10), 
             phyto = species, 
             resident = toupper(substr(alpha_name, start = 7, stop = 10)))
    
    ## scale
    scaled <- posts.long %>%
      group_by(phyto, resident, treatment) %>%
      summarise(alpha_scaled = ifelse(treatment == "C", alpha_value/lambda_c, alpha_value/lambda_d))
    
    ## calculate outstrength
    out.abs <- scaled %>%
      group_by(treatment) %>%
      summarise(outstrength = sum(abs(alpha_scaled))) %>%
      mutate(type = "abs_value")
    
    out.net <- scaled %>%
      group_by(treatment) %>%
      summarise(outstrength = sum(alpha_scaled)) %>%
      mutate(type = "net_value")
     
    facil <- scaled %>%
      group_by(treatment) %>%
      filter(alpha_scaled < 0) %>%
      summarise(outstrength = sum(alpha_scaled)) %>%
      mutate(type = "net_facil")
    
    comp <- scaled %>%
      group_by(treatment) %>%
      filter(alpha_scaled > 0) %>%
      summarise(outstrength = sum(alpha_scaled)) %>%
      mutate(type = "net_comp")
    
    all.out <- rbind(out.abs, out.net, facil, comp) %>%
      mutate(species = sp,
             post.draw = iter)
    
    tmp.output <- rbind(tmp.output, all.out) %>%
      filter(!is.na(treatment))
    
  }
  
  interaction_out <- rbind(interaction_out, tmp.output) %>%
    filter(!is.na(treatment))
  
}

write.csv(interaction_out, "data/outstrength_20240424.csv")

## iterate over replication?

## select one draw of the posteriors

## reformat?

## scale alphas

## calculate outstrength of interactions

## save output


posts.long <- posts %>%
  select(-X, -lambda_d, -lambda_c, -alpha_weeds_d, -alpha_weeds_c) %>%
  pivot_longer(cols = c(2:33), names_to = "alpha_name", values_to = "alpha_value") %>%
  mutate(treatment = toupper(substr(alpha_name, start = 12, stop = 12)),
         alpha_name = substr(alpha_name, start = 1, stop = 10), 
         phyto = species, 
         resident = toupper(substr(alpha_name, start = 7, stop = 10)))

posts.long2 <- 



# with random draws ####


for(i in species){
  
  
  
  
  
}


# with summaries ####
## outstrength
inter_sc2 <- inter_sc %>%
  mutate(fg = ifelse(resident %in% c("TACA", "BRHO", "LOMU"), "grass",
                     ifelse(resident %in% c("TWIL", "THIR", "ACAM"), "legume", "forb")))


out <- inter_sc2 %>%
  group_by(phyto, resident, treatment, fg) %>%
  summarise(outstrength = sum(abs(alpha_scaled)))

facil <- inter_sc2 %>%
  group_by(phyto, resident, treatment, fg) %>%
  filter(alpha_scaled < 0) %>%
  summarise(outstrength = sum(alpha_scaled))

comp <- inter_sc2 %>%
  group_by(phyto, resident, treatment, fg) %>%
  filter(alpha_scaled > 0) %>%
  summarise(outstrength = sum(alpha_scaled))

ggplot(out, aes(x=resident, y=outstrength)) +
  geom_boxplot() +
  ylab("Abs Val Median Outstrength")

ggplot(facil, aes(x=resident, y=abs(outstrength), color = fg)) +
  geom_boxplot() +
  ylab("Abs Val Facil Median Outstrength")

ggplot(comp, aes(x=resident, y=abs(outstrength))) +
  geom_boxplot() +
  ylab("Abs Val Comp Median Outstrength") +
  coord_cartesian(ylim = c(0,0.01))

test <- lm(outstrength ~ fg, data = comp)
summary(test)
Anova(test)

