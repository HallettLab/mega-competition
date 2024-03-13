## run equilibriums with median param values

# Set up Env ####
## Read in Data ####
## posteriors
median_posts <- read.csv("data/parameter_summaries_20231218_models.csv")

## seed survival data
source("data_cleaning/seed-survival_data-cleaning/seed-survival_data-cleaning.R")

## germination data
source("data_cleaning/germination_data-cleaning/germination_rates.R")

## Clean Data ####
germ.sum <- germ.sum.sp.DC %>% ## rename so it's easier to use later on
  mutate(treatment = trt) %>%
  select(-trt)

rm(germ.sum.sp.DC)

## join with germ & survival data
median_posts_g <- left_join(median_posts, germ.sum[,c(1,3,5)], by = c("species", "treatment"))
median_posts_gs <- left_join(median_posts_g, surv.sum[,1:2], by = c("species")) 

### equilibrium abundance of resident sp
run.to.equilibrium <- function(surv, germ, lambda, alpha_intra, Nt, alpha_inter, germ_inter, inter_abund) {
  Ntp1 <- (1-germ)*surv*Nt + germ*lambda*Nt*exp(-alpha_intra *germ* Nt - alpha_inter*germ_inter*inter_abund)
  return(Ntp1)
}

theme_set(theme_classic())

# Median Params ####
## set up loop
species <- unique(median_posts_gs$species) ## make a vector of species
species <- species[-5]

treats <- unique(median_posts_gs$treatment)

median.pop <- data.frame(species = NA, treatment = NA, lambda = NA, alpha_intra = NA, pop_size = NA, timestep = NA)

## loop thru each species & treatment combo
for(sp in 1:length(species)) {
  
  for(trt in 1:length(treats)) {
    
    tmp.sp <- species[sp]
    tmp.trt <- treats[trt]
    
    tmp.params <- median_posts_gs %>%
      filter(species == tmp.sp, treatment == tmp.trt)
    
    intra <- paste0("alpha_", tolower(tmp.sp))
    
    tmp.pop <- data.frame(species = NA, treatment = NA, lambda = NA, alpha_intra = NA, pop_size = 100, timestep = 0)
    
    for(i in 1:100){
      
      ## subset model params
      timestep <- i
      
      lambda <- unique(tmp.params[tmp.params$parameter_type == "lambda",]$median_parameter)
      alpha_intra <- unique(tmp.params[tmp.params$parameter_type == intra,]$median_parameter)
      
      ## run pop model
      Nt_x <- run.to.equilibrium(germ = unique(tmp.params$avg.germ), 
                                 surv = unique(tmp.params$surv.mean.p),
                                 lambda = lambda, 
                                 alpha_intra = alpha_intra, 
                                 Nt = tmp.pop[tmp.pop$timestep == (i - 1),]$pop_size, 
                                 alpha_inter = 0,
                                 germ_inter = 0,
                                 inter_abund = 0) 
      
      ## make temp df to store outputs
      temp <- data.frame(species = NA, treatment = NA, lambda = NA, alpha_intra = NA, pop_size = NA, timestep = NA)
      temp$species <- tmp.sp
      temp$treatment <- tmp.trt
      temp$lambda <- lambda
      temp$alpha_intra <- alpha_intra
      temp$timestep <- timestep
      temp$pop_size <- Nt_x
      
      ## append
      tmp.pop <- rbind(tmp.pop, temp)
    
    }
    
    median.pop <- rbind(median.pop, tmp.pop)
    
  }
  
}

median.pop <- median.pop %>%
  filter(!is.na(species))

ggplot(median.pop, aes(x=timestep, y=pop_size, color = treatment)) +
  geom_line(linewidth = 0.75) +
  facet_wrap(~species, scales = "free", ncol = 3, nrow=5) +
  scale_color_manual(values = c("#70a494", "#de8a5a")) +
  xlab("Time Step") +
  ylab("Population Size") +
  labs(color = "Precip Trt") +
  ggtitle("Median Parameter Values")

ggsave("analyses/classic_MCT/preliminary_equil_abundance/median_params_equil_abund.png", width = 15, height = 8)


# Mean Params ####
species <- unique(median_posts_gs$species) ## make a vector of species
species <- species[-5]

treats <- unique(median_posts_gs$treatment)

mean.pop <- data.frame(species = NA, treatment = NA, lambda = NA, alpha_intra = NA, pop_size = NA, timestep = NA)

## loop thru each species & treatment combo
for(sp in 1:length(species)) {
  
  for(trt in 1:length(treats)) {
    
    tmp.sp <- species[sp]
    tmp.trt <- treats[trt]
    
    tmp.params <- median_posts_gs %>%
      filter(species == tmp.sp, treatment == tmp.trt)
    
    intra <- paste0("alpha_", tolower(tmp.sp))
    
    tmp.pop <- data.frame(species = NA, treatment = NA, lambda = NA, alpha_intra = NA, pop_size = 100, timestep = 0)
    
    for(i in 1:100){
      
      ## subset model params
      timestep <- i
      
      lambda <- unique(tmp.params[tmp.params$parameter_type == "lambda",]$mean_parameter)
      alpha_intra <- unique(tmp.params[tmp.params$parameter_type == intra,]$mean_parameter)
      
      ## run pop model
      Nt_x <- run.to.equilibrium(germ = unique(tmp.params$avg.germ), 
                                 surv = unique(tmp.params$surv.mean.p),
                                 lambda = lambda, 
                                 alpha_intra = alpha_intra, 
                                 Nt = tmp.pop[tmp.pop$timestep == (i - 1),]$pop_size, 
                                 alpha_inter = 0,
                                 germ_inter = 0,
                                 inter_abund = 0) 
      
      ## make temp df to store outputs
      temp <- data.frame(species = NA, treatment = NA, lambda = NA, alpha_intra = NA, pop_size = NA, timestep = NA)
      temp$species <- tmp.sp
      temp$treatment <- tmp.trt
      temp$lambda <- lambda
      temp$alpha_intra <- alpha_intra
      temp$timestep <- timestep
      temp$pop_size <- Nt_x
      
      ## append
      tmp.pop <- rbind(tmp.pop, temp)
      
    }
    
    mean.pop <- rbind(mean.pop, tmp.pop)
    
  }
  
}

mean.pop <- mean.pop %>%
  filter(!is.na(species))

ggplot(mean.pop, aes(x=timestep, y=pop_size, color = treatment)) +
  geom_line(linewidth = 0.75) +
  facet_wrap(~species, scales = "free", ncol = 3, nrow=5) +
  scale_color_manual(values = c("#70a494", "#de8a5a")) +
  xlab("Time Step") +
  ylab("Population Size") +
  labs(color = "Precip Trt") +
  ggtitle("Mean Parameter Values")

ggsave("analyses/classic_MCT/preliminary_equil_abundance/mean_params_equil_abund.png", width = 15, height = 8)

