# run coexistence models for dry versus wet

# Set up ####
theme_set(theme_classic())

date <- 20231218
## Read in Data ####
## model posteriors
#source("Models/CW/ricker_model/random_effects_block/negative_binomial/posterior_processing.R")
posteriors <- read.csv("data/posteriors_20231218_models.csv")
median_posts <- read.csv("data/parameter_summaries_20231218_models.csv")

## seed survival data
source("data_cleaning/seed-survival_data-cleaning/seed-survival_data-cleaning.R")

## germination data
source("data_cleaning/germination_data-cleaning/germination_rates.R")

germ.sum <- germ.sum.sp.DC %>% ## rename so it's easier to use later on
  mutate(treatment = trt) %>%
  select(-trt)

rm(germ.sum.sp.DC)

## replicate information
reps <- read.csv("data/replicate-info.csv")

## clean posteriors up
posteriors_long <- posteriors %>%
  pivot_longer(5:38, names_to = "alpha_name", values_to = "alpha_value") %>% ## change to long format
  filter(!alpha_name %in% c("alpha_weeds_c", "alpha_weeds_d")) %>% ## take out weed alphas
  mutate(combo = paste0(species, toupper(substr(alpha_name, 6,12)))) %>% ## create combo col
  mutate(treatment = ifelse(substr(alpha_name, 12, 13) == "d", "D", "C"),
         alpha_name = substr(alpha_name, 1, 10)) %>%
  filter(toupper(substr(alpha_name, start = 7, stop = 10)) == species)

## join with germ & survival data
median_posts_g <- left_join(median_posts, germ.sum[,c(1,3,5)], by = c("species", "treatment"))
median_posts_gs <- left_join(median_posts_g, surv.sum[,1:2], by = c("species")) %>%
  mutate(alpha_name = parameter_type)

#posteriors_gs <- left_join(posteriors, median_posts_gs, by = c("species"))
posts_all <- left_join(posteriors_long, median_posts_gs[,c(2,3,11,7:10)], by = c("species", "treatment", "alpha_name")) %>%
  group_by(species, alpha_name, treatment) %>%
  filter(alpha_value > hdi_lo & alpha_value < hdi_hi)

# Equations ####
## 'As-Is' ####
### equilibrium abundance of resident sp
run.to.equilibrium <- function(surv, germ, lambda, alpha_intra, Nt, alpha_inter, germ_inter, inter_abund) {
  Ntp1 <- (1-germ)*surv*Nt + germ*lambda*Nt*exp(-alpha_intra *germ* Nt - alpha_inter*germ_inter*inter_abund)
  return(Ntp1)
}

# Run to Equilibrium ####
## Set up loop ####
#all_intra <- c("alpha_acam",  
               #"alpha_amme", 
              # "alpha_anar", 
              # "alpha_brho",  
               #"alpha_brni", 
              # "alpha_ceso",
              # "alpha_gitr", 
              # "alpha_leni",
              # "alpha_lomu", 
              # "alpha_mael", 
              # "alpha_mica", 
              # "alpha_pler", 
              # "alpha_plno",  
              # "alpha_taca", 
              # "alpha_thir", 
              # "alpha_twil") 

#options <- length(all_intra)

#time <- 300
reps <- 1:200

#N <- array(NA, c(options, runs, time))
#N[,,1] <- 100 # start with 100 individuals in every case
## create an array where each of the rows is one of the species-treatment combos arranged in the order of all_intra. 
## Each of the columns is one separate run of the model
## Each of the stacked matrices represents a particular time slice

## create empty dataframes
#residents_dry <- as.data.frame(matrix(data = NA, nrow = 200, ncol = 1))
#residents_wet <- as.data.frame(matrix(data = NA, nrow = 200, ncol = 1))

set.seed(42)

species <- unique(posts_all$species)

pop.eqm <- data.frame(species = NA, treatment = NA, lambda = NA, alpha_intra = NA, pop_size = NA, timestep = NA, rep = NA)

### Loop thru all posteriors ####
#### DRY ####
for(i in 1:length(species)) {
  
  sp <- species[i]
  
  ## select the species
  datset <- posts_all %>%
    filter(species == sp, treatment == "D")
  
  ## set the intraspecific alpha name
  intra <- paste0("alpha_", tolower(sp))
  
  ## make a vector of the length of the posterior distribution
  post_length <- length(datset$lambda_d)
  
  ## get list of all intraspecific alphas
  all_intras <- datset$alpha_value
  
  tmp.pop.reps <- data.frame(species = NA, treatment = NA, lambda = NA, alpha_intra = NA, pop_size = NA, timestep = NA, rep = NA)
  
  ## do 200 repititions
  for(j in 1:length(reps)) {
    
    r <- reps[j]
    
    ## get a posterior
    posts <- sample(post_length, 1, replace=TRUE)
    
    tmp.pop <- data.frame(species = NA, treatment = NA, lambda = NA, alpha_intra = NA, pop_size = 100, timestep = 0, rep = NA)
    
    ## loop thru each of 100 time steps
    for(k in 1:100){
      
      ## subset model params
      timestep <- k
      
      ## use these indices to select a lambda values
      lambda <- datset$lambda_d[posts]
      
      ## use again to select an intra_alpha values
      alpha_intra <- all_intras[posts]
      
      ## run pop model
      Nt_x <- run.to.equilibrium(germ = unique(datset$avg.germ), 
                                 surv = unique(datset$surv.mean.p),
                                 lambda = lambda, 
                                 alpha_intra = alpha_intra, 
                                 Nt = tmp.pop[tmp.pop$timestep == (k - 1),]$pop_size, 
                                 alpha_inter = 0,
                                 germ_inter = 0,
                                 inter_abund = 0) 
      
      ## make temp df to store outputs
      temp <- data.frame(species = NA, treatment = NA, lambda = NA, alpha_intra = NA, pop_size = NA, timestep = NA, rep = NA)
      temp$species <- sp
      temp$treatment <- "D"
      temp$lambda <- lambda
      temp$alpha_intra <- alpha_intra
      temp$timestep <- timestep
      temp$pop_size <- Nt_x
      temp$rep <- r
      
      ## append
      tmp.pop <- rbind(tmp.pop, temp)
      
    }
    
    tmp.pop.reps <- rbind(tmp.pop.reps, tmp.pop)
    
  }
  
  pop.eqm <- rbind(pop.eqm, tmp.pop.reps)
  
}
  
  
finals <- pop.eqm %>%
  filter(timestep == 100)

ggplot(finals, aes(x=pop_size)) +
  geom_histogram() +
facet_wrap(~species, scales = "free", nrow= 3, ncol = 5) +
  xlab("final pop size, 100 timesteps")

ggsave("analyses/classic_MCT/preliminary_equil_abundance/random_consistent_draws_equil_abund_hist.png", width = 12, height = 5)

ggplot(finals, aes(x=rep, y=pop_size)) +
  geom_point() +
  facet_wrap(~species, scales = "free", nrow= 3, ncol = 5) +
  xlab("final pop size, 100 timesteps")

ggsave("analyses/classic_MCT/preliminary_equil_abundance/random_consistent_draws_equil_abund_scatter.png", width = 12, height = 5)
