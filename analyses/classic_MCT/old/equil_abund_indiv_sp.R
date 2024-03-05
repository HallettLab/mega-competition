
## STILL NEED TO INCORPORATE RANDOM EFFECTS



source("Models/CW/ricker_model/random_effects_block/negative_binomial/posterior_processing.R")

## seed survival data
source("data_cleaning/seed-survival_data-cleaning/seed-survival_data-cleaning.R")

## germination data
source("data_cleaning/germination_data-cleaning/germination_rates.R")

germ.sum <- germ.sum.sp.DC ## rename so it's easier to use later on
rm(germ.sum.sp.DC)

### equilibrium abundance of resident sp
run.to.equilibrium <- function(surv, germ, lambda, alpha_intra, Nt, alpha_inter, germ_inter, inter_abund) {
  Ntp1 <- (1-germ)*surv*Nt + germ*lambda*Nt*exp(-alpha_intra *germ* Nt - alpha_inter*germ_inter*inter_abund)
  return(Ntp1)
}


# germ rates dry
dry[["ACAM"]]$germ <- germ.sum[germ.sum$species == "ACAM" & germ.sum$trt == "D", ]$avg.germ
dry[["AMME"]]$germ <- germ.sum[germ.sum$species == "AMME" & germ.sum$trt == "D", ]$avg.germ
dry[["ANAR"]]$germ <- germ.sum[germ.sum$species == "ANAR" & germ.sum$trt == "D", ]$avg.germ
dry[["BRHO"]]$germ <- germ.sum[germ.sum$species == "BRHO" & germ.sum$trt == "D", ]$avg.germ
dry[["BRNI"]]$germ <- germ.sum[germ.sum$species == "BRNI" & germ.sum$trt == "D", ]$avg.germ
dry[["CESO"]]$germ <- germ.sum[germ.sum$species == "CESO" & germ.sum$trt == "D", ]$avg.germ
dry[["GITR"]]$germ <- germ.sum[germ.sum$species == "GITR" & germ.sum$trt == "D", ]$avg.germ
dry[["LENI"]]$germ <- germ.sum[germ.sum$species == "LENI" & germ.sum$trt == "D", ]$avg.germ
dry[["LOMU"]]$germ <- germ.sum[germ.sum$species == "LOMU" & germ.sum$trt == "D", ]$avg.germ
dry[["MAEL"]]$germ <- germ.sum[germ.sum$species == "MAEL" & germ.sum$trt == "D", ]$avg.germ
dry[["MICA"]]$germ <- germ.sum[germ.sum$species == "MICA" & germ.sum$trt == "D", ]$avg.germ
dry[["PLER"]]$germ <- germ.sum[germ.sum$species == "PLER" & germ.sum$trt == "D", ]$avg.germ
dry[["PLNO"]]$germ <- germ.sum[germ.sum$species == "PLNO" & germ.sum$trt == "D", ]$avg.germ
dry[["TACA"]]$germ <- germ.sum[germ.sum$species == "TACA" & germ.sum$trt == "D", ]$avg.germ
dry[["THIR"]]$germ <- germ.sum[germ.sum$species == "THIR" & germ.sum$trt == "D", ]$avg.germ
dry[["TWIL"]]$germ <- germ.sum[germ.sum$species == "TWIL" & germ.sum$trt == "D", ]$avg.germ

# germ rates wet
wet[["ACAM"]]$germ <- germ.sum[germ.sum$species == "ACAM" & germ.sum$trt == "C", ]$avg.germ
wet[["AMME"]]$germ <- germ.sum[germ.sum$species == "AMME" & germ.sum$trt == "C", ]$avg.germ
wet[["ANAR"]]$germ <- germ.sum[germ.sum$species == "ANAR" & germ.sum$trt == "C", ]$avg.germ
wet[["BRHO"]]$germ <- germ.sum[germ.sum$species == "BRHO" & germ.sum$trt == "C", ]$avg.germ
wet[["BRNI"]]$germ <- germ.sum[germ.sum$species == "BRNI" & germ.sum$trt == "C", ]$avg.germ
wet[["CESO"]]$germ <- germ.sum[germ.sum$species == "CESO" & germ.sum$trt == "C", ]$avg.germ
wet[["GITR"]]$germ <- germ.sum[germ.sum$species == "GITR" & germ.sum$trt == "C", ]$avg.germ
wet[["LENI"]]$germ <- germ.sum[germ.sum$species == "LENI" & germ.sum$trt == "C", ]$avg.germ
wet[["LOMU"]]$germ <- germ.sum[germ.sum$species == "LOMU" & germ.sum$trt == "C", ]$avg.germ
wet[["MAEL"]]$germ <- germ.sum[germ.sum$species == "MAEL" & germ.sum$trt == "C", ]$avg.germ
wet[["MICA"]]$germ <- germ.sum[germ.sum$species == "MICA" & germ.sum$trt == "C", ]$avg.germ
wet[["PLER"]]$germ <- germ.sum[germ.sum$species == "PLER" & germ.sum$trt == "C", ]$avg.germ
wet[["PLNO"]]$germ <- germ.sum[germ.sum$species == "PLNO" & germ.sum$trt == "C", ]$avg.germ
wet[["TACA"]]$germ <- germ.sum[germ.sum$species == "TACA" & germ.sum$trt == "C", ]$avg.germ
wet[["THIR"]]$germ <- germ.sum[germ.sum$species == "THIR" & germ.sum$trt == "C", ]$avg.germ
wet[["TWIL"]]$germ <- germ.sum[germ.sum$species == "TWIL" & germ.sum$trt == "C", ]$avg.germ

## Seed Survival ####
## add seed survival to models
dry[["ACAM"]]$surv <- surv.sum[surv.sum$species == "ACAM",]$surv.mean.p
dry[["AMME"]]$surv <- surv.sum[surv.sum$species == "AMME",]$surv.mean.p
dry[["ANAR"]]$surv <- surv.sum[surv.sum$species == "ANAR",]$surv.mean.p
dry[["BRHO"]]$surv <- surv.sum[surv.sum$species == "BRHO",]$surv.mean.p
dry[["BRNI"]]$surv <- surv.sum[surv.sum$species == "BRNI",]$surv.mean.p
dry[["CESO"]]$surv <- surv.sum[surv.sum$species == "CESO",]$surv.mean.p
dry[["GITR"]]$surv <- surv.sum[surv.sum$species == "GITR",]$surv.mean.p
dry[["LENI"]]$surv <- surv.sum[surv.sum$species == "LENI",]$surv.mean.p
dry[["LOMU"]]$surv <- surv.sum[surv.sum$species == "LOMU",]$surv.mean.p
dry[["MAEL"]]$surv <- surv.sum[surv.sum$species == "MAEL",]$surv.mean.p
dry[["MICA"]]$surv <- surv.sum[surv.sum$species == "MICA",]$surv.mean.p
dry[["PLER"]]$surv <- surv.sum[surv.sum$species == "PLER",]$surv.mean.p
dry[["PLNO"]]$surv <- surv.sum[surv.sum$species == "PLNO",]$surv.mean.p
dry[["TACA"]]$surv <- surv.sum[surv.sum$species == "TACA",]$surv.mean.p
dry[["THIR"]]$surv <- surv.sum[surv.sum$species == "THIR",]$surv.mean.p
dry[["TWIL"]]$surv <- surv.sum[surv.sum$species == "TWIL",]$surv.mean.p

wet[["ACAM"]]$surv <- surv.sum[surv.sum$species == "ACAM",]$surv.mean.p
wet[["AMME"]]$surv <- surv.sum[surv.sum$species == "AMME",]$surv.mean.p
wet[["ANAR"]]$surv <- surv.sum[surv.sum$species == "ANAR",]$surv.mean.p
wet[["BRHO"]]$surv <- surv.sum[surv.sum$species == "BRHO",]$surv.mean.p
wet[["BRNI"]]$surv <- surv.sum[surv.sum$species == "BRNI",]$surv.mean.p
wet[["CESO"]]$surv <- surv.sum[surv.sum$species == "CESO",]$surv.mean.p
wet[["GITR"]]$surv <- surv.sum[surv.sum$species == "GITR",]$surv.mean.p
wet[["LENI"]]$surv <- surv.sum[surv.sum$species == "LENI",]$surv.mean.p
wet[["LOMU"]]$surv <- surv.sum[surv.sum$species == "LOMU",]$surv.mean.p
wet[["MAEL"]]$surv <- surv.sum[surv.sum$species == "MAEL",]$surv.mean.p
wet[["MICA"]]$surv <- surv.sum[surv.sum$species == "MICA",]$surv.mean.p
wet[["PLER"]]$surv <- surv.sum[surv.sum$species == "PLER",]$surv.mean.p
wet[["PLNO"]]$surv <- surv.sum[surv.sum$species == "PLNO",]$surv.mean.p
wet[["TACA"]]$surv <- surv.sum[surv.sum$species == "TACA",]$surv.mean.p
wet[["THIR"]]$surv <- surv.sum[surv.sum$species == "THIR",]$surv.mean.p
wet[["TWIL"]]$surv <- surv.sum[surv.sum$species == "TWIL",]$surv.mean.p



# BRHO ####
# Try population models with just BRHO

## set up loop
brho_dry <- dry[["BRHO"]]

intra <- "alpha_brho"

post_length <- length(brho_dry$lambda)

brho_dry_pop <- data.frame(lambda = NA, alpha_intra = NA, pop_size = 100, timestep = 0, post_num = NA)

posts <- sample(post_length, 100)


for(reps in 1:length(posts)){
  
  post_num <- posts[reps]
  
  lambda <- brho_dry$lambda[post_num]
  
  alpha_intra <- brho_dry$alpha_brho[post_num]
  
  temp2 <- data.frame(lambda = NA, alpha_intra = NA, pop_size = 100, timestep = 0, post_num = NA)
  
  for(i in 1:100){
    
    timestep <- i
    
    temp <- data.frame(lambda = NA, alpha_intra = NA, pop_size = NA, timestep = NA, post_num = NA)
    
    Nt_x <- run.to.equilibrium(germ = brho_dry$germ, 
                               surv = brho_dry$surv,
                               lambda = lambda, 
                               alpha_intra = alpha_intra, 
                               Nt = temp2[temp2$timestep == (i - 1),]$pop_size, 
                               alpha_inter = 0,
                               germ_inter = 0,
                               inter_abund = 0) 
    
    
    temp$lambda <- lambda
    temp$alpha_intra <- alpha_intra
    temp$timestep <- timestep
    temp$pop_size <- Nt_x
    temp$post_num <- post_num
    
    temp2 <- rbind(temp2, temp)
    
  }
  
  brho_dry_pop <- rbind(brho_dry_pop, temp2)
  
}


brho_dry_pop$lambda <- as.integer(brho_dry_pop$lambda)

ggplot(brho_dry_pop[brho_dry_pop$lambda < 150,], aes(x=timestep, y=pop_size, color = alpha_intra)) +
  geom_point() +
  geom_line() +
  facet_wrap(~lambda)

ggsave("analyses/classic_MCT/preliminary_equil_abundance/20240219_species_specific/BRHO/brho_eq_abund_L150.png", width = 16, height = 10)

ggplot(brho_dry_pop[brho_dry_pop$lambda < 200 & brho_dry_pop$lambda > 150,], aes(x=timestep, y=pop_size, color = alpha_intra)) +
  geom_point() +
  geom_line() +
  facet_wrap(~lambda)

ggsave("analyses/classic_MCT/preliminary_equil_abundance/20240219_species_specific/BRHO/brho_eq_abund_L150_200.png", width = 16, height = 10)

ggplot(brho_dry_pop[brho_dry_pop$lambda > 200,], aes(x=timestep, y=pop_size, color = alpha_intra)) +
  geom_point() +
  geom_line() +
  facet_wrap(~lambda)

ggsave("analyses/classic_MCT/preliminary_equil_abundance/20240219_species_specific/BRHO/brho_eq_abund_L200.png", width = 16, height = 10)

# GITR ####
# Try population models with just GITR

## set up loop
gitr_dry <- dry[["GITR"]]

intra <- "alpha_gitr"

post_length <- length(gitr_dry$lambda)

gitr_dry_pop <- data.frame(lambda = NA, alpha_intra = NA, pop_size = 100, timestep = 0, post_num = NA)

posts <- sample(post_length, 100)


for(reps in 1:length(posts)){
  
  post_num <- posts[reps]
  
  lambda <- gitr_dry$lambda[post_num]
  
  alpha_intra <- gitr_dry$alpha_gitr[post_num]
  
  temp2 <- data.frame(lambda = NA, alpha_intra = NA, pop_size = 100, timestep = 0, post_num = NA)
  
  for(i in 1:100){
    
    timestep <- i
    
    temp <- data.frame(lambda = NA, alpha_intra = NA, pop_size = NA, timestep = NA, post_num = NA)
    
    Nt_x <- run.to.equilibrium(germ = gitr_dry$germ, 
                               surv = gitr_dry$surv,
                               lambda = lambda, 
                               alpha_intra = alpha_intra, 
                               Nt = temp2[temp2$timestep == (i - 1),]$pop_size, 
                               alpha_inter = 0,
                               germ_inter = 0,
                               inter_abund = 0) 
    
    
    temp$lambda <- lambda
    temp$alpha_intra <- alpha_intra
    temp$timestep <- timestep
    temp$pop_size <- Nt_x
    temp$post_num <- post_num
    
    temp2 <- rbind(temp2, temp)
    
  }
  
  gitr_dry_pop <- rbind(gitr_dry_pop, temp2)
  
}


gitr_dry_pop$lambda <- as.integer(gitr_dry_pop$lambda)

ggplot(gitr_dry_pop, aes(x=timestep, y=pop_size, color = alpha_intra)) +
  geom_point() +
  geom_line() +
  facet_wrap(~lambda)

ggplot(gitr_dry_pop[gitr_dry_pop$lambda < 200,], aes(x=timestep, y=pop_size, color = alpha_intra)) +
  geom_point() +
  geom_line() +
  facet_wrap(~lambda)

ggsave("analyses/classic_MCT/preliminary_equil_abundance/20240219_species_specific/GITR/gitr_eq_abund_L200.png", width = 16, height = 10)

ggplot(gitr_dry_pop[gitr_dry_pop$lambda > 200 & gitr_dry_pop$lambda < 255,], aes(x=timestep, y=pop_size, color = alpha_intra)) +
  geom_point() +
  geom_line() +
  facet_wrap(~lambda)

ggsave("analyses/classic_MCT/preliminary_equil_abundance/20240219_species_specific/GITR/gitr_eq_abund_L200_255.png", width = 16, height = 10)

ggplot(gitr_dry_pop[gitr_dry_pop$lambda > 255,], aes(x=timestep, y=pop_size, color = alpha_intra)) +
  geom_point() +
  geom_line() +
  facet_wrap(~lambda)

ggsave("analyses/classic_MCT/preliminary_equil_abundance/20240219_species_specific/GITR/gitr_eq_abund_L255.png", width = 16, height = 10)


# ACAM ####
run.to.equilibrium.facilitation <- function(surv, germ, lambda, alpha_intra, alpha_intra_sq, Nt, alpha_inter, germ_inter, inter_abund) {
  Ntp1 <- (1-germ)*surv*Nt + germ*lambda*Nt*exp(-alpha_intra*germ*Nt - alpha_intra_sq*germ*(Nt^2) - alpha_inter*germ_inter*inter_abund)
  return(Ntp1)
}


## set up loop
acam_dry <- dry[["ACAM"]]

intra <- "alpha_acam"

post_length <- length(acam_dry$lambda)

acam_dry_pop <- data.frame(lambda = NA, alpha_intra = NA, pop_size = 100, timestep = 0, post_num = NA)

posts <- sample(post_length, 100)


for(reps in 1:length(posts)){
  
  post_num <- posts[reps]
  
  lambda <- gitr_dry$lambda[post_num]
  
  alpha_intra <- gitr_dry$alpha_gitr[post_num]
  
  temp2 <- data.frame(lambda = NA, alpha_intra = NA, pop_size = 100, timestep = 0, post_num = NA)
  
  for(i in 1:100){
    
    timestep <- i
    
    temp <- data.frame(lambda = NA, alpha_intra = NA, pop_size = NA, timestep = NA, post_num = NA)
    
    Nt_x <- run.to.equilibrium(germ = gitr_dry$germ, 
                               surv = gitr_dry$surv,
                               lambda = lambda, 
                               alpha_intra = alpha_intra, 
                               Nt = temp2[temp2$timestep == (i - 1),]$pop_size, 
                               alpha_inter = 0,
                               germ_inter = 0,
                               inter_abund = 0) 
    
    
    temp$lambda <- lambda
    temp$alpha_intra <- alpha_intra
    temp$timestep <- timestep
    temp$pop_size <- Nt_x
    temp$post_num <- post_num
    
    temp2 <- rbind(temp2, temp)
    
  }
  
  gitr_dry_pop <- rbind(gitr_dry_pop, temp2)
  
}


gitr_dry_pop$lambda <- as.integer(gitr_dry_pop$lambda)

ggplot(gitr_dry_pop, aes(x=timestep, y=pop_size, color = alpha_intra)) +
  geom_point() +
  geom_line() +
  facet_wrap(~lambda)

ggplot(gitr_dry_pop[gitr_dry_pop$lambda < 200,], aes(x=timestep, y=pop_size, color = alpha_intra)) +
  geom_point() +
  geom_line() +
  facet_wrap(~lambda)

ggsave("analyses/classic_MCT/preliminary_equil_abundance/20240219_species_specific/GITR/gitr_eq_abund_L200.png", width = 16, height = 10)

ggplot(gitr_dry_pop[gitr_dry_pop$lambda > 200 & gitr_dry_pop$lambda < 255,], aes(x=timestep, y=pop_size, color = alpha_intra)) +
  geom_point() +
  geom_line() +
  facet_wrap(~lambda)

ggsave("analyses/classic_MCT/preliminary_equil_abundance/20240219_species_specific/GITR/gitr_eq_abund_L200_255.png", width = 16, height = 10)

ggplot(gitr_dry_pop[gitr_dry_pop$lambda > 255,], aes(x=timestep, y=pop_size, color = alpha_intra)) +
  geom_point() +
  geom_line() +
  facet_wrap(~lambda)

ggsave("analyses/classic_MCT/preliminary_equil_abundance/20240219_species_specific/GITR/gitr_eq_abund_L255.png", width = 16, height = 10)











