# Run experimental data with fecundity model
# use knowledge of species to diagnose species and models - we know how many seeds they produce so we know what we should expect; lambdas might be huge but competition coefficients are equally inflated because they are having proportionally - this works for modeling coexistence and partitioning because the scale is correct even if this individual lambdas or alphas are technically incorrect

# weed term as stem coefficient ; we dont care about seeds out so DONT process
# look at really weedy plots to get estimates to use as priors for other models

# Prep ####
source("data_cleaning/format_model_dat.R")

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library(here)

# things to work out: germination rates, seed survival rates

initials <- list(lambda=250, 
                 alpha_pler=1, 
                 alpha_anar=1, 
                 alpha_acam=1,
                 alpha_brni=1, 
                 alpha_clpu=1, 
                 alpha_brho=1,
                 alpha_gitr=1, 
                 alpha_amme=1, 
                 alpha_plno=1,
                 alpha_thir=1, 
                 alpha_mica=1, 
                 alpha_ceso=1,
                 alpha_twil=1, 
                 alpha_lomu=1, 
                 alpha_taca=1,
                 alpha_mael=1, 
                 alpha_leni=1, 
                 alpha_avba=1)

initials1<- list(initials, initials, initials, initials) 


# ACAM ####
## control ####
dat <- subset(model.dat.filtered, phyto == "ACAM" & bkgrd != "ANAR")
dat <- subset(dat, treatment == "C")

Fecundity <- as.integer(round(dat$phyto.seeds.out.final))

pler <- as.integer(dat$PLER)
anar <- as.integer(dat$ANAR)
acam <- as.integer(dat$ACAM)
brni <- as.integer(dat$BRNI)
clpu <- as.integer(dat$CLPU)
brho <- as.integer(dat$BRHO)
gitr <- as.integer(dat$GITR)
amme <- as.integer(dat$AMME)
plno <- as.integer(dat$PLNO)
thir <- as.integer(dat$THIR)
mica <- as.integer(dat$MICA)
ceso <- as.integer(dat$CESO)
twil <- as.integer(dat$TWIL)
lomu <- as.integer(dat$LOMU)
taca <- as.integer(dat$TACA)
mael <- as.integer(dat$MAEL)
leni <- as.integer(dat$LENI)
avba <- as.integer(dat$AVBA)

N <- as.integer(length(Fecundity))

intra <- acam

intra_g <- 0.66 #hwat germ rate are we using?
#intra_s <- ignoring this for now

seeds_acam_c <- stan(file = "models/CW/models_by_species/ACAM_BH_model_C.stan", data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba"),
                     iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) # adapt delta controls how quikcly you move through parameter space, closer to one the smaller those steps, so if having convergence issures set closer to one but might also need more iteractions because it slows down fitting process

plot(seeds_acam_c)
print(seeds_acam_c)
pairs(seeds_acam_c)
stan_dens(seeds_acam_c)

save(seeds_acam_c, file = "models/CW/models_by_species/posteriors/seeds_acam_C_posteriors_constrainedL.rdata")

## drought ####
dat <- subset(model.dat.filtered, phyto == "ACAM" & bkgrd != "ANAR")
dat <- subset(dat, treatment == "D")

Fecundity <- as.integer(round(dat$phyto.seeds.out.final))

pler <- as.integer(dat$PLER)
anar <- as.integer(dat$ANAR)
acam <- as.integer(dat$ACAM)
brni <- as.integer(dat$BRNI)
clpu <- as.integer(dat$CLPU)
brho <- as.integer(dat$BRHO)
gitr <- as.integer(dat$GITR)
amme <- as.integer(dat$AMME)
plno <- as.integer(dat$PLNO)
thir <- as.integer(dat$THIR)
mica <- as.integer(dat$MICA)
ceso <- as.integer(dat$CESO)
twil <- as.integer(dat$TWIL)
lomu <- as.integer(dat$LOMU)
taca <- as.integer(dat$TACA)
mael <- as.integer(dat$MAEL)
leni <- as.integer(dat$LENI)
avba <- as.integer(dat$AVBA)

N <- as.integer(length(Fecundity))

intra <- acam

intra_g <- 0.52 #hwat germ rate are we using?
#intra_s <- ignoring this for now

seeds_acam_d <- stan(file = "models/CW/models_by_species/ACAM_BH_model_D.stan", data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba"),
                     iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) # adapt delta controls how quikcly you move through parameter space, closer to one the smaller those steps, so if having convergence issures set closer to one but might also need more iteractions because it slows down fitting process

plot(seeds_acam_d)
print(seeds_acam_d)
pairs(seeds_acam_d)
stan_dens(seeds_acam_d)

save(seeds_acam_d, file = "models/CW/models_by_species/posteriors/seeds_acam_D_posteriors_constrainedL.rdata")
### NOTE ####
## some distribs don't look normal, tend to have right tails 


# AMME ####
## control ####
dat <- subset(model.dat.filtered, phyto == "AMME" & bkgrd != "ANAR")
dat <- subset(dat, treatment == "C")

Fecundity <- as.integer(round(dat$phyto.seeds.out.final))

pler <- as.integer(dat$PLER)
anar <- as.integer(dat$ANAR)
acam <- as.integer(dat$ACAM)
brni <- as.integer(dat$BRNI)
clpu <- as.integer(dat$CLPU)
brho <- as.integer(dat$BRHO)
gitr <- as.integer(dat$GITR)
amme <- as.integer(dat$AMME)
plno <- as.integer(dat$PLNO)
thir <- as.integer(dat$THIR)
mica <- as.integer(dat$MICA)
ceso <- as.integer(dat$CESO)
twil <- as.integer(dat$TWIL)
lomu <- as.integer(dat$LOMU)
taca <- as.integer(dat$TACA)
mael <- as.integer(dat$MAEL)
leni <- as.integer(dat$LENI)
avba <- as.integer(dat$AVBA)

N <- as.integer(length(Fecundity))

intra <- amme

intra_g <- 0.88 #hwat germ rate are we using?
#intra_s <- ignoring this for now

seeds_amme_c <- stan(file = "models/CW/models_by_species/AMME_BH_model_C.stan", data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba"),
                     iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) # adapt delta controls how quikcly you move through parameter space, closer to one the smaller those steps, so if having convergence issures set closer to one but might also need more iteractions because it slows down fitting process

plot(seeds_amme_c)
print(seeds_amme_c)
pairs(seeds_amme_c)
stan_dens(seeds_amme_c)

save(seeds_amme_c, file = "models/CW/models_by_species/posteriors/seeds_amme_C_posteriors_constrainedL.rdata")

## drought ####
dat <- subset(model.dat.filtered, phyto == "AMME" & bkgrd != "ANAR")
dat <- subset(dat, treatment == "D")

Fecundity <- as.integer(round(dat$phyto.seeds.out.final))

pler <- as.integer(dat$PLER)
anar <- as.integer(dat$ANAR)
acam <- as.integer(dat$ACAM)
brni <- as.integer(dat$BRNI)
clpu <- as.integer(dat$CLPU)
brho <- as.integer(dat$BRHO)
gitr <- as.integer(dat$GITR)
amme <- as.integer(dat$AMME)
plno <- as.integer(dat$PLNO)
thir <- as.integer(dat$THIR)
mica <- as.integer(dat$MICA)
ceso <- as.integer(dat$CESO)
twil <- as.integer(dat$TWIL)
lomu <- as.integer(dat$LOMU)
taca <- as.integer(dat$TACA)
mael <- as.integer(dat$MAEL)
leni <- as.integer(dat$LENI)
avba <- as.integer(dat$AVBA)

N <- as.integer(length(Fecundity))

intra <- amme

intra_g <- 0.14 #hwat germ rate are we using?
#intra_s <- ignoring this for now

seeds_amme_d <- stan(file = "models/CW/models_by_species/AMME_BH_model_D.stan", data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba"),
                     iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) # adapt delta controls how quikcly you move through parameter space, closer to one the smaller those steps, so if having convergence issures set closer to one but might also need more iteractions because it slows down fitting process

plot(seeds_amme_d)
print(seeds_amme_d)
pairs(seeds_amme_d)
stan_dens(seeds_amme_d)

save(seeds_amme_d, file = "models/CW/models_by_species/posteriors/seeds_amme_D_posteriors_constrainedL.rdata")



# ANAR ####
## control ####
dat <- subset(model.dat.filtered, phyto == "ANAR" & bkgrd != "ANAR")
dat <- subset(dat, treatment == "C")

Fecundity <- as.integer(round(dat$phyto.seeds.out.final))

pler <- as.integer(dat$PLER)
anar <- as.integer(dat$ANAR)
acam <- as.integer(dat$ACAM)
brni <- as.integer(dat$BRNI)
clpu <- as.integer(dat$CLPU)
brho <- as.integer(dat$BRHO)
gitr <- as.integer(dat$GITR)
amme <- as.integer(dat$AMME)
plno <- as.integer(dat$PLNO)
thir <- as.integer(dat$THIR)
mica <- as.integer(dat$MICA)
ceso <- as.integer(dat$CESO)
twil <- as.integer(dat$TWIL)
lomu <- as.integer(dat$LOMU)
taca <- as.integer(dat$TACA)
mael <- as.integer(dat$MAEL)
leni <- as.integer(dat$LENI)
avba <- as.integer(dat$AVBA)

N <- as.integer(length(Fecundity))

intra <- anar

intra_g <- 0.15 #hwat germ rate are we using?
#intra_s <- ignoring this for now

seeds_anar_c <- stan(file = "models/CW/models_by_species/ANAR_BH_model_C.stan", data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba"),
                     iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) 

plot(seeds_anar_c)
print(seeds_anar_c)
stan_dens(seeds_anar_c)

save(seeds_anar_c, file = "models/CW/models_by_species/posteriors/seeds_anar_C_posteriors_constrainedL.rdata")

### NOTE #### 
## the intra-alpha is probably not meaningful as we've removed ANAR backgrounds...


## drought ####
dat <- subset(model.dat.filtered, phyto == "ANAR" & bkgrd != "ANAR")
dat <- subset(dat, treatment == "D")

Fecundity <- as.integer(round(dat$phyto.seeds.out.final))

pler <- as.integer(dat$PLER)
anar <- as.integer(dat$ANAR)
acam <- as.integer(dat$ACAM)
brni <- as.integer(dat$BRNI)
clpu <- as.integer(dat$CLPU)
brho <- as.integer(dat$BRHO)
gitr <- as.integer(dat$GITR)
amme <- as.integer(dat$AMME)
plno <- as.integer(dat$PLNO)
thir <- as.integer(dat$THIR)
mica <- as.integer(dat$MICA)
ceso <- as.integer(dat$CESO)
twil <- as.integer(dat$TWIL)
lomu <- as.integer(dat$LOMU)
taca <- as.integer(dat$TACA)
mael <- as.integer(dat$MAEL)
leni <- as.integer(dat$LENI)
avba <- as.integer(dat$AVBA)

N <- as.integer(length(Fecundity))

intra <- anar

intra_g <- 0.07 #hwat germ rate are we using?
#intra_s <- ignoring this for now

seeds_anar_d <- stan(file = "models/CW/models_by_species/ANAR_BH_model_D.stan", data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba"),
                     iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) 

plot(seeds_anar_d)
print(seeds_anar_d)
stan_dens(seeds_anar_d)

save(seeds_anar_d, file = "models/CW/models_by_species/posteriors/seeds_anar_D_posteriors_constrainedL.rdata")

### NOTE: ####
## interesting - when lambda was bounded by 10,000 the distrib was cut off. I have increased the boundary to 15,000 for this model.
## that still didn't help, it was bounded by the 15,000 so I have now removed the upper bound just to see where it goes.
## leaving it unbounded has lambda centered around 16,000

## also, the alpha_anar is still not very meaningful here.
# AVBA ####

# BRHO ####
## control ####

dat <- subset(model.dat.filtered, phyto == "BRHO" & bkgrd != "ANAR")
dat <- subset(dat, treatment == "C")

Fecundity <- as.integer(round(dat$phyto.seeds.out.final))

pler <- as.integer(dat$PLER)
anar <- as.integer(dat$ANAR)
acam <- as.integer(dat$ACAM)
brni <- as.integer(dat$BRNI)
clpu <- as.integer(dat$CLPU)
brho <- as.integer(dat$BRHO)
gitr <- as.integer(dat$GITR)
amme <- as.integer(dat$AMME)
plno <- as.integer(dat$PLNO)
thir <- as.integer(dat$THIR)
mica <- as.integer(dat$MICA)
ceso <- as.integer(dat$CESO)
twil <- as.integer(dat$TWIL)
lomu <- as.integer(dat$LOMU)
taca <- as.integer(dat$TACA)
mael <- as.integer(dat$MAEL)
leni <- as.integer(dat$LENI)
avba <- as.integer(dat$AVBA)

N <- as.integer(length(Fecundity))

intra <- brho

intra_g <- 0.97 #hwat germ rate are we using?
#intra_s <- ignoring this for now

seeds_brho_c <- stan(file = "models/CW/models_by_species/BRHO_BH_model_C.stan", data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba"),
                     iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) # adapt delta controls how quikcly you move through parameter space, closer to one the smaller those steps, so if having convergence issures set closer to one but might also need more iteractions because it slows down fitting process

plot(seeds_brho_c)
print(seeds_brho_c)
pairs(seeds_brho_c)
stan_dens(seeds_brho_c)

save(seeds_brho_c, file = "models/CW/models_by_species/posteriors/seeds_brho_C_posteriors_constrainedL.rdata")

## drought ####

dat <- subset(model.dat.filtered, phyto == "BRHO" & bkgrd != "ANAR")
dat <- subset(dat, treatment == "D")

Fecundity <- as.integer(round(dat$phyto.seeds.out.final))

pler <- as.integer(dat$PLER)
anar <- as.integer(dat$ANAR)
acam <- as.integer(dat$ACAM)
brni <- as.integer(dat$BRNI)
clpu <- as.integer(dat$CLPU)
brho <- as.integer(dat$BRHO)
gitr <- as.integer(dat$GITR)
amme <- as.integer(dat$AMME)
plno <- as.integer(dat$PLNO)
thir <- as.integer(dat$THIR)
mica <- as.integer(dat$MICA)
ceso <- as.integer(dat$CESO)
twil <- as.integer(dat$TWIL)
lomu <- as.integer(dat$LOMU)
taca <- as.integer(dat$TACA)
mael <- as.integer(dat$MAEL)
leni <- as.integer(dat$LENI)
avba <- as.integer(dat$AVBA)

N <- as.integer(length(Fecundity))

intra <- brho

intra_g <- 1 
#intra_s <- ignoring this for now

seeds_brho_d <- stan(file = "models/CW/models_by_species/BRHO_BH_model_D.stan", data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba"),
                     iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) # adapt delta controls how quikcly you move through parameter space, closer to one the smaller those steps, so if having convergence issures set closer to one but might also need more iteractions because it slows down fitting process

plot(seeds_brho_d)
print(seeds_brho_d)
pairs(seeds_brho_d)
stan_dens(seeds_brho_d)

save(seeds_brho_d, file = "models/CW/models_by_species/posteriors/seeds_brho_D_posteriors_constrainedL.rdata")

# BRNI ####

# CESO ####

# CLPU ####

# GITR ####
## control ####
dat <- subset(model.dat.filtered, phyto == "GITR" & bkgrd != "ANAR")
dat <- subset(dat, treatment == "C")

Fecundity <- as.integer(round(dat$phyto.seeds.out.final))

pler <- as.integer(dat$PLER)
anar <- as.integer(dat$ANAR)
acam <- as.integer(dat$ACAM)
brni <- as.integer(dat$BRNI)
clpu <- as.integer(dat$CLPU)
brho <- as.integer(dat$BRHO)
gitr <- as.integer(dat$GITR)
amme <- as.integer(dat$AMME)
plno <- as.integer(dat$PLNO)
thir <- as.integer(dat$THIR)
mica <- as.integer(dat$MICA)
ceso <- as.integer(dat$CESO)
twil <- as.integer(dat$TWIL)
lomu <- as.integer(dat$LOMU)
taca <- as.integer(dat$TACA)
mael <- as.integer(dat$MAEL)
leni <- as.integer(dat$LENI)
avba <- as.integer(dat$AVBA)

N <- as.integer(length(Fecundity))

intra <- gitr

intra_g <- 0.98 #hwat germ rate are we using?
#intra_s <- ignoring this for now

seeds_gitr_c <- stan(file = "models/CW/models_by_species/GITR_BH_model_C.stan", data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba"),
                     iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) 

plot(seeds_gitr_c)
print(seeds_gitr_c)
stan_dens(seeds_gitr_c)

save(seeds_gitr_c, file = "models/CW/models_by_species/posteriors/seeds_gitr_C_posteriors_constrainedL.rdata")


## drought ####
dat <- subset(model.dat.filtered, phyto == "GITR" & bkgrd != "ANAR")
dat <- subset(dat, treatment == "D")

Fecundity <- as.integer(round(dat$phyto.seeds.out.final))

pler <- as.integer(dat$PLER)
anar <- as.integer(dat$ANAR)
acam <- as.integer(dat$ACAM)
brni <- as.integer(dat$BRNI)
clpu <- as.integer(dat$CLPU)
brho <- as.integer(dat$BRHO)
gitr <- as.integer(dat$GITR)
amme <- as.integer(dat$AMME)
plno <- as.integer(dat$PLNO)
thir <- as.integer(dat$THIR)
mica <- as.integer(dat$MICA)
ceso <- as.integer(dat$CESO)
twil <- as.integer(dat$TWIL)
lomu <- as.integer(dat$LOMU)
taca <- as.integer(dat$TACA)
mael <- as.integer(dat$MAEL)
leni <- as.integer(dat$LENI)
avba <- as.integer(dat$AVBA)

N <- as.integer(length(Fecundity))

intra <- gitr

intra_g <- 0.91 #hwat germ rate are we using?
#intra_s <- ignoring this for now

seeds_gitr_d <- stan(file = "models/CW/models_by_species/GITR_BH_model_D.stan", data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba"),
                     iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) 

plot(seeds_gitr_d)
print(seeds_gitr_d)
stan_dens(seeds_gitr_d)

save(seeds_gitr_d, file = "models/CW/models_by_species/posteriors/seeds_gitr_D_posteriors_constrainedL.rdata")


# LENI ####

# LOMU ####
## control ####
dat <- subset(model.dat.filtered, phyto == "LOMU" & bkgrd != "ANAR")
dat <- subset(dat, treatment == "C")

Fecundity <- as.integer(round(dat$phyto.seeds.out.final))

pler <- as.integer(dat$PLER)
anar <- as.integer(dat$ANAR)
acam <- as.integer(dat$ACAM)
brni <- as.integer(dat$BRNI)
clpu <- as.integer(dat$CLPU)
brho <- as.integer(dat$BRHO)
gitr <- as.integer(dat$GITR)
amme <- as.integer(dat$AMME)
plno <- as.integer(dat$PLNO)
thir <- as.integer(dat$THIR)
mica <- as.integer(dat$MICA)
ceso <- as.integer(dat$CESO)
twil <- as.integer(dat$TWIL)
lomu <- as.integer(dat$LOMU)
taca <- as.integer(dat$TACA)
mael <- as.integer(dat$MAEL)
leni <- as.integer(dat$LENI)
avba <- as.integer(dat$AVBA)

N <- as.integer(length(Fecundity))

intra <- lomu

intra_g <- 0.96 #hwat germ rate are we using?
#intra_s <- ignoring this for now

seeds_lomu_c <- stan(file = "models/CW/models_by_species/LOMU_BH_model_C.stan", data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba"),
                     iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) 

plot(seeds_lomu_c)
print(seeds_lomu_c)
stan_dens(seeds_lomu_c)

save(seeds_lomu_c, file = "models/CW/models_by_species/posteriors/seeds_lomu_C_posteriors_constrainedL.rdata")


## drought ####
dat <- subset(model.dat.filtered, phyto == "LOMU" & bkgrd != "ANAR")
dat <- subset(dat, treatment == "D")

Fecundity <- as.integer(round(dat$phyto.seeds.out.final))

pler <- as.integer(dat$PLER)
anar <- as.integer(dat$ANAR)
acam <- as.integer(dat$ACAM)
brni <- as.integer(dat$BRNI)
clpu <- as.integer(dat$CLPU)
brho <- as.integer(dat$BRHO)
gitr <- as.integer(dat$GITR)
amme <- as.integer(dat$AMME)
plno <- as.integer(dat$PLNO)
thir <- as.integer(dat$THIR)
mica <- as.integer(dat$MICA)
ceso <- as.integer(dat$CESO)
twil <- as.integer(dat$TWIL)
lomu <- as.integer(dat$LOMU)
taca <- as.integer(dat$TACA)
mael <- as.integer(dat$MAEL)
leni <- as.integer(dat$LENI)
avba <- as.integer(dat$AVBA)

N <- as.integer(length(Fecundity))

intra <- lomu

intra_g <- 0.87 #hwat germ rate are we using?
#intra_s <- ignoring this for now

seeds_lomu_d <- stan(file = "models/CW/models_by_species/LOMU_BH_model_D.stan", data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba"),
                     iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) 

plot(seeds_lomu_d)
print(seeds_lomu_d)
stan_dens(seeds_lomu_d)

save(seeds_lomu_d, file = "models/CW/models_by_species/posteriors/seeds_lomu_D_posteriors_constrainedL.rdata")



# MAEL ####
## control ####
dat <- subset(model.dat.filtered, phyto == "MAEL" & bkgrd != "ANAR")
dat <- subset(dat, treatment == "C")

Fecundity <- as.integer(round(dat$phyto.seeds.out.final))

pler <- as.integer(dat$PLER)
anar <- as.integer(dat$ANAR)
acam <- as.integer(dat$ACAM)
brni <- as.integer(dat$BRNI)
clpu <- as.integer(dat$CLPU)
brho <- as.integer(dat$BRHO)
gitr <- as.integer(dat$GITR)
amme <- as.integer(dat$AMME)
plno <- as.integer(dat$PLNO)
thir <- as.integer(dat$THIR)
mica <- as.integer(dat$MICA)
ceso <- as.integer(dat$CESO)
twil <- as.integer(dat$TWIL)
lomu <- as.integer(dat$LOMU)
taca <- as.integer(dat$TACA)
mael <- as.integer(dat$MAEL)
leni <- as.integer(dat$LENI)
avba <- as.integer(dat$AVBA)

N <- as.integer(length(Fecundity))

intra <- mael

intra_g <- 0.59 #hwat germ rate are we using?
#intra_s <- ignoring this for now

seeds_mael_c <- stan(file = "models/CW/18_species_BH_model_updated_Lprior_weeds_C.stan", data = c("N", "Fecundity", "intra", "intra_g", "mean_ctrl_seeds", "sd_ctrl_seeds", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba", "crco", "erbo", "figa", "gamu", "hygl", "siga"),
                     iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) 

print(seeds_pler_c)
pairs(seeds_pler_c)
stan_dens(seeds_pler_c)

save(seeds_pler_c, file = "models/CW/models_by_species/posteriors/seeds_pler_C_posteriors_constrainedL.rdata")

## drought ####

dat <- subset(model.dat.filtered, phyto == "PLER" & bkgrd != "ANAR")
dat <- subset(dat, treatment == "D")

Fecundity <- as.integer(round(dat$phyto.seeds.out.final))

pler <- as.integer(dat$PLER)
anar <- as.integer(dat$ANAR)
acam <- as.integer(dat$ACAM)
brni <- as.integer(dat$BRNI)
clpu <- as.integer(dat$CLPU)
brho <- as.integer(dat$BRHO)
gitr <- as.integer(dat$GITR)
amme <- as.integer(dat$AMME)
plno <- as.integer(dat$PLNO)
thir <- as.integer(dat$THIR)
mica <- as.integer(dat$MICA)
ceso <- as.integer(dat$CESO)
twil <- as.integer(dat$TWIL)
lomu <- as.integer(dat$LOMU)
taca <- as.integer(dat$TACA)
mael <- as.integer(dat$MAEL)
leni <- as.integer(dat$LENI)
avba <- as.integer(dat$AVBA)

N <- as.integer(length(Fecundity))

intra <- pler

intra_g <- 0.53 #hwat germ rate are we using?
#intra_s <- ignoring this for now

seeds_pler_d <- stan(file = "models/CW/models_by_species/PLER_BH_model_D.stan", data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba"),
                     iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) 

plot(seeds_pler_d)
print(seeds_pler_d)
stan_dens(seeds_pler_d)

save(seeds_pler_d, file = "models/CW/models_by_species/posteriors/seeds_pler_D_posteriors_constrainedL.rdata")





# MICA ####


# PLER ####
## control ####

dat <- subset(model.dat.filtered, phyto == "PLER" & bkgrd != "ANAR")
dat <- subset(dat, treatment == "C")

Fecundity <- as.integer(round(dat$phyto.seeds.out.final))

pler <- as.integer(dat$PLER)
anar <- as.integer(dat$ANAR)
acam <- as.integer(dat$ACAM)
brni <- as.integer(dat$BRNI)
clpu <- as.integer(dat$CLPU)
brho <- as.integer(dat$BRHO)
gitr <- as.integer(dat$GITR)
amme <- as.integer(dat$AMME)
plno <- as.integer(dat$PLNO)
thir <- as.integer(dat$THIR)
mica <- as.integer(dat$MICA)
ceso <- as.integer(dat$CESO)
twil <- as.integer(dat$TWIL)
lomu <- as.integer(dat$LOMU)
taca <- as.integer(dat$TACA)
mael <- as.integer(dat$MAEL)
leni <- as.integer(dat$LENI)
avba <- as.integer(dat$AVBA)

N <- as.integer(length(Fecundity))

intra <- pler

intra_g <- 0.8 #hwat germ rate are we using?
#intra_s <- ignoring this for now

seeds_pler_c <- stan(file = "models/CW/models_by_species/PLER_BH_model_C.stan", data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba"),
                     iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) 

print(seeds_pler_c)
pairs(seeds_pler_c)
stan_dens(seeds_pler_c)

save(seeds_pler_c, file = "models/CW/models_by_species/posteriors/seeds_pler_C_posteriors_constrainedL.rdata")

## drought ####

dat <- subset(model.dat.filtered, phyto == "PLER" & bkgrd != "ANAR")
dat <- subset(dat, treatment == "D")

Fecundity <- as.integer(round(dat$phyto.seeds.out.final))

pler <- as.integer(dat$PLER)
anar <- as.integer(dat$ANAR)
acam <- as.integer(dat$ACAM)
brni <- as.integer(dat$BRNI)
clpu <- as.integer(dat$CLPU)
brho <- as.integer(dat$BRHO)
gitr <- as.integer(dat$GITR)
amme <- as.integer(dat$AMME)
plno <- as.integer(dat$PLNO)
thir <- as.integer(dat$THIR)
mica <- as.integer(dat$MICA)
ceso <- as.integer(dat$CESO)
twil <- as.integer(dat$TWIL)
lomu <- as.integer(dat$LOMU)
taca <- as.integer(dat$TACA)
mael <- as.integer(dat$MAEL)
leni <- as.integer(dat$LENI)
avba <- as.integer(dat$AVBA)

N <- as.integer(length(Fecundity))

intra <- pler

intra_g <- 0.53 #hwat germ rate are we using?
#intra_s <- ignoring this for now

seeds_pler_d <- stan(file = "models/CW/models_by_species/PLER_BH_model_D.stan", data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba"),
                     iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) 

plot(seeds_pler_d)
print(seeds_pler_d)
stan_dens(seeds_pler_d)

save(seeds_pler_d, file = "models/CW/models_by_species/posteriors/seeds_pler_D_posteriors_constrainedL.rdata")





# PLNO ####
## control ####
dat <- subset(model.dat.filtered, phyto == "PLNO" & bkgrd != "ANAR")
dat <- subset(dat, treatment == "C")

Fecundity <- as.integer(round(dat$phyto.seeds.out.final))

pler <- as.integer(dat$PLER)
anar <- as.integer(dat$ANAR)
acam <- as.integer(dat$ACAM)
brni <- as.integer(dat$BRNI)
clpu <- as.integer(dat$CLPU)
brho <- as.integer(dat$BRHO)
gitr <- as.integer(dat$GITR)
amme <- as.integer(dat$AMME)
plno <- as.integer(dat$PLNO)
thir <- as.integer(dat$THIR)
mica <- as.integer(dat$MICA)
ceso <- as.integer(dat$CESO)
twil <- as.integer(dat$TWIL)
lomu <- as.integer(dat$LOMU)
taca <- as.integer(dat$TACA)
mael <- as.integer(dat$MAEL)
leni <- as.integer(dat$LENI)
avba <- as.integer(dat$AVBA)

N <- as.integer(length(Fecundity))

intra <- plno

intra_g <- 0.66 #hwat germ rate are we using?
#intra_s <- ignoring this for now

seeds_plno_c <- stan(file = "models/CW/models_by_species/PLNO_BH_model_C.stan", data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba"),
                     iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) 

plot(seeds_plno_c)
print(seeds_plno_c)
stan_dens(seeds_plno_c)

save(seeds_plno_c, file = "models/CW/models_by_species/posteriors/seeds_plno_C_posteriors_constrainedL.rdata")


## drought ####
dat <- subset(model.dat.filtered, phyto == "PLNO" & bkgrd != "ANAR")
dat <- subset(dat, treatment == "D")

Fecundity <- as.integer(round(dat$phyto.seeds.out.final))

pler <- as.integer(dat$PLER)
anar <- as.integer(dat$ANAR)
acam <- as.integer(dat$ACAM)
brni <- as.integer(dat$BRNI)
clpu <- as.integer(dat$CLPU)
brho <- as.integer(dat$BRHO)
gitr <- as.integer(dat$GITR)
amme <- as.integer(dat$AMME)
plno <- as.integer(dat$PLNO)
thir <- as.integer(dat$THIR)
mica <- as.integer(dat$MICA)
ceso <- as.integer(dat$CESO)
twil <- as.integer(dat$TWIL)
lomu <- as.integer(dat$LOMU)
taca <- as.integer(dat$TACA)
mael <- as.integer(dat$MAEL)
leni <- as.integer(dat$LENI)
avba <- as.integer(dat$AVBA)

N <- as.integer(length(Fecundity))

intra <- plno

intra_g <- 0.35 #hwat germ rate are we using?
#intra_s <- ignoring this for now

seeds_plno_d <- stan(file = "models/CW/models_by_species/PLNO_BH_model_D.stan", data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba"),
                     iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) 

plot(seeds_plno_d)
print(seeds_plno_d)
stan_dens(seeds_plno_d)

save(seeds_plno_d, file = "models/CW/models_by_species/posteriors/seeds_plno_D_posteriors_constrainedL.rdata")






# TACA ####
## control ####
dat <- subset(model.dat.filtered, phyto == "TACA" & bkgrd != "ANAR")
dat <- subset(dat, treatment == "C")

Fecundity <- as.integer(round(dat$phyto.seeds.out.final))

pler <- as.integer(dat$PLER)
anar <- as.integer(dat$ANAR)
acam <- as.integer(dat$ACAM)
brni <- as.integer(dat$BRNI)
clpu <- as.integer(dat$CLPU)
brho <- as.integer(dat$BRHO)
gitr <- as.integer(dat$GITR)
amme <- as.integer(dat$AMME)
plno <- as.integer(dat$PLNO)
thir <- as.integer(dat$THIR)
mica <- as.integer(dat$MICA)
ceso <- as.integer(dat$CESO)
twil <- as.integer(dat$TWIL)
lomu <- as.integer(dat$LOMU)
taca <- as.integer(dat$TACA)
mael <- as.integer(dat$MAEL)
leni <- as.integer(dat$LENI)
avba <- as.integer(dat$AVBA)

N <- as.integer(length(Fecundity))

intra <- taca

intra_g <- 0.87 #hwat germ rate are we using?
#intra_s <- ignoring this for now

seeds_taca_c <- stan(file = "models/CW/models_by_species/TACA_BH_model_C.stan", data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba"),
                     iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) 

plot(seeds_taca_c)
print(seeds_taca_c)
stan_dens(seeds_taca_c)

save(seeds_taca_c, file = "models/CW/models_by_species/posteriors/seeds_taca_C_posteriors_constrainedL.rdata")


## drought ####
dat <- subset(model.dat.filtered, phyto == "TACA" & bkgrd != "ANAR")
dat <- subset(dat, treatment == "D")

Fecundity <- as.integer(round(dat$phyto.seeds.out.final))

pler <- as.integer(dat$PLER)
anar <- as.integer(dat$ANAR)
acam <- as.integer(dat$ACAM)
brni <- as.integer(dat$BRNI)
clpu <- as.integer(dat$CLPU)
brho <- as.integer(dat$BRHO)
gitr <- as.integer(dat$GITR)
amme <- as.integer(dat$AMME)
plno <- as.integer(dat$PLNO)
thir <- as.integer(dat$THIR)
mica <- as.integer(dat$MICA)
ceso <- as.integer(dat$CESO)
twil <- as.integer(dat$TWIL)
lomu <- as.integer(dat$LOMU)
taca <- as.integer(dat$TACA)
mael <- as.integer(dat$MAEL)
leni <- as.integer(dat$LENI)
avba <- as.integer(dat$AVBA)

N <- as.integer(length(Fecundity))

intra <- taca

intra_g <- 0.86 #hwat germ rate are we using?
#intra_s <- ignoring this for now

seeds_taca_d <- stan(file = "models/CW/models_by_species/TACA_BH_model_D.stan", data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba"),
                     iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) 

plot(seeds_taca_d)
print(seeds_taca_d)
stan_dens(seeds_taca_d)

save(seeds_taca_d, file = "models/CW/models_by_species/posteriors/seeds_taca_D_posteriors_constrainedL.rdata")


# THIR ####
## control ####
dat <- subset(model.dat.filtered, phyto == "THIR" & bkgrd != "ANAR")
dat <- subset(dat, treatment == "C")

Fecundity <- as.integer(round(dat$phyto.seeds.out.final))

pler <- as.integer(dat$PLER)
anar <- as.integer(dat$ANAR)
acam <- as.integer(dat$ACAM)
brni <- as.integer(dat$BRNI)
clpu <- as.integer(dat$CLPU)
brho <- as.integer(dat$BRHO)
gitr <- as.integer(dat$GITR)
amme <- as.integer(dat$AMME)
plno <- as.integer(dat$PLNO)
thir <- as.integer(dat$THIR)
mica <- as.integer(dat$MICA)
ceso <- as.integer(dat$CESO)
twil <- as.integer(dat$TWIL)
lomu <- as.integer(dat$LOMU)
taca <- as.integer(dat$TACA)
mael <- as.integer(dat$MAEL)
leni <- as.integer(dat$LENI)
avba <- as.integer(dat$AVBA)

N <- as.integer(length(Fecundity))

intra <- thir

intra_g <- 0.79 #hwat germ rate are we using?
#intra_s <- ignoring this for now

seeds_thir_c <- stan(file = "models/CW/models_by_species/THIR_BH_model_C.stan", data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba"),
                     iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) 

plot(seeds_thir_c)
print(seeds_thir_c)
stan_dens(seeds_thir_c)

save(seeds_thir_c, file = "models/CW/models_by_species/posteriors/seeds_thir_C_posteriors_constrainedL.rdata")


## drought ####
dat <- subset(model.dat.filtered, phyto == "THIR" & bkgrd != "ANAR")
dat <- subset(dat, treatment == "D")

Fecundity <- as.integer(round(dat$phyto.seeds.out.final))

pler <- as.integer(dat$PLER)
anar <- as.integer(dat$ANAR)
acam <- as.integer(dat$ACAM)
brni <- as.integer(dat$BRNI)
clpu <- as.integer(dat$CLPU)
brho <- as.integer(dat$BRHO)
gitr <- as.integer(dat$GITR)
amme <- as.integer(dat$AMME)
plno <- as.integer(dat$PLNO)
thir <- as.integer(dat$THIR)
mica <- as.integer(dat$MICA)
ceso <- as.integer(dat$CESO)
twil <- as.integer(dat$TWIL)
lomu <- as.integer(dat$LOMU)
taca <- as.integer(dat$TACA)
mael <- as.integer(dat$MAEL)
leni <- as.integer(dat$LENI)
avba <- as.integer(dat$AVBA)

N <- as.integer(length(Fecundity))

intra <- thir

intra_g <- 0.38 #hwat germ rate are we using?
#intra_s <- ignoring this for now

seeds_thir_d <- stan(file = "models/CW/models_by_species/THIR_BH_model_D.stan", data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba"),
                     iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) 

plot(seeds_thir_d)
print(seeds_thir_d)
stan_dens(seeds_thir_d)

save(seeds_thir_d, file = "models/CW/models_by_species/posteriors/seeds_thir_D_posteriors_constrainedL.rdata")


# TWIL ####
## control ####
dat <- subset(model.dat.filtered, phyto == "TWIL" & bkgrd != "ANAR")
dat <- subset(dat, treatment == "C")

Fecundity <- as.integer(round(dat$phyto.seeds.out.final))

pler <- as.integer(dat$PLER)
anar <- as.integer(dat$ANAR)
acam <- as.integer(dat$ACAM)
brni <- as.integer(dat$BRNI)
clpu <- as.integer(dat$CLPU)
brho <- as.integer(dat$BRHO)
gitr <- as.integer(dat$GITR)
amme <- as.integer(dat$AMME)
plno <- as.integer(dat$PLNO)
thir <- as.integer(dat$THIR)
mica <- as.integer(dat$MICA)
ceso <- as.integer(dat$CESO)
twil <- as.integer(dat$TWIL)
lomu <- as.integer(dat$LOMU)
taca <- as.integer(dat$TACA)
mael <- as.integer(dat$MAEL)
leni <- as.integer(dat$LENI)
avba <- as.integer(dat$AVBA)

N <- as.integer(length(Fecundity))

intra <- twil

intra_g <- 0.44 #hwat germ rate are we using?
#intra_s <- ignoring this for now

seeds_twil_c <- stan(file = "models/CW/models_by_species/TWIL_BH_model_C.stan", data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba"),
                     iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) 

plot(seeds_twil_c)
print(seeds_twil_c)
stan_dens(seeds_twil_c)

save(seeds_twil_c, file = "models/CW/models_by_species/posteriors/seeds_twil_C_posteriors_constrainedL.rdata")


## drought ####
dat <- subset(model.dat.filtered, phyto == "TWIL" & bkgrd != "ANAR")
dat <- subset(dat, treatment == "D")

Fecundity <- as.integer(round(dat$phyto.seeds.out.final))

pler <- as.integer(dat$PLER)
anar <- as.integer(dat$ANAR)
acam <- as.integer(dat$ACAM)
brni <- as.integer(dat$BRNI)
clpu <- as.integer(dat$CLPU)
brho <- as.integer(dat$BRHO)
gitr <- as.integer(dat$GITR)
amme <- as.integer(dat$AMME)
plno <- as.integer(dat$PLNO)
thir <- as.integer(dat$THIR)
mica <- as.integer(dat$MICA)
ceso <- as.integer(dat$CESO)
twil <- as.integer(dat$TWIL)
lomu <- as.integer(dat$LOMU)
taca <- as.integer(dat$TACA)
mael <- as.integer(dat$MAEL)
leni <- as.integer(dat$LENI)
avba <- as.integer(dat$AVBA)

N <- as.integer(length(Fecundity))

intra <- twil

intra_g <- 0.02 #hwat germ rate are we using?
#intra_s <- ignoring this for now

seeds_twil_d <- stan(file = "models/CW/models_by_species/TWIL_BH_model_D.stan", data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba"),
                     iter = 5000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) 

plot(seeds_twil_d)
print(seeds_twil_d)
stan_dens(seeds_twil_d)

save(seeds_twil_d, file = "models/CW/models_by_species/posteriors/seeds_twil_D_posteriors_constrainedL.rdata")



