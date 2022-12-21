# Run experimental data with fecundity model
# use knowledge of species to diagnose species and models - we know how many seeds they produce so we know what we should expect; lambdas might be huge but competition coefficients are equally inflated because they are having proportionally - this works for modeling coexistence and partitioning because the scale is correct even if this individual lambdas or alphas are technically incorrect

# weed term as stem coefficient ; we dont care about seeds out so DONT process
# look at really weedy plots to get estimates to use as priors for other models

### Prep ####
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

### BRHO ####
#### control ####

dat <- subset(model.dat, phyto == "BRHO")
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

seeds_brho_c <- stan(file = "Models/eight_species_BH_model_c.stan", data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba"),
                     iter = 1000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) # adapt delta controls how quikcly you move through parameter space, closer to one the smaller those steps, so if having convergence issures set closer to one but might also need more iteractions because it slows down fitting process

plot(seeds_brho_c)
print(seeds_brho_c)
pairs(seeds_brho_c)
stan_dens(seeds_brho_c)

save(seeds_brho_c, file = "Models/Posteriors/seeds_brho_c_posteriors.rdata")

#### drought ####

dat <- subset(model.dat, phyto == "BRHO")
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

seeds_brho_d <- stan(file = "Models/eight_species_BH_model_d.stan", data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba"),
                     iter = 1000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) # adapt delta controls how quikcly you move through parameter space, closer to one the smaller those steps, so if having convergence issures set closer to one but might also need more iteractions because it slows down fitting process

plot(seeds_brho_d)
print(seeds_brho_d)
pairs(seeds_brho_d)
stan_dens(seeds_brho_d)

save(seeds_brho_d, file = "Models/Posteriors/seeds_brho_d_posteriors.rdata")

### PLER ####
#### control ####

dat <- subset(model.dat, phyto == "PLER")
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

seeds_pler_c <- stan(file = "Models/eight_species_BH_model_c.stan", data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba"),
                     iter = 1000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) 

print(seeds_pler_c)
pairs(seeds_pler_c)
stan_dens(seeds_pler_c)

save(seeds_pler_c, file = "Models/Posteriors/seeds_pler_c_posteriors.rdata")

#### drought ####

dat <- subset(model.dat, phyto == "PLER")
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

seeds_pler_d <- stan(file = "Models/eight_species_BH_model_c.stan", data = c("N", "Fecundity", "intra", "intra_g", "pler", "anar", "acam", "brni","clpu","brho","gitr","amme","plno","thir","mica","ceso","twil","lomu","taca","mael","leni", "avba"),
                     iter = 1000, chains = 4, thin = 3, control = list(adapt_delta = 0.95, max_treedepth = 20),
                     init = initials1) 

plot(seeds_pler_d)
print(seeds_pler_d)
stan_dens(seeds_pler_d)

save(seeds_pler_d, file = "Models/Posteriors/seeds_pler_d_posteriors.rdata")

