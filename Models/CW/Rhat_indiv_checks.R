
## A more in-depth look at model Rhat values

# ACAM ####
## Control ####
load("models/CW/posteriors/seeds_ACAM_C_posteriors_upLprior_weeds.rdata")
print(tmp)

## all Rhat vals = 1

## Drought ####
load("models/CW/posteriors/seeds_ACAM_D_posteriors_upLprior_weeds.rdata")
print(tmp)
## all Rhat vals = 1

# AMME ####
## Control ####
load("models/CW/posteriors/seeds_AMME_C_posteriors_upLprior_weeds.rdata")
print(tmp)

## all Rhat vals = 1

## Drought ####
load("models/CW/posteriors/seeds_AMME_D_posteriors_upLprior_weeds.rdata")
print(tmp)
## all Rhat vals = 1


# ANAR ####
## Control ####
load("models/CW/posteriors/seeds_ANAR_C_posteriors_upLprior_weeds.rdata")
print(tmp)

## all Rhat vals = 1

## Drought ####
load("models/CW/posteriors/seeds_ANAR_D_posteriors_upLprior_weeds.rdata")
print(tmp)
## all Rhat vals = 1
## still a very high alpha_twil value

# AVBA ####
## Control ####
load("models/CW/posteriors/seeds_AVBA_C_posteriors_upLprior_weeds.rdata")
print(tmp)

## all Rhat vals = 1

## Drought ####
load("models/CW/posteriors/seeds_AVBA_D_posteriors_upLprior_weeds.rdata")
print(tmp)
## all Rhat vals = 1


# BRHO ####
## Control ####
load("models/CW/posteriors/seeds_BRHO_C_posteriors_upLprior_weeds.rdata")
print(tmp)

## all Rhat vals = 1

## Drought ####
load("models/CW/posteriors/seeds_BRHO_D_posteriors_upLprior_weeds.rdata")
print(tmp)
## all Rhat vals = 1


# BRNI ####
## Control ####
load("models/CW/posteriors/seeds_BRNI_C_posteriors_upLprior_weeds.rdata")
print(tmp)

## all Rhat vals = 1

## Drought ####
load("models/CW/posteriors/seeds_BRNI_D_posteriors_upLprior_weeds.rdata")
print(tmp)
## all Rhat vals = 1
## a few very high alphas - AMME, PLNO, AVBA - these are not high in the controls... wonder what is going on here


# CESO ####
## Control ####
load("models/CW/posteriors/seeds_CESO_C_posteriors_upLprior_weeds.rdata")
print(tmp)

## all Rhat vals = 1

## Drought ####
load("models/CW/posteriors/seeds_CESO_D_posteriors_upLprior_weeds.rdata")
print(tmp)
## all Rhat vals = 1
## very high alpha_AMME

# CLPU ####
## Control ####
load("models/CW/posteriors/seeds_CLPU_C_posteriors_upLprior_weeds.rdata")
print(tmp)

## all Rhat vals = 1
## several high alphas = PLNO, ERBO, FIGA

## Drought ####
load("models/CW/posteriors/seeds_CLPU_D_posteriors_upLprior_weeds.rdata")
print(tmp)
## all Rhat vals = 1


# GITR ####
## Control ####
load("models/CW/posteriors/seeds_GITR_C_posteriors_upLprior_weeds.rdata")
print(tmp)
## all Rhat vals = 1


## Drought ####
load("models/CW/posteriors/seeds_GITR_D_posteriors_upLprior_weeds.rdata")
print(tmp)
## all Rhat vals = 1
## very high alpha_twil, alpha_clpu


# LENI ####
## Control ####
load("models/CW/posteriors/seeds_LENI_C_posteriors_upLprior_weeds.rdata")
print(tmp)
## all Rhat vals = 1


## Drought ####
load("models/CW/posteriors/seeds_LENI_D_posteriors_upLprior_weeds.rdata")
print(tmp)
## all Rhat vals = 1
## very high alpha_amme, alpha_brni, alpha_twil, alpha_avba, alpha_figa, alpha_ceso


# LOMU ####
## Control ####
load("models/CW/posteriors/seeds_LOMU_C_posteriors_upLprior_weeds.rdata")
print(tmp)
## all Rhat vals = 1
## very high alpha_avba, alpha_erbo, apha_figa

## Drought ####
load("models/CW/posteriors/seeds_LOMU_D_posteriors_upLprior_weeds.rdata")
print(tmp)
### some Rhat = 1.01 ####
## very high alpha_pler, alpha_brni, alpha_clpu, alpha_amme, alpha_twil, alpha_mael, alpha_leni, alpha_avba, alpha_erbo, 
## lower n_eff than most other models so far


# MAEL ####
## Control ####
load("models/CW/posteriors/seeds_MAEL_C_posteriors_upLprior_weeds.rdata")
print(tmp)
## all Rhat vals = 1
## very high alpha_acam, alpha_crco, apha_figa

## Drought ####
load("models/CW/posteriors/seeds_MAEL_D_posteriors_upLprior_weeds.rdata")
print(tmp)
### very high Rhat vals ####
## extremely low n_eff vals


# MICA ####
## Control ####
load("models/CW/posteriors/seeds_MICA_C_posteriors_upLprior_weeds.rdata")
print(tmp)
## all Rhat vals = 1
## high apha_figa

## Drought ####
load("models/CW/posteriors/seeds_MICA_D_posteriors_upLprior_weeds.rdata")
print(tmp)
## all Rhat vals = 1

# PLER ####
## Control ####
load("models/CW/posteriors/seeds_PLER_C_posteriors_upLprior_weeds.rdata")
print(tmp)
## all Rhat vals = 1
## high apha_erbo

## Drought ####
load("models/CW/posteriors/seeds_PLER_D_posteriors_upLprior_weeds.rdata")
print(tmp)
## all Rhat vals = 1

# PLNO ####
## Control ####
load("models/CW/posteriors/seeds_PLNO_C_posteriors_upLprior_weeds.rdata")
print(tmp)
## all Rhat vals = 1
## high apha_erbo

## Drought ####
load("models/CW/posteriors/seeds_PLNO_D_posteriors_upLprior_weeds.rdata")
print(tmp)
## all Rhat vals = 1

# TACA ####
## Control ####
load("models/CW/posteriors/seeds_TACA_C_posteriors_upLprior_weeds.rdata")
print(tmp)
## all Rhat vals = 1
## high apha_brni, thir, ceso, lomu; very high alpha_avba, alpha_erbo, figa

## Drought ####
load("models/CW/posteriors/seeds_TACA_D_posteriors_upLprior_weeds.rdata")
print(tmp)
## all Rhat vals = 1
## very high alpha_amme, twil


# THIR ####
## Control ####
load("models/CW/posteriors/seeds_THIR_C_posteriors_upLprior_weeds.rdata")
print(tmp)
## all Rhat vals = 1


## Drought ####
load("models/CW/posteriors/seeds_THIR_D_posteriors_upLprior_weeds.rdata")
print(tmp)
## all Rhat vals = 1


# TWIL ####
## Control ####
load("models/CW/posteriors/seeds_TWIL_C_posteriors_upLprior_weeds.rdata")
print(tmp)
## all Rhat vals = 1


## Drought ####
load("models/CW/posteriors/seeds_TWIL_D_posteriors_upLprior_weeds.rdata")
print(tmp)
## all Rhat vals = 1

              

## used
#"ACAM", "AMME", "ANAR", "AVBA", "BRHO", "BRNI", "CESO", "CLPU", "GITR", "LENI", "LOMU",  "MAEL", "MICA", "PLER", "PLNO", "TACA", "THIR", "TWIL", 
