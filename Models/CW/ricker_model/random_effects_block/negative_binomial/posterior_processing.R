## load models
source("Models/CW/ricker_model/random_effects_block/negative_binomial/import_ricker_posteriors_neg_binom.R")

## separate dry and wet parameters into two lists
## do post-processing - i.e. add base and dev terms for wet treatment

## make empty lists
dry <- list()
wet <- list()

## use a for-loop to separate terms for each species
for(i in 1:length(names(ricker_posteriors))) {
  
  ## separate out a species
  datset <- ricker_posteriors[[i]]
  
  ## dry 
  ## make empty temp list
  tmp.dry <- list()
  
  ## fill empty temp list
  tmp.dry$lambda <- datset$lambda_base
  tmp.dry$alpha_acam <- datset$alpha_acam_base
  tmp.dry$alpha_amme <- datset$alpha_amme_base
  tmp.dry$alpha_anar <- datset$alpha_anar_base
  tmp.dry$alpha_brho <- datset$alpha_brho_base
  tmp.dry$alpha_brni <- datset$alpha_brni_base
  tmp.dry$alpha_ceso <- datset$alpha_ceso_base
  tmp.dry$alpha_gitr <- datset$alpha_gitr_base
  tmp.dry$alpha_leni <- datset$alpha_leni_base
  tmp.dry$alpha_lomu <- datset$alpha_lomu_base
  tmp.dry$alpha_mael <- datset$alpha_mael_base
  tmp.dry$alpha_mica <- datset$alpha_mica_base
  tmp.dry$alpha_pler <- datset$alpha_pler_base
  tmp.dry$alpha_plno <- datset$alpha_plno_base
  tmp.dry$alpha_taca <- datset$alpha_taca_base
  tmp.dry$alpha_thir <- datset$alpha_thir_base
  tmp.dry$alpha_twil <- datset$alpha_twil_base
  
  ## add list back into dry list under a species name
  dry[[paste0(names(ricker_posteriors))[i]]] <- tmp.dry
  
  ## wet 
  tmp.wet <- list()
  
  tmp.wet$lambda <- datset$lambda_base + datset$lambda_dev
  tmp.wet$alpha_acam <- datset$alpha_acam_base + datset$alpha_acam_dev
  tmp.wet$alpha_amme <- datset$alpha_amme_base + datset$alpha_amme_dev
  tmp.wet$alpha_anar <- datset$alpha_anar_base + datset$alpha_anar_dev
  tmp.wet$alpha_brho <- datset$alpha_brho_base + datset$alpha_brho_base
  tmp.wet$alpha_brni <- datset$alpha_brni_base + datset$alpha_brni_base
  tmp.wet$alpha_ceso <- datset$alpha_ceso_base + datset$alpha_ceso_base
  tmp.wet$alpha_gitr <- datset$alpha_gitr_base + datset$alpha_gitr_base
  tmp.wet$alpha_leni <- datset$alpha_leni_base + datset$alpha_leni_base
  tmp.wet$alpha_lomu <- datset$alpha_lomu_base + datset$alpha_lomu_base
  tmp.wet$alpha_mael <- datset$alpha_mael_base + datset$alpha_mael_base
  tmp.wet$alpha_mica <- datset$alpha_mica_base + datset$alpha_mica_base
  tmp.wet$alpha_pler <- datset$alpha_pler_base + datset$alpha_pler_base
  tmp.wet$alpha_plno <- datset$alpha_plno_base + datset$alpha_plno_base
  tmp.wet$alpha_taca <- datset$alpha_taca_base + datset$alpha_taca_base
  tmp.wet$alpha_thir <- datset$alpha_thir_base + datset$alpha_thir_base
  tmp.wet$alpha_twil <- datset$alpha_twil_base + datset$alpha_twil_base
  
  wet[[paste0(names(ricker_posteriors))[i]]] <- tmp.wet
  
}


