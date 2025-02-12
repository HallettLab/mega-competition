// Ricker growth model
// Model as a negative binomial
// Incorporate random effects of blocks
// Models both precip treatments together

data{
  
  int<lower = 1> N; // number of observations
  int Fecundity[N]; // observed fecundity count at time t+1
  
  // add in random effects
  int<lower=1> N_blocks; // Number of groups
  int Blocks[N]; // block column
  
  vector[N] N_i; // population size of species i at time t
  vector[N] trt; // precip treatment

// population sizes of interacting species at time t
  vector[N] acam;
  vector[N] amme;
  vector[N] anar;
  vector[N] brho;
  vector[N] brni;
  vector[N] ceso;
  vector[N] gitr;
  vector[N] leni;
  vector[N] lomu;
  vector[N] mael;
  vector[N] mica;
  vector[N] pler;
  vector[N] plno;
  vector[N] taca;
  vector[N] thir;
  vector[N] twil;
  vector[N] weeds;

}


parameters{
  
  // group-level random effects
  real epsilon[N_blocks]; 
  real<lower = 0> sigma;
  
  // dispersion deviation parameter 
  real<lower=0> disp_dev; 

  // lambda
  real<lower = 0, upper = 15000> lambda_base;
  real lambda_dev;
  
  // all the alphas
   real alpha_acam_base;
   real alpha_amme_base;
   real alpha_anar_base;
   real alpha_brho_base;
   real alpha_brni_base;
   real alpha_ceso_base;
   real alpha_gitr_base;
   real alpha_leni_base;
   real alpha_lomu_base;
   real alpha_mael_base;
   real alpha_mica_base;
   real alpha_pler_base;
   real alpha_plno_base;
   real alpha_taca_base;
   real alpha_thir_base;
   real alpha_twil_base;
   real alpha_weeds_base;
   
   // alpha deviations
   real alpha_acam_dev;
   real alpha_amme_dev;
   real alpha_anar_dev;
   real alpha_brho_dev;
   real alpha_brni_dev;
   real alpha_ceso_dev;
   real alpha_gitr_dev;
   real alpha_leni_dev;
   real alpha_lomu_dev;
   real alpha_mael_dev;
   real alpha_mica_dev;
   real alpha_pler_dev;
   real alpha_plno_dev;
   real alpha_taca_dev;
   real alpha_thir_dev;
   real alpha_twil_dev;
   real alpha_weeds_dev;

}

model{
  
  // create vector of predictions
  vector[N] F_hat;
  vector[N] F_hat2;
  
  // set priors
  sigma ~ gamma(1, 1);
  epsilon ~ gamma(sigma, sigma); // prior for group level random effects
  
  lambda_base ~ exponential(0.0009); // exponential distrib prior
  lambda_dev ~ normal(0, 100);

  alpha_acam_base ~ normal(0, 5);
  alpha_acam_dev ~ normal(0,5);
  alpha_amme_base ~ normal(0, 5);
  alpha_amme_dev ~ normal(0, 5);
  alpha_anar_base ~ normal(0, 5);
  alpha_anar_dev ~ normal(0, 5);
  alpha_brho_base ~ normal(0, 5);
  alpha_brho_dev ~ normal(0, 5);
  alpha_brni_base ~ normal(0, 5);
  alpha_brni_dev ~ normal(0, 5); 
  alpha_ceso_base ~ normal(0, 5);
  alpha_ceso_dev ~ normal(0, 5);
  alpha_gitr_base ~ normal(0, 5);
  alpha_gitr_dev ~ normal(0, 5);
  alpha_leni_base ~ normal(0, 5);
  alpha_leni_dev ~ normal(0, 5);
  alpha_lomu_base ~ normal(0, 5);
  alpha_lomu_dev ~ normal(0, 5);
  alpha_mael_base ~ normal(0, 5);
  alpha_mael_dev ~ normal(0, 5);
  alpha_mica_base ~ normal(0, 5);
  alpha_mica_dev ~ normal(0, 5);
  alpha_pler_base ~ normal(0, 5);
  alpha_pler_dev ~ normal(0, 5);
  alpha_plno_base ~ normal(0, 5);
  alpha_plno_dev ~ normal(0, 5);
  alpha_taca_base ~ normal(0, 5);
  alpha_taca_dev ~ normal(0, 5);
  alpha_thir_base ~ normal(0, 5);
  alpha_thir_dev ~ normal(0, 5);
  alpha_twil_base ~ normal(0, 5);
  alpha_twil_dev ~ normal(0, 5);
  alpha_weeds_base ~ normal(0,5);
  alpha_weeds_dev ~ normal(0,5);
  
  // Biological model
  for(i in 1:N){

    F_hat[i] = N_i[i]*(lambda_base + lambda_dev*trt[i]) -
    
    acam[i]*(alpha_acam_base + alpha_acam_dev*trt[i]) * brho[i] -
    amme[i]*(alpha_amme_base + alpha_amme_dev*trt[i]) * brho[i] -
    anar[i]*(alpha_anar_base + alpha_anar_dev*trt[i]) * brho[i] - 
    brho[i]*(alpha_brho_base + alpha_brho_dev*trt[i]) * brho[i] -
    brni[i]*(alpha_brni_base + alpha_brni_dev*trt[i]) * brho[i] - 
    ceso[i]*(alpha_ceso_base + alpha_ceso_dev*trt[i]) * brho[i] -
    gitr[i]*(alpha_gitr_base + alpha_gitr_dev*trt[i]) * brho[i] -
    leni[i]*(alpha_leni_base + alpha_leni_dev*trt[i]) * brho[i] -
    lomu[i]*(alpha_lomu_base + alpha_lomu_dev*trt[i]) * brho[i] -
    mael[i]*(alpha_mael_base + alpha_mael_dev*trt[i]) * brho[i] -
    mica[i]*(alpha_mica_base + alpha_mica_dev*trt[i]) * brho[i] -
    pler[i]*(alpha_pler_base + alpha_pler_dev*trt[i]) * brho[i] -
    plno[i]*(alpha_plno_base + alpha_plno_dev*trt[i]) * brho[i] -
    taca[i]*(alpha_taca_base + alpha_taca_dev*trt[i]) * brho[i] -
    thir[i]*(alpha_thir_base + alpha_thir_dev*trt[i]) * brho[i] -
    twil[i]*(alpha_twil_base + alpha_twil_dev*trt[i]) * brho[i] -
    weeds[i]*(alpha_weeds_base + alpha_weeds_dev*trt[i] * brho[i]);
    
    F_hat2[i] = F_hat[i]*epsilon[Blocks[i]]; // effect of block 
    
  }
  
  // calculate the likelihood
 Fecundity ~ neg_binomial_2(F_hat2, disp_dev);
  // likelihood outside of for-loop
  // could think of this as observation error term
  
}

