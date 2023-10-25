// Ricker growth model
// Incorporating random effects of block
// Models both precip treatments together - currently just drought

data{
  
  int<lower = 1> N; // number of observations
  int Fecundity[N]; // observed fecundity count at time t+1
  
  // add in random effects
  int<lower=1> N_blocks; // Number of groups
  int Blocks[N]; // block column
  
  vector[N] N_i; // population size of species i at time t
  vector[N] g_i; //germination of the focal species

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
  real epsilon[N_blocks]; // added in a lower bound of epsilon to see if this helps the error evaluating log probability at the initial value?
  real<lower = 0> sigma; // needs the lower bound? removed for the moment...yeah it seems to need it
  
  // lambda
  real<lower = 0, upper = 10000> lambda_base;
  
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

}


model{
  
  // create vector of predictions
  vector[N] F_hat;
  vector[N] F_hat2;
  
  // set priors
  sigma ~ gamma(0.001, 0.001);
  epsilon ~ gamma(sigma, sigma); // prior for group level random effects
  
  lambda_base ~ gamma(0.001, 0.001); // non-informative prior

  
  alpha_acam_base ~ normal(0, 5);
  alpha_amme_base ~ normal(0, 5);
  alpha_anar_base ~ normal(0, 5);
  alpha_brho_base ~ normal(0, 5);
  alpha_brni_base ~ normal(0, 5);
  alpha_ceso_base ~ normal(0, 5);
  alpha_gitr_base ~ normal(0, 5);
  alpha_leni_base ~ normal(0, 5);
  alpha_lomu_base ~ normal(0, 5);
  alpha_mael_base ~ normal(0, 5);
  alpha_mica_base ~ normal(0, 5);
  alpha_pler_base ~ normal(0, 5);
  alpha_plno_base ~ normal(0, 5);
  alpha_taca_base ~ normal(0, 5);
  alpha_thir_base ~ normal(0, 5);
  alpha_twil_base ~ normal(0, 5);
  alpha_weeds_base ~ normal(0, 5);
  
  // Biological model
  for(i in 1:N){
    // use stems data - NOT back calculated seeds in data
    // should model both precip conditions together
    F_hat[i] = N_i[i]*g_i[i]*lambda_base* 
    exp(-acam[i]*(alpha_acam_base)- 
    amme[i]*(alpha_amme_base)- 
    anar[i]*(alpha_anar_base)- 
    brho[i]*(alpha_brho_base)- 
    brni[i]*(alpha_brni_base)-  
    ceso[i]*(alpha_ceso_base)- 
    gitr[i]*(alpha_gitr_base)- 
    leni[i]*(alpha_leni_base)- 
    lomu[i]*(alpha_lomu_base)- 
    mael[i]*(alpha_mael_base)- 
    mica[i]*(alpha_mica_base)- 
    pler[i]*(alpha_pler_base)- 
    plno[i]*(alpha_plno_base)- 
    taca[i]*(alpha_taca_base)- 
    thir[i]*(alpha_thir_base)- 
    twil[i]*(alpha_twil_base)- 
    weeds[i]*(alpha_weeds_base)); 
    
    F_hat2[i] = F_hat[i]*epsilon[Blocks[i]]; // effect of block 
    
  }
  
  // calculate the likelihood
 Fecundity ~ poisson(F_hat2);
  // likelihood outside of for-loop
  // could think of this as observation error term
  
}


