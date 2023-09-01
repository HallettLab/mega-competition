// Ricker growth model
// Incorporating random effects of block


data{
  int<lower = 1> N; // number of observations
  int Fecundity[N]; // observed fecundity count at time t+1
  
  // add in random effects
  int<lower=1> N_blocks; // Number of groups
  int group[N]; // Group indicator
  
  vector[N] intra; // population size of species i at time t
  real intra_g; //germination of the focal species

// population sizes of interacting species at time t
  //vector[N] acam;
  //vector[N] amme;
  //vector[N] anar;
  //vector[N] brho;
  //vector[N] brni;
  //vector[N] ceso;
  //vector[N] gitr;
  //vector[N] leni;
  //vector[N] lomu;
  //vector[N] mael;
  //vector[N] mica;
  //vector[N] pler;
  //vector[N] plno;
  //vector[N] taca;
  //vector[N] thir;
  //vector[N] twil;
  //vector[N] weeds;
  
  // do NOT use these- double dipping in data
  //real mean_ctrl_seeds; 
 // real sd_ctrl_seeds;
  
}

parameters{
  
  // lambda
  real<lower = 0, upper = 10000> lambda;
  
  real epsilon[N_blocks];
  real<lower = 0> sigma; // group-level random effects
  
  // all the alphas
   //real alpha_acam;
   //real alpha_amme;
   //real alpha_anar;
   //real alpha_brho;
   //real alpha_brni;
   //real alpha_ceso;
   //real alpha_gitr;
   //real alpha_leni;
   //real alpha_lomu;
   //real alpha_mael;
   //real alpha_mica;
   //real alpha_pler;
   //real alpha_plno;
   //real alpha_taca;
   //real alpha_thir;
   //real alpha_twil;
   //real alpha_weeds;

}

model{
  
  // create a vector of predictions
  //vector[N] F_hat;
  
  
  // set priors
  lambda ~ gamma(0.001, 0.001); // non-informative prior!
  
  //phi ~ gamma(1,1); // prior for overdispersion parameter
  sigma ~ gamma(0.001, 0.001);
  epsilon ~ gamma(sigma, sigma); // prior for group level random effects

  //alpha_acam ~ normal(0, 1);
  //alpha_amme ~ normal(0, 1);
  //alpha_anar ~ normal(0, 1);
  //alpha_brho ~ normal(0, 1);
  //alpha_brni ~ normal(0, 1);
  //alpha_ceso ~ normal(0, 1);
  //alpha_gitr ~ normal(0, 1);
  //alpha_leni ~ normal(0, 1);
  //alpha_lomu ~ normal(0, 1);
  //alpha_mael ~ normal(0, 1);
 // alpha_mica ~ normal(0, 1);
  //alpha_pler ~ normal(0, 1);
  //alpha_plno ~ normal(0, 1);
  //alpha_taca ~ normal(0, 1);
  //alpha_thir ~ normal(0, 1);
  //alpha_twil ~ normal(0, 1);
  //alpha_weeds ~ normal(0,1);

  // implement the biological model (control, germ rates taken from mean of temps, but using corresponding WP)
  // this models F (seed production) and so does not include seed survival from the seed back (see equations 3 and 4 from Kraft et al. 2015)
  vector[N] F_hat;
  vector[N] F_hat2;
  
  for(i in 1:N){
    F_hat[i] = intra[i]*intra_g*lambda;
    // use stems data - NOT back calculated seeds in data
    // should model both precip conditions together
    
    //*exp( 
    //- alpha_acam*acam[i]*0.66 -
    //alpha_amme*amme[i]*0.88 -
    //alpha_anar*anar[i]*0.15 - 
    //alpha_brho*brho[i]*0.97 -
    //alpha_brni*brni[i]*0.69 -
    //alpha_ceso*ceso[i]*0.92 -
    //alpha_gitr*gitr[i]*0.98 -
    //alpha_leni*leni[i]*0.85 -
    //alpha_lomu*lomu[i]*0.96 -
    //alpha_mael*mael[i]*0.59 -
    //alpha_mica*mica[i]*0.72 -
    //alpha_pler*pler[i]*0.8 - 
    //alpha_plno*plno[i]*0.66 - 
    //alpha_taca*taca[i]*0.87 - 
    //alpha_thir*thir[i]*0.79 - 
    //alpha_twil*twil[i]*0.44 -
    //alpha_weeds*weeds[i]) +
    F_hat2[i] = F_hat[i]*epsilon[group[i]]; // effect of group (block) // because drawing from poisson need to multiply rather than add the group effect
    
  }
  
  // calculate the likelihood
  Fecundity ~ poisson(F_hat2);
  // likelihood outside of for-loop
  // could think of this as observation error term
}

