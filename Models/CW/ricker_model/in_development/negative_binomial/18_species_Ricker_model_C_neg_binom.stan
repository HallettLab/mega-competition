// Ricker growth model

data{
  int<lower = 1> N; // number of observations
  int Fecundity[N]; // should this be a vector?
  vector[N] intra;
  real intra_g;
  real mean_ctrl_seeds; 
  real sd_ctrl_seeds;
  //vector[N] acam;
  //vector[N] amme;
  //vector[N] anar;
  vector[N] brho;
  //vector[N] brni;
  //vector[N] ceso;
  //vector[N] gitr;
  //vector[N] leni;
  //vector[N] lomu;
  //vector[N] mael;
  //vector[N] mica;
  vector[N] pler;
  //vector[N] plno;
  //vector[N] taca;
  //vector[N] thir;
  //vector[N] twil;
  //vector[N] crco;
  //vector[N] erbo;
  //vector[N] figa;
  //vector[N] gamu; 
  //vector[N] hygl;
  //vector[N] siga;
  //vector[N] other;
  
}

parameters{
  //real<lower = 0> lambda;
  
   real <lower = 0, upper = 15000> lambda; // changed this from real to int in attempt to get neg_binomial function to work
   real<lower=0> phi; // negative binomial overdispersion parameter
   //real alpha_acam;
   //real alpha_amme;
   //real alpha_anar;
   //real alpha_avba;
   real alpha_brho;
   //real alpha_brni;
   //real alpha_ceso;
   //real alpha_clpu;
   //real alpha_gitr;
   //real alpha_leni;
   //real alpha_lomu;
   //real alpha_mael;
   //real alpha_mica;
   real alpha_pler;
   //real alpha_plno;
   //real alpha_taca;
   //real alpha_thir;
   //real alpha_twil;
   //real alpha_crco; 
   //real alpha_erbo; 
   //real alpha_figa;
   //real alpha_gamu; 
   //real alpha_hygl;
   //real alpha_siga;
   //real alpha_other;

}

model{
  // create a vector of predictions
  //vector[N] F_hat;
  // set priors
  phi ~ gamma(1,1); // prior for overdispersion parameter
  //alpha_acam ~ normal(0, 1);
  //alpha_amme ~ normal(0, 1);
  //alpha_anar ~ normal(0, 1);
  //alpha_avba ~ normal(0, 1);
  alpha_brho ~ normal(0, 1);
  //alpha_brni ~ normal(0, 1);
  //alpha_ceso ~ normal(0, 1);
  //alpha_clpu ~ normal(0, 1);
  //alpha_gitr ~ normal(0, 1);
  //alpha_leni ~ normal(0, 1);
  //alpha_lomu ~ normal(0, 1);
  //alpha_mael ~ normal(0, 1);
  //alpha_mica ~ normal(0, 1);
  alpha_pler ~ normal(0, 1);
  //alpha_plno ~ normal(0, 1);
  //alpha_taca ~ normal(0, 1);
  //alpha_thir ~ normal(0, 1);
  //alpha_twil ~ normal(0, 1);
  //alpha_crco ~ normal(0, 1);
  //alpha_erbo ~ normal(0, 1);
  //alpha_figa ~ normal(0, 1);
  //alpha_gamu ~ normal(0, 1);
  //alpha_hygl ~ normal(0, 1);
  //alpha_siga ~ normal(0, 1);
  //alpha_other ~ normal(0, 1);
  
  // changed this to a cauchy distrib based on Bimler et al. 2023; not sure if it's right, but trying for now
  lambda ~ normal(mean_ctrl_seeds, sd_ctrl_seeds);

  // implement the biological model (control, germ rates taken from mean of temps, but using corresponding WP)
  // this models F (seed production) and so does not include seed survival from the seed back (see equations 3 and 4 from Kraft et al. 2015)
  
  for(i in 1:N){
    real lambda_t1 = intra[i]*intra_g*lambda*exp( 
    //- alpha_acam*acam[i]*0.66 -
    //alpha_amme*amme[i]*0.88 -
    //alpha_anar*anar[i]*0.15 - 
   - alpha_brho*brho[i]*0.97 -
    //alpha_brni*brni[i]*0.69 -
    //alpha_ceso*ceso[i]*0.92 -
    //alpha_gitr*gitr[i]*0.98 -
    //alpha_leni*leni[i]*0.85 -
    //alpha_lomu*lomu[i]*0.96 -
    //alpha_mael*mael[i]*0.59 -
    //alpha_mica*mica[i]*0.72 -
    alpha_pler*pler[i]*0.8 //- 
    //alpha_plno*plno[i]*0.66 - 
    //alpha_taca*taca[i]*0.87 - 
    //alpha_thir*thir[i]*0.79 - 
    //alpha_twil*twil[i]*0.44 -
    //alpha_crco*crco[i] -
    //alpha_erbo*erbo[i] -
    //alpha_figa*figa[i] -
    //alpha_gamu*gamu[i] -
    //alpha_hygl*hygl[i] -
    //alpha_siga*siga[i] -
    //alpha_other*other[i]
    ); 
    
     Fecundity[i] ~ neg_binomial_2(lambda_t1, phi);
    
  }

  // this might be one place to change to a negative binomial distribution?
  // calculate the likelihood
 // Fecundity ~ neg_binomial2(F_hat);
  
}

