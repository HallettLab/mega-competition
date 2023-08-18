// Ricker growth model with squared intra_alpha term

data{
  int<lower = 1> N;
  int Fecundity[N];
  vector[N] intra;
  real intra_g;
  real mean_ctrl_seeds; 
  real sd_ctrl_seeds;
  vector[N] acam;
  vector[N] amme;
  vector[N] anar;
  //vector[N] avba;
  vector[N] brho;
  vector[N] brni;
  vector[N] ceso;
  //vector[N] clpu;
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
  vector[N] crco;
  vector[N] erbo;
  vector[N] figa;
  vector[N] gamu; 
  vector[N] hygl;
  vector[N] siga;
  vector[N] other;
  
}


parameters{
  //real<lower = 0> lambda;
  real<lower = 0, upper = 10000> lambda;
   real alpha_acam_a;
   real alpha_acam_b;
   real alpha_amme;
   real alpha_anar;
   //real alpha_avba;
   real alpha_brho;
   real alpha_brni;
   real alpha_ceso;
   //real alpha_clpu;
   real alpha_gitr;
   real alpha_leni;
   real alpha_lomu;
   real alpha_mael;
   real alpha_mica;
   real alpha_pler;
   real alpha_plno;
   real alpha_taca;
   real alpha_thir;
   real alpha_twil;
   real alpha_crco; 
   real alpha_erbo; 
   real alpha_figa;
   real alpha_gamu; 
   real alpha_hygl;
   real alpha_siga;
   real alpha_other;
   
}

model{
  // create a vector of predictions
  vector[N] F_hat;
  // set priors
  alpha_acam_a ~ normal(0, 1);
  alpha_acam_b ~ normal(0, 1);
  alpha_amme ~ normal(0, 1);
  alpha_anar ~ normal(0, 1);
  //alpha_avba ~ normal(0, 1);
  alpha_brho ~ normal(0, 1);
  alpha_brni ~ normal(0, 1);
  alpha_ceso ~ normal(0, 1);
  //alpha_clpu ~ normal(0, 1);
  alpha_gitr ~ normal(0, 1);
  alpha_leni ~ normal(0, 1);
  alpha_lomu ~ normal(0, 1);
  alpha_mael ~ normal(0, 1);
  alpha_mica ~ normal(0, 1);
  alpha_pler ~ normal(0, 1);
  alpha_plno ~ normal(0, 1);
  alpha_taca ~ normal(0, 1);
  alpha_thir ~ normal(0, 1);
  alpha_twil ~ normal(0, 1);
  alpha_crco ~ normal(0, 1);
  alpha_erbo ~ normal(0, 1);
  alpha_figa ~ normal(0, 1);
  alpha_gamu ~ normal(0, 1);
  alpha_hygl ~ normal(0, 1);
  alpha_siga ~ normal(0, 1);
  alpha_other ~ normal(0, 1);
  lambda ~ normal(mean_ctrl_seeds, sd_ctrl_seeds);
  

  // implement the biological model (drought, germ rates taken from mean of temps, but using corresponding WP)
  for(i in 1:N){
    F_hat[i] = lambda*intra[i]*intra_g*exp(
    - alpha_acam_a*acam[i]*0.52 -
    alpha_acam_b*pow(acam[i],2)*0.52 -
    alpha_amme*amme[i]*0.14 -
    alpha_anar*anar[i]*0.07 -
    // alpha_avba*avba[i]*0.88 -
    alpha_brho*brho[i]*1 - 
    alpha_brni*brni[i]*0.39 - 
    alpha_ceso*ceso[i]*0.74 -
    //alpha_clpu*clpu[i]*0.04 - 
    alpha_gitr*gitr[i]*0.91 -
    alpha_leni*leni[i]*0.3 -
    alpha_lomu*lomu[i]*0.87 -
    alpha_mael*mael[i]*0.18 - 
    alpha_mica*mica[i]*0.13 -
    alpha_pler*pler[i]*0.53 - 
    alpha_plno*plno[i]*0.35 - 
    alpha_taca*taca[i]*0.86 - 
    alpha_thir*thir[i]*0.38 - 
    alpha_twil*twil[i]*0.02 -
    alpha_crco*crco[i] -
    alpha_erbo*erbo[i] -
    alpha_figa*figa[i] -
    alpha_gamu*gamu[i] -
    alpha_hygl*hygl[i] -
    alpha_siga*siga[i] -
    alpha_other*other[i]);
    
  }

  // calculate the likelihood
  Fecundity ~ poisson(F_hat);
  
}

