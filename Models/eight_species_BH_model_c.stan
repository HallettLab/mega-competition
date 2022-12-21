// Beverton-Holt growth model

data{
  int<lower = 1> N;
  int Fecundity[N];
  vector[N] intra;
  real intra_g;
  //real intra_s; //ignoring this for now until we process the data
  vector[N] pler;
  vector[N] anar;
  vector[N] acam;
  vector[N] brni;
  vector[N] clpu;
  vector[N] brho;
  vector[N] gitr;
  vector[N] amme;
  vector[N] plno;
  vector[N] thir;
  vector[N] mica;
  vector[N] ceso;
  vector[N] twil;
  vector[N] lomu;
  vector[N] taca;
  vector[N] mael;
  vector[N] leni;
  vector[N] avba;
}


parameters{
  //real<lower = 0> lambda;
  real<lower = 0, upper = 10000> lambda;
  real alpha_pler;
  real alpha_anar;
  real alpha_acam;
  real alpha_brni;
  real alpha_clpu;
  real alpha_brho;
  real alpha_gitr;
  real alpha_amme;
  real alpha_plno;
  real alpha_thir;
  real alpha_mica;
  real alpha_ceso;
  real alpha_twil;
  real alpha_lomu;
  real alpha_taca;
  real alpha_mael;
  real alpha_leni;
  real alpha_avba;

  //real<lower = 0>  alpha_avfa;
  //real<lower = 0>  alpha_brho;
  //real<lower = 0>  alpha_esca;
  //real<lower = 0>  alpha_laca;
  //real<lower = 0>  alpha_trhi;
  //real<lower = 0>  alpha_vumy;

}

model{
  // create a vector of predictions
  vector[N] F_hat;

  // set priors
  alpha_pler ~ normal(0, 1);
  alpha_anar ~ normal(0, 1);
  alpha_acam ~ normal(0, 1);
  alpha_brni ~ normal(0, 1);
  alpha_clpu ~ normal(0, 1);
  alpha_brho ~ normal(0, 1);
  alpha_gitr ~ normal(0, 1);
  alpha_amme ~ normal(0, 1);
  alpha_plno ~ normal(0, 1);
  alpha_thir ~ normal(0, 1);
  alpha_mica ~ normal(0, 1);
  alpha_ceso ~ normal(0, 1);
  alpha_twil ~ normal(0, 1);
  alpha_lomu ~ normal(0, 1);
  alpha_taca ~ normal(0, 1);
  alpha_mael ~ normal(0, 1);
  alpha_leni ~ normal(0, 1);
  alpha_avba ~ normal(0, 1);
  lambda ~ gamma(0.001, 0.001);

  // implement the biological model (control, germ rates taken from mean of temps, but using corresponding WP)
  for(i in 1:N){
    F_hat[i] = lambda*intra[i]*intra_g / (1 + 
    alpha_pler*pler[i]*0.8 + 
    alpha_anar*anar[i]*0.15 + 
    alpha_acam*acam[i]*0.66 + 
    alpha_brni*brni[i]*0.69 + 
    alpha_clpu*clpu[i]*0.37 + 
    alpha_brho*brho[i]*0.97 + 
    alpha_gitr*gitr[i]*0.98 + 
    alpha_amme*amme[i]*0.88 + 
    alpha_plno*plno[i]*0.66 + 
    alpha_thir*thir[i]*0.79 + 
    alpha_mica*mica[i]*0.72 + 
    alpha_ceso*ceso[i]*0.92 + 
    alpha_twil*twil[i]*0.44 + 
    alpha_lomu*lomu[i]*0.96 + 
    alpha_taca*taca[i]*0.87 + 
    alpha_mael*mael[i]*0.59 + 
    alpha_leni*leni[i]*0.85 + 
    alpha_avba*avba[i]*0.96);
  }

  // calculate the likelihood
  Fecundity ~ poisson(F_hat);
}
