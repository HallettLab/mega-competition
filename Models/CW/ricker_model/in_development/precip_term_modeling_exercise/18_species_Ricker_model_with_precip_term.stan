// Ricker model - Controls only

data{
  int<lower = 1> N; // number of observations for a particular species
  int Fecundity[N]; // 
  vector[N] intra;
  real intra_g;
 // real mean_ctrl_seeds; 
 // real sd_ctrl_seeds;
  vector[N] weeds;
  vector[N] precip;
  
}

parameters{
  real<lower = 0, upper = 15000> lambda;
  real alpha_weeds; 
  real<lower = -0.999> beta; // precipitation term; can't be lower than this, otherwise could get 0 or negative lambda
  
}

model{
  // create a vector of predictions
  //vector[N] F_hat;
  
  // set priors
  alpha_weeds ~ normal(0, 1);
  lambda ~ gamma(0.001, 0.001);
  beta ~ normal(0,1);
  
  vector[N] Fhat;

  // implement the biological model 
  for(i in 1:N){
    
    Fhat[i] = intra[i]*intra_g*lambda*(1 + precip*beta)*exp(
    -alpha_weeds*weeds[i]);
   
  }

  // calculate the likelihood
  Fecundity ~ poisson(F_hat);
  
}

