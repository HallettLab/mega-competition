// Ricker model - Controls only

data{
  int<lower = 1> N; // number of observations for a particular species
  int Fecundity[N]; // 
  vector[N] intra; 
  real intra_g;
  real mean_ctrl_seeds; 
  real sd_ctrl_seeds;
  vector[N] weeds;
  vector[N] precip;
  
}

parameters{
  real<lower = 0, upper = 15000> lambda;
  real<lower=0> phi;
  real alpha_weeds; 
  real<lower = -0.999> beta; // precipitation term; can't be lower than this, otherwise could get 0 or negative lambda
  
}

model{
  // create a vector of predictions
  //vector[N] F_hat;
  
  // set priors
  alpha_weeds ~ normal(0, 1);
  lambda ~ normal(mean_ctrl_seeds, sd_ctrl_seeds);
  beta ~ normal(0,1);

  // implement the biological model 
  for(i in 1:N){
    
    real lambda_t1 = intra[i]*intra_g*lambda*(1 + precip*beta)*exp(
    -alpha_weeds*weeds[i]);
    
    Fecundity[i] ~ neg_binom_2(lambda_t1, phi);
   
  }

  // calculate the likelihood
  //Fecundity ~ neg_binomial(F_hat);
  
}

