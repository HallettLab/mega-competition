// Ricker growth model
// Incorporating random effects of block
// Models both precip treatments together


data{
  
  int<lower = 1> N; // number of observations
  int Fecundity[N]; // observed fecundity count at time t+1
  
  // add in random effects
  int<lower=1> N_blocks; // Number of groups
  int block[N]; // block indicator
  
  vector[N] N_i; // population size of species i at time t
  vector[N] g_i; //germination of the focal species
  vector[N] trt; // precip treatment

// population sizes of interacting species at time t
  vector[N] brho;
  vector[N] pler;
  vector[N] weeds;

}


parameters{
  
  // lambda
  real<lower = 0, upper = 10000> lambda_base; //
  //real lambda_dev;
  
  // group-level random effects
  real epsilon[N_blocks]; // added in a lower bound of epsilon to see if this helps the error evaluating log probability at the initial value?
  real sigma_0; // needs the lower bound
  
  // all the alphas
   real alpha_brho_base;
   real alpha_brho_dev;
   real alpha_pler_base;
   real alpha_pler_dev;
   real alpha_weeds_base;
   real alpha_weeds_dev;

}

transformed parameters{
  real<lower = 0> sigma;
  sigma = exp(sigma_0);
}

model{
  
  // set priors
  //lambda_base ~ gamma(0.001, 0.001); // non-informative prior!
  lambda_base ~ gamma(0.001, 0.001);
  //lambda_dev ~ normal(0,100); //if get errors with this running let Lauren know; lambda+lambda_dev can't be below 0; if it doesn't work, take it out and get alpha-dev running 
  sigma_0 ~ normal(0, 1000);
  //sigma ~ gamma(0.001, 0.001);
  epsilon ~ gamma(sigma, sigma); // prior for group level random effects
  alpha_brho_base ~ normal(0, 1000);
  alpha_brho_dev ~ normal(0, 1000);
  alpha_pler_base ~ normal(0, 1000);
  alpha_pler_dev ~ normal(0, 1000);
  alpha_weeds_base ~ normal(0,1000);
  alpha_weeds_dev ~ normal(0,1000);

  // create vector of predictions
  vector[N] F_hat;
  vector[N] F_hat2;
  
  // Biological model
  for(i in 1:N){

    F_hat[i] = N_i[i]*g_i[i]*(lambda_base)* //+ lambda_dev*trt[i])*
    exp(-brho[i]*(alpha_brho_base + alpha_brho_dev*trt[i]) -
    pler[i]*(alpha_pler_base + alpha_pler_dev*trt[i]) -
    weeds[i]*(alpha_weeds_base + alpha_weeds_dev*trt[i]));
    
    F_hat2[i] = F_hat[i]*epsilon[block[i]]; // effect of block 
    // because drawing from poisson need to multiply rather than add the block effect
    
  }
  
  // calculate the likelihood
  Fecundity ~ poisson(F_hat2);
  // likelihood outside of for-loop
  // could think of this as observation error term
  
}

