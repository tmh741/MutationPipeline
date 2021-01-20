data{
  int<lower=0> U;
  int<lower=0> I;
  int<lower=0> K;
  int<lower=0> y[U,I];
  real<lower=0> lambda0;
  real<lower=0> alpha0;
}

transformed data{
  vector<lower=0>[K] alpha0_vec;
  for (k in 1:K){
    alpha0_vec[k] = alpha0;
  }
}

parameters {
  simplex[K] theta[U];
  vector<lower=0>[I] phi[K];
}

model {
  for (u in 1:U)
    theta[u] ~ dirichlet(alpha0_vec);
  for (i in 1:I)
    phi[i] ~ exponential(lambda0);
    
  for(u in 1:U){
    for(i in 1:I){
      y[u,i] ~ poisson(theta[u]'*phi[i]);
    }
  }
}
