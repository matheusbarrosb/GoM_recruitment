data {
  int<lower=0> N;       // number of years
  int<lower=0> S;       // number of species
  int<lower=0> n_pos;   // number of data points
  vector[n_pos] y;      // relative cumulative count
  vector[n_pos] doy;    // day of year
  array [n_pos]int<lower=0> col_indx_pos; // species index
  array [n_pos]int<lower=0> row_indx_pos; // year index
}
parameters {
  matrix[N, S] k;       // logistic growth rate
  matrix[N, S] doy_50;  // recruitment peak
  vector<lower=0>[S] sigma;
}
transformed parameters {
  matrix[N, S] mu;
  for (i in 1:N) {
    for (s in 1:S) {
      for (j in 1:n_pos) {
        mu[i,s] = 1 / (1 + exp(-k[i,s]*(doy[row_indx_pos[j]] - doy_50[i,s])));
      }
    }
  }
}
model {
  
  // PRIORS
  sigma  ~ gamma(0.01, 0.01);
  for (i in 1:N) for (s in 1:S) k[i,s]      ~ normal(0, 5);
  for (i in 1:N) for (s in 1:S) doy_50[i,s] ~ normal(100,20);
  
  // LIKELIHOOD
  for (j in 1:n_pos) y[j] ~ normal(mu[row_indx_pos[j], col_indx_pos[j]], sigma[col_indx_pos[j]]);
  
}