data {
  int<lower=0> N;       // number of years
  int<lower=0> S;       // number of species
  int<lower=0> n_pos;   // number of data points
  vector[n_pos] y;      // relative cumulative count
  vector[n_pos] doy;    // day of year
  array[n_pos] int<lower=0> col_indx_pos; // species index
  array[n_pos] int<lower=0> row_indx_pos; // year index
}
parameters {
  matrix[N, S] k;       // logistic growth rate
  matrix[N, S] doy_50;  // recruitment peak
  vector<lower=0>[S] sigma;
}
model {
  // PRIORS
  sigma  ~ gamma(0.01, 0.01);
  to_vector(k) ~ normal(0, 5);
  to_vector(doy_50) ~ normal(100, 20);

  // LIKELIHOOD (vectorized)
  {
    vector[n_pos] mu;
    for (j in 1:n_pos) {
      mu[j] = inv_logit(
        k[row_indx_pos[j], col_indx_pos[j]] *
        (doy[j] - doy_50[row_indx_pos[j], col_indx_pos[j]])
      );
    }
    y ~ normal(mu, sigma[col_indx_pos]);
  }
}