functions {
  real partial_sum(int[] slice_idx,
                  int start, int end,
                  vector y,
                  vector doy,
                  array[] int row_indx_pos,
                  array[] int col_indx_pos,
                  matrix k,
                  matrix doy_50,
                  vector sigma) {
    real lp = 0;
    for (i in 1:size(slice_idx)) {
      int j = slice_idx[i];
      real mu = inv_logit(
        k[row_indx_pos[j], col_indx_pos[j]] * (doy[j] - doy_50[row_indx_pos[j], col_indx_pos[j]])
      );
      lp += normal_lpdf(y[j] | mu, sigma[col_indx_pos[j]]);
    }
    return lp;
  }
}
data {
  int<lower=0> N;
  int<lower=0> S;
  int<lower=0> n_pos;
  vector[n_pos] y;
  vector[n_pos] doy;
  array[n_pos] int<lower=0> col_indx_pos;
  array[n_pos] int<lower=0> row_indx_pos;
}
parameters {
  matrix[N, S] k_raw;
  matrix[N, S] doy_50_raw;
  real mu_k;
  real<lower=0> sigma_k;
  real<lower=0,upper=365> mu_doy_50;
  real<lower=0> sigma_doy_50;
  vector<lower=0>[S] sigma;
}
transformed parameters {
  matrix[N, S] k = mu_k + sigma_k * k_raw;
  matrix[N, S] doy_50 = mu_doy_50 + sigma_doy_50 * doy_50_raw;
}
model {
  mu_k ~ normal(0, 5);
  sigma_k ~ normal(0, 5);
  to_vector(k_raw) ~ normal(0, 1);

  mu_doy_50 ~ normal(100, 50);
  sigma_doy_50 ~ normal(0, 20);
  to_vector(doy_50_raw) ~ normal(0, 1);

  sigma ~ gamma(0.01, 0.01);

  array[n_pos] int data_idx = linspaced_int_array(n_pos, 1, n_pos);
  target += reduce_sum(partial_sum, data_idx, 1, y, doy, row_indx_pos, col_indx_pos, k, doy_50, sigma);
}
generated quantities {
  vector[n_pos] y_rep;
  for (j in 1:n_pos) {
    real mu = inv_logit(
      k[row_indx_pos[j], col_indx_pos[j]] * (doy[j] - doy_50[row_indx_pos[j], col_indx_pos[j]])
    );
    y_rep[j] = normal_rng(mu, sigma[col_indx_pos[j]]);
  }
}