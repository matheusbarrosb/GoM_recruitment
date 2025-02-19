make_input_data = function(raw_data, species_list, standardize = TRUE, shared_trends = FALSE) {
  
  if(!require(dplyr)) install.packages("dplyr") else require(dplyr)
  if(!require(Thermimage)) install.packages("Thermimage") else require(Thermimage)
  
  # Format data ----------------------------------------------------------------
  filtered_data = raw_data %>%
    filter(Genus_species %in% species_list) %>%
    group_by(Genus_species, YEAR, MONTH, STATION, DAY) %>%
    summarise(n = Total_NUM,
              sal  = SAL,
              do   = O2_MG_L,
              temp = Temp_C) %>%
    na.exclude() %>%
    group_by(Genus_species, YEAR) %>%
    summarise(mean_count = mean(n, na.rm = TRUE),
              sal        = mean(sal, na.rm = TRUE),
              do         = mean(do, na.rm = TRUE),
              temp       = mean(temp, na.rm = TRUE))
  
  names(filtered_data) = c("species", "year", "y", "sal", "do", "temp")
  
  # fill in missing years with NAs ---------------------------------------------
  min_year  = 1981
  max_year  = 2018
  all_years = expand.grid(species = unique(filtered_data$species),
                          year    = min_year:max_year)
  
  df_filled = all_years %>%
    left_join(filtered_data, by = c("species", "year")) %>%
    arrange(species, year)
  
  # Data inputation ------------------------------------------------------------
  y = df_filled$y
  for (i in 2:length(y)) {
    
      if (is.na(y[i])) y[i] = y[i-1] + rnorm(1, 0, sd =  y[i-1]*0.1)
    
  }
  
  # ----------------------------------------------------------------------------
  previous_na_action = options('na.action')
  options(na.action = 'na.pass')
  
  N            = length(unique(df_filled$year))
  M            = length(unique(df_filled$species))
  X            = model.matrix(~ df_filled$sal + df_filled$do + df_filled$temp)
  K            = dim(X)[2]
  y            = y
  states       = 1:M
  S            = M
  n_obsvar     = 1
  proVariances = c(rep(1, M), 0)
  obsVariances = rep(1, M)
  trends       = proVariances
  est_trend    = ifelse(shared_trends == TRUE, 1, 0)
  est_nu       = 1
  family       = 1
  n_provar     = 1
  n_trends     = S
  n_pos        = dim(df_filled)[1]
  
  # Data inputation ------------------------------------------------------------
  for (k in 1:K) {
    for (i in 1:n_pos) if (is.na(X[i,k])) X[i,k] = X[i-1,k] # missing value = value at t-1
  }
  
  # standardization ------------------------------------------------------------
  if (standardize == TRUE) {
    
    means   = rep(NA, S)
    split_y = list()
    for (i in 1:S) means[i] = bin_mean(y, every = N)[i]
    
    split_y = split(y, ceiling(seq_along(y)/N))
    
    split_y_std = list()
    for (i in 1:S) split_y_std[[i]] = as.numeric(unlist(split_y[i]))/means[i]
    
    y = unlist(split_y_std)

  } else y = y
  
  # indexing -------------------------------------------------------------------
  row_indx_pos = as.numeric(as.factor(df_filled$species))
  col_indx_pos = as.numeric(as.factor(df_filled$year))
  
  # ----------------------------------------------------------------------------
  est_A = rep(1, M)
  for (i in 1:max(states)) {
    
    index = which(states == i)
    est_A[index[1]] = 0
    
  }
  
  est_A = which(est_A > 0)
  est_A = c(est_A, 0, 0)
  n_A   = length(est_A) - 2
  
  data_list = list(
    
    N            = N,
    M            = M,
    y            = y,
    X            = X,
    K            = K,
    states       = states,
    S            = S,
    n_obsvar     = n_obsvar,
    proVariances = proVariances,
    obsVariances = obsVariances,
    n_provar     = n_provar,
    n_trends     = n_trends,
    n_pos        = n_pos,
    row_indx_pos = row_indx_pos,
    col_indx_pos = col_indx_pos,
    est_trend    = est_trend,
    est_A        = est_A,
    est_nu       = est_nu,
    n_A          = n_A,
    trends       = trends,
    family       = family
    
  )
  
  options(na.action=previous_na_action$na.action)
  
  output = list(data_list, df_filled)
  names(output) = c("stan_input", "df")
  
  return(output)
  
}
