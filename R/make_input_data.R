make_input_data = function(raw_data, 
                           species_list,
                           standardize = TRUE,
                           shared_trends = FALSE,
                           standardize_covariates = TRUE,
                           overdispersion = FALSE) {
  
  if(!require(dplyr)) install.packages("dplyr") else require(dplyr)
  if(!require(Thermimage)) install.packages("Thermimage") else require(Thermimage)
  
  # Get temperature threshold for heatwave metrics -----------------------------
  temp_threshold = get_temp_threshold(raw_data$Temp_C)
  
  # Format data ----------------------------------------------------------------
  filtered_data = raw_data %>%
    filter(Genus_species %in% species_list) %>%
    mutate(Date = as.Date(paste(YEAR, MONTH, DAY, sep = "-"))) %>%
    group_by(Genus_species, YEAR) %>%
    summarise(n          = mean(Total_NUM, na.rm = TRUE),
              sal        = mean(SAL, na.rm = TRUE),
              do         = mean(O2_MG_L, na.rm = TRUE),
              temp       = mean(Temp_C, na.rm = TRUE),
              maxTemp    = max(Temp_C, na.rm = TRUE),
              days_above = n_distinct(Date[Temp_C > temp_threshold])) 
  
  names(filtered_data) = c("species", "year", "y", "sal", "do", "temp", "max_temp", "days_above")
  
  # fill in missing years with NAs ---------------------------------------------
  # min_year  = 1981
  # max_year  = 2018
  # all_years = expand.grid(species = unique(filtered_data$species),
  #                         year    = min_year:max_year)
  # 
  # df_filled = all_years %>%
  #   left_join(filtered_data, by = c("species", "year")) %>%
  #   arrange(species, year)
  
  df_filled = filtered_data
  
  # Data inputation ------------------------------------------------------------
  y = df_filled$y
  for (i in 2:length(y)) {
    
      if (is.na(y[i])) y[i] = y[i-1] + rnorm(1, 0, 0.05)
    
  }
  
  # ----------------------------------------------------------------------------
  previous_na_action = options('na.action')
  options(na.action = 'na.pass')
  
  N            = length(unique(df_filled$year))
  M            = length(unique(df_filled$species))
  X            = model.matrix(~ df_filled$sal + df_filled$do + df_filled$temp + df_filled$max_temp + df_filled$days_above)
  K            = dim(X)[2]
  y            = y
  states       = 1:M
  S            = M
  n_obsvar     = 1
  proVariances = c(rep(1, M), 0)
  obsVariances = rep(1, M)
  trends       = proVariances
  est_trend    = ifelse(shared_trends == TRUE, 1, 0)
  est_nu       = ifelse(overdispersion == TRUE, 1, 0)
  family       = 1
  n_provar     = 1
  n_trends     = S
  n_pos        = dim(df_filled)[1]
  
  # Standardize covariates -----------------------------------------------------
  if (standardize_covariates == TRUE) {
    
    for (k in 1:K) {
      
      X[,k] = X[,k]/mean(X[,k], na.rm = TRUE) 
      
    }
  }
  
  # Data inputation ------------------------------------------------------------
  for (k in 1:K) {
    for (i in 1:n_pos) if (is.na(X[i,k])) X[i,k] = X[i-1,k] + rnorm(1, 0, 0.05) # missing value = value at t-1 + random error
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
