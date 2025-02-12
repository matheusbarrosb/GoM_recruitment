make_input_data = function(raw_data, species_list) {
  
  if(!require(dplyr)) install.packages("dplyr") else require(dplyr)
  
  # Format data ----------------------------------------------------------------
  filtered_data = raw_data %>%
    filter(Genus_species %in% species_list) %>%
    group_by(Genus_species, YEAR, MONTH, STATION, DAY) %>%
    summarise(n = Total_NUM) %>%
    na.exclude() %>%
    group_by(Genus_species, YEAR) %>%
    summarise(mean_count = mean(n, na.rm = TRUE),
              sd         = sd(n, na.rm = TRUE),
              se         = sd/sqrt(n()))
  
  names(filtered_data) = c("species", "year", "y", "sd", "se")
  
  
  N            = length(unique(filtered_data$year))
  M            = length(unique(filtered_data$species))
  y            = filtered_data$y
  states       = 1:M
  S            = M
  n_obsvar     = 1
  proVariances = c(rep(1, M), 0)
  obsVariances = rep(1, M)
  trends       = proVariances
  est_trend    = TRUE
  est_nu       = TRUE
  family       = 1
  n_provar     = 1
  n_trends     = 1
  n_pos        = dim(filtered_data)[1]
  
  # indexing
  row_indx_pos = matrix((rep(1:M, N)), M, N)[which(!is.na(y))]
  col_indx_pos  = matrix(sort(rep(1:N, M)), M, N)[which(!is.na(y))]
  
  
  est_A = rep(1, M)
  for (i in 1:max(states)) {
    
    index = which(states == i)
    est_A[index[1]] = 0
    
  }
  
  est_A = which(est_A > 0)
  est_A = c(est_A, 0, 0)
  n_A   = length(est_A) - 2
  
  output = list(
    N = N,
    M = M,
    y = y,
    states = states,
    S = S,
    n_obsvar = n_obsvar,
    proVariances = proVariances,
    obsVariances = obsVariances,
    n_provar = n_provar,
    n_trends = n_trends,
    n_pos = n_pos,
    row_indx_pos = row_indx_pos,
    col_indx_pos = col_indx_pos,
    est_trend = est_trend,
    est_A = est_A,
    est_nu =est_nu,
    n_A = n_A,
    trends = trends,
    family = family
  )
  
  return(output)
  
}
