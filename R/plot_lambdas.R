plot_lambdas = function(fit, species_list, stan_input, plot_type = NULL) {
  
  lambdas = fit$summary(variables = "lambda")
  
  # Kepp observed years only ---------------------------------------------------
  indices = gsub("lambda\\[|\\]", "", lambdas$variable)
  
  split_list = strsplit(indices, ",")
  year_index = sapply(split_list, function(x) as.numeric(x[1]))
  spp_index  = sapply(split_list, function(x) as.numeric(x[2]))
  
  lambdas$year_index = year_index
  lambdas$spp_index = spp_index
  
  years_to_keep = as.numeric(stan_input$stan_input$col_indx_pos)
  spps_to_keep  = as.numeric(stan_input$stan_input$row_indx_pos)
  
  keep_df = data.frame(year_index = years_to_keep, spp_index = spps_to_keep)
  
  filtered_data = lambdas %>%
    inner_join(keep_df, by = c("year_index", "spp_index"))
  
  # ------------------------------------------------------------------------------
  N = stan_input$df %>%
    group_by(species) %>%
    summarise(n_distinct(year))
  
  N = as.numeric(unlist(N[,2]))
  
  spps = list()
  for (i in 1:length(N)) spps[[i]] = rep(species_list[[i]], N[i])
  spps = unlist(spps)
  
  filtered_data$spps = spps
  filtered_data$spps = sapply(filtered_data$spps, capitalize_first_word)
  filtered_data$year = filtered_data$year_index + 1980
  
  if (plot_type == "time_series") {
  
  filtered_data %>%
    ggplot(aes(x = year, y = median)) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    custom_theme() +
    theme(legend.position = "none",
          strip.text.x = element_text(face = "italic")) +
    facet_wrap(~spps, scales = "free_y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ylab(expression(lambda)) +
    xlab("Year")
    
  } else {
    
    filtered_data %>%
      group_by(spps) %>%
      summarise(lambda = mean(median, na.rm = TRUE),
                sd   = sd(median, na.rm = TRUE),
                se   = sd/sqrt(n())) %>%
      mutate(trend = ifelse(lambda < 0, ifelse(lambda + se > 0, "Stable", "Declining"), ifelse(lambda - se < 0, "Stable", "Increasing"))) %>%
      mutate(color = ifelse(trend == "Stable", "gray", ifelse(trend == "Declining", "red", "blue"))) %>%
      
      ggplot(aes(x = spps, y = lambda)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      geom_errorbar(aes(ymin = lambda - se, ymax = lambda + se), width = 0.1) +
      geom_point(aes(fill = trend), shape = 21, size = 2) +
      coord_flip() +
      custom_theme() +
      theme(axis.text.y = element_text(face = "italic"),
            legend.title = element_blank(),
            legend.position = "top") +
      scale_fill_manual(values = c("Declining" = "red",
                                   "Stable" = "grey70",
                                   "Increasing" = "blue")) +
      xlab("") +
      ylab(expression(lambda))

    
  }
 
}
