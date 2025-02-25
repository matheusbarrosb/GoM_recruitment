plot_fits_to_data = function(stan_input, fit, species_list) {
  
  if(!require(stringr)) install.packages("stringr") else require(stringr)
  
  preds = fit$summary(variables = "pred")
  
  # Keep only observed years ---------------------------------------------------
  year_index = t(sapply(preds$variable, extract_indices))[,1]
  spp_index  = t(sapply(preds$variable, extract_indices))[,2]
  
  preds$year_index = as.numeric(year_index)
  preds$spp_index = as.numeric(spp_index)
  
  years_to_keep = as.numeric(stan_input$stan_input$col_indx_pos)
  spps_to_keep = as.numeric(stan_input$stan_input$row_indx_pos)
  
  keep_df = data.frame(
    spp_index = spps_to_keep,
    year_index = years_to_keep
  )

  filtered_data = preds %>%
    inner_join(keep_df, by = c("spp_index", "year_index"))
  
  # ----------------------------------------------------------------------------
  
  N = stan_input$df %>%
    group_by(species) %>%
    summarise(n_distinct(year))
  
  N = as.numeric(unlist(N[,2]))
  
  spps = list()
  for (i in 1:(stan_input$stan_input$S)) spps[[i]] = rep(species_list[[i]], N[i])
  spps = unlist(spps)
  
  filtered_data$spps = spps
  filtered_data$spps = sapply(filtered_data$spps, capitalize_first_word)
  filtered_data$year = as.numeric(filtered_data$year_index) + 1980
  filtered_data$obs  = stan_input$stan_input$y
 
  filtered_data %>%
    ggplot() +
    geom_line(aes(x = year, y = median, color = "Estimated trend"),
              linetype = 1,
              linewidth = 1) +
    geom_line(aes(x = year, y = obs), color = "black") +
    geom_point(aes(x = year, y = obs), color = "black", shape = 1) +
    geom_ribbon(aes(ymin = q5, ymax = q95, x = year), alpha = 0.2) +
    xlab("Year") +
    ylab("Mean recruitment index") +
    facet_wrap(~spps, scales = "free_y") +
    theme_minimal() +
    theme(legend.title = element_blank(),
          legend.position = "top",
          strip.text.x = element_text(face = "italic"))
  
}

