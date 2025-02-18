plot_fits_to_data = function(stan_input, fit, species_list) {
  
  if(!require(stringr)) install.packages("dplyr") else require(stringr)
  
  preds = fit$summary(variables = "pred")
  
  spps = list()
  for (i in 1:stan_input$S) spps[[i]] = rep(selected_spps[[i]], stan_input$N)
  spps = unlist(spps)
  
  preds$spps = spps
  preds$spps = sapply(preds$spps, capitalize_first_word)
  preds$year = rep(1:38, stan_input$S) + 1981
  preds$obs  = stan_input$y
  

  preds %>%
    ggplot() +
    geom_line(aes(x = year, y = median, color = "Estimated trend"),
              linetype = 1,
              linewidth = 1) +
    geom_line(aes(x = year, y = obs), color = "black") +
    geom_ribbon(aes(ymin = q5, ymax = q95, x = year), alpha = 0.2) +
    theme_minimal() +
    xlab("Year") +
    ylab("Mean recruitment index") +
    facet_wrap(~spps, scales = "free_y") +
    theme(legend.title = element_blank(),
          legend.position = "top",
          strip.text.x = element_text(face = "italic"))
  
}
