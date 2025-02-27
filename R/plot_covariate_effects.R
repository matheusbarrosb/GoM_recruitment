plot_covariate_effects = function(fit, species_list, stan_input) {
  
  B = fit$summary(variables = "B", ~ quantile(.x, probs = c(0.5, 0.2, 0.8)))
  names(B) = c("parameter", "mean", "q20", "q80")
  
  spps = list()
  for (i in 1:stan_input$S) spps[[i]] = rep(selected_spps[[i]], stan_input$K)
  spps = unlist(spps)
  B$spps = spps
  B$spps = sapply(B$spps, capitalize_first_word)
  
  cov_list = list("alpha", "[beta]Salinity", "[beta]DO", "[beta]Temperature", "[beta]Max_temp", "[beta]Days_above_28") 
  covs = rep(unlist(cov_list), stan_input$S)
  B$covs = covs
  
  labels = c(
    "alpha" = expression(alpha),
    "[beta]Temperature" = expression(beta[temperature]),
    "[beta]DO" = expression(beta[DO]),
    "[beta]Salinity" = expression(beta[salinity]),
    "[beta]Max_temp" = expression(beta[Max_temp]),
    "[beta]Days_above_28" = expression(beta[Days_above_28])
  )
  
  B %>%
    mutate(trend = ifelse(mean < 0, ifelse(q20 <= 0 & q80 >=0, "No effect", "Negative effect"), ifelse(q20 <= 0 & q80 >=0, "No effect", "Positive effect"))) %>%
    
    ggplot(aes(x = covs, y = mean)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_errorbar(aes(ymin = q20, ymax = q80), width = .1) +
    geom_point(aes(fill = trend, shape = covs), shape = 21, size = 2) +
    facet_wrap(~spps, ncol = 6, scales = "free_x") +
    coord_flip() +
    scale_x_discrete(labels = labels) +
    custom_theme() +
    theme(strip.text.x = element_text(face = "italic"),
          legend.title = element_blank(),
          legend.position = "top") +
    scale_fill_manual(values = c("Negative effect" = "red",
                                 "No effect" = "grey70",
                                 "Positive effect" = "blue")) +
    ylab("Parameter value") +
    xlab("")

  
}
