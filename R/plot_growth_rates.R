plot_growth_rates = function(fit, species_list, stan_input) {
  
  if(!require(dplyr)) install.packages("dplyr") else require(dplyr)
  if(!require(ggplot2)) install.packages("ggplot2") else require(ggplot2)
  if(!require(stringr)) install.packages("stringr") else require(stringr)

  preds = fit$summary(variables = "lambda")

  spps = list()
  for (i in 1:stan_input$S) spps[[i]] = rep(selected_spps[[i]], stan_input$N)
  spps = unlist(spps)

  preds$spps = spps
  preds$spps = sapply(preds$spps, capitalize_first_word)
  preds$year = rep(1:38, stan_input$S) + 1981
    
  sum_df = preds %>%
    group_by(spps) %>%
    summarise(lambda = mean(median, na.rm = TRUE),
              sd   = sd(median, na.rm = TRUE),
              se   = sd/sqrt(n())) %>%
    mutate(trend = ifelse(lambda < 0, ifelse(lambda + se > 0, "Stable", "Declining"), ifelse(lambda - se < 0, "Stable", "Increasing"))) %>%
    mutate(color = ifelse(trend == "Stable", "gray", ifelse(trend == "Declining", "red", "blue")))

    sum_df %>%
    ggplot(aes(x = spps, y = lambda)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_errorbar(aes(ymin = lambda - se, ymax = lambda + se), width = 0.1) +
    geom_point(aes(fill = trend), shape = 21, size = 2) +
    coord_flip() +
    theme_minimal() +
    theme(axis.text.y = element_text(face = "italic"),
          legend.title = element_blank(),
          legend.position = "top") +
    scale_fill_manual(values = c("Declining" = "red",
                                  "Stable" = "grey70",
                                  "Increasing" = "blue")) +
    xlab("") +
    ylab(expression(lambda))
  
}
