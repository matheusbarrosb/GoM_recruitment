require(dplyr)
require(lubridate)
library(ggplot2)

data_directory = file.path(here::here(), "Data", "FAMP_trawl.csv")
raw_data       = read.csv(data_directory)

selected_spps = list(spp_list[33], spp_list[34], spp_list[35])

# selected_spps = list(spp_list[33], spp_list[34], spp_list[35], spp_list[59],
#                      spp_list[70], spp_list[84], spp_list[91], spp_list[104],
#                      spp_list[123], spp_list[124], spp_list[173], spp_list[174],
#                      spp_list[230], spp_list[235], spp_list[251], spp_list[262],
#                      spp_list[263], spp_list[264], spp_list[283], spp_list[297],
#                      spp_list[365], spp_list[376], spp_list[480])

get_cumulative_data <- function(raw_data, selected_spps) {
  filtered_data <- raw_data %>%
    filter(Genus_species %in% selected_spps) %>%
    mutate(Date = as.Date(paste(YEAR, MONTH, DAY, sep = "-"))) %>%
    mutate(DOY = yday(Date)) %>%        
    group_by(Genus_species, YEAR, MONTH, DAY, DOY) %>%  
    summarise(
      mu_count  = mean(Total_NUM, na.rm = TRUE),
      se_count = sd(Total_NUM, na.rm = TRUE)/sqrt(n()),
      n = n()
    )
  
  names(filtered_data) <- c(
    "species", "year", "month", "day", "doy",
    "count", "se", "N"
  )
  
  cumulative_data <- filtered_data %>%
    group_by(species, year) %>%
    arrange(doy) %>%
    mutate(cumulative_count = cumsum(count)) %>%
    ungroup()
  
  max_counts <- cumulative_data %>%
    group_by(species, year) %>%
    summarise(max_count = max(cumulative_count, na.rm = TRUE), .groups = 'drop')
  
  cumulative_data <- cumulative_data %>%
    left_join(max_counts, by = c("species", "year")) %>%
    mutate(relative_cumulative_count = cumulative_count / max_count)
  
  # # put data in a list for stan
  # output = list
  
  return(cumulative_data)
}

# test function
test_data <- get_cumulative_data(raw_data, selected_spps)


# plot
ggplot(test_data, aes(x = doy, y = relative_cumulative_count, color = species)) +
  geom_point(size = 0.25) +
  labs(title = "Relative Cumulative Counts by Day of Year",
       x = "Day of Year",
       y = "Relative Cumulative Count") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~ year, scales = "free_y")


test_data = na.exclude(test_data)

stan_data = list(
  N = length(unique(test_data$year)),
  S = length(unique(test_data$species)),
  n_pos = nrow(test_data),
  row_indx_pos = as.integer(factor(test_data$year, levels = unique(test_data$year))),
  col_indx_pos = as.integer(factor(test_data$species, levels = unique(test_data$species))),
  y = test_data$relative_cumulative_count,
  doy = test_data$doy
)

# fit
library(rstan)
stan_model_path = file.path(here::here(),"stan", "phenology_opt_non_centered.stan") 
stan_model <- stan_model(stan_model_path)

fit = sampling(stan_model, 
                data      = stan_data, 
                iter      = 5000,
                warmup    = 500,
                chains    = 2, 
                control   = list(adapt_delta = 0.95),
                refresh   = 100,
                algorithm = "NUTS",
                cores     = 2)

# extract model predictions and plot against data
predictions <- rstan::extract(fit, pars = "y_rep")$y_rep

# get means and sds
# size of mean and sds for predictions should be = 1321 (n_pos)
predictions_mean <- apply(predictions, 2, mean)
predictions_sd <- apply(predictions, 2, sd)
ob_served_data <- test_data$relative_cumulative_count

# create a dataframe for plotting
data.frame(
  doy = test_data$doy,
  species = test_data$species,
  year = test_data$year,
  mean = predictions_mean,
  sd = predictions_sd,
  observed = ob_served_data
) %>%
  
  ggplot(aes(x = doy, y = mean)) +
  geom_line(color = "red") +
  geom_ribbon(aes(ymin = mean - 2*sd, ymax = mean + 2*sd), alpha = 0.2) +
  geom_point(aes(y = observed), size = 0.5, shape = 1) +
  labs(x = "Day of Year",
       y = "Relative Cumulative Count") +
  custom_theme() +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~ year, ncol = 8) +
  ylim(0,1.2) +
  scale_y_continuous(breaks = seq(0, 1.2, by = 0.5))


# plot time series of 'doy_50' parameter from model fit
# extract the 'doy_50' parameter from the model fit not using 'extract' function

doy_50 <- rstan::extract(fit, pars = "doy_50")$doy_50

# dim(day_50) outputs this: [1] 18000    38     1, and I need to create a dataframe with the mean estimate for each year (n = 38) across all 1800 samples
day_50_mean <- apply(doy_50, c(2, 3), mean)
doy_50_sd <- apply(doy_50, c(2, 3), sd)

data.frame(
  year = unique(test_data$year),
  doy_50 = day_50_mean,
  doy_50_sd = doy_50_sd
) %>%
  
  ggplot(aes(x = year, y = doy_50)) +
  geom_line() +
  geom_ribbon(aes(ymin = doy_50 - 2*doy_50_sd, ymax = doy_50 + 2*doy_50_sd), alpha = 0.2) +
  custom_theme() +
  xlab("Year") +
  ylab("Day of Year")
