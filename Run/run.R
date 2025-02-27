require(here)
require(dplyr)
require(ggplot2)
require(cmdstanr)

# Load-in functions ------------------------------------------------------------
function_directory = file.path(here::here(), "R/")
function_files     = list.files(function_directory)
for (i in 1:length(function_files)) source(paste0(function_directory, function_files[i]))

data_directory = file.path(here::here(), "Data", "FAMP_trawl.csv")
raw_data       = read.csv(data_directory)

# Select species ---------------------------------------------------------------
spp_list = levels(as.factor(raw_data$Genus_species))

selected_spps = list(spp_list[33], spp_list[34], spp_list[35], spp_list[59], 
                     spp_list[70], spp_list[84], spp_list[91], spp_list[104],
                     spp_list[123], spp_list[124], spp_list[173], spp_list[174],
                     spp_list[230], spp_list[235], spp_list[251], spp_list[262], 
                     spp_list[263], spp_list[264], spp_list[283], spp_list[297],
                     spp_list[365], spp_list[376], spp_list[480])

#selected_spps = list(spp_list[33], spp_list[262])

# Make input data for model ----------------------------------------------------
input_data = make_input_data(raw_data               = raw_data,
                             species_list           = selected_spps,
                             standardize            = TRUE, 
                             shared_trends          = FALSE,
                             standardize_covariates = FALSE,
                             log_covariates         = FALSE,
                             overdispersion         = FALSE,
                             family                 = 1 # 1 = gaussian, 4 = gamma, 5 = lognormal
                             )

# Configure and fit model ------------------------------------------------------
model_directory = file.path(here::here(), "Stan", "MARSS_gompertz.stan")
model_file      = cmdstan_model(model_directory)

fit = model_file$sample(
  data            = input_data$stan_input,
  seed            = 2025,
  chains          = 2,
  parallel_chains = 2,
  iter_warmup     = 500,
  iter_sampling   = 2000,
  adapt_delta     = 0.99,
  step_size       = 0.05,
  refresh         = 10,   # refresh output progress display every X iterations
  max_treedepth   = 15    # I suggest a minimum of 15 to avoid convergence problems
); fit$cmdstan_diagnose() # check convergence diagnostics

# Plot results -----------------------------------------------------------------
figure_directory = file.path(here::here(), "Figures")

plot_fits_to_data(stan_input   = input_data,
                  fit          = fit,
                  species_list = selected_spps,
                  log          = FALSE)
ggsave("fits_to_data.png", width = 9.2, height = 5.5, path = figure_directory)
ggsave("fits_to_data.pdf", width = 9.2, height = 5.5, path = figure_directory)

plot_lambdas(fit          = fit,
             species_list = selected_spps,
             stan_input   = input_data,
             plot_type    = "")
ggsave("lambdas.png", width = 6, height = 5.5, path = figure_directory)
ggsave("lambdas.pdf", width = 6, height = 5.5, path = figure_directory)

plot_lambdas(fit          = fit,
             species_list = selected_spps,
             stan_input   = input_data,
             plot_type    = "time_series")
ggsave("lambda_ts.png", width = 9, height = 5.5, path = figure_directory)
ggsave("lambda_ts.pdf", width = 9, height = 5.5, path = figure_directory)

plot_covariate_effects(fit          = fit,
                       species_list = selected_spps, 
                       stan_input   = input_data$stan_input)
ggsave("covariates.png", width = 10.4, height = 5.5, path = figure_directory)
ggsave("covariates.pdf", width = 10.4, height = 5.5, path = figure_directory)
