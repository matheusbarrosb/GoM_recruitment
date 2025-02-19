require(here)
require(dplyr)
require(ggplot2)
require(cmdstanr)

function_directory = file.path(here::here(), "R/")
function_files     = list.files(function_directory)
for (i in 1:length(function_files)) source(paste0(function_directory, function_files[i]))

data_directory = file.path(here::here(), "Data", "FAMP_trawl.csv")
raw_data       = read.csv(data_directory)

spp_list = levels(as.factor(raw_data$Genus_species))

selected_spps = list(spp_list[33], spp_list[34], spp_list[35], spp_list[59], spp_list[70],
                     spp_list[84], spp_list[90], spp_list[91], spp_list[104],
                     spp_list[123], spp_list[124], spp_list[173], spp_list[174],
                     spp_list[230], spp_list[235], spp_list[251], spp_list[262], 
                     spp_list[263], spp_list[264], spp_list[283], spp_list[297],
                     spp_list[365], spp_list[376], spp_list[480])

input_data = make_input_data(raw_data      = raw_data,
                             species_list  = selected_spps,
                             standardize   = TRUE, 
                             shared_trends = FALSE)
mcmc_list = list(n_mcmc = 1000, n_burn = 100, n_chain = 2,
                  n_thin = 1, step_size = 0.4, adapt_delta = 0.9)

model_directory = file.path(here::here(), "Stan", "MARSS.stan")
model_file      = cmdstan_model(model_directory)

fit = model_file$sample(
  data            = input_data$stan_input,
  seed            = 2025,
  chains          = mcmc_list$n_chain,
  parallel_chains = mcmc_list$n_chain,
  iter_warmup     = mcmc_list$n_burn,
  iter_sampling   = mcmc_list$n_mcmc,
  adapt_delta     = 0.97,
  step_size       = 0.05,
  refresh         = 100,
  max_treedepth   = 20
)


plot_fits_to_data(stan_input   = input_data$stan_input,
                  fit          = fit,
                  species_list = selected_spps)

plot_growth_rates(fit          = fit,
                  species_list = selected_spps,
                  stan_input   = input_data$stan_input)


plot_covariate_effects(fit          = fit,
                       species_list = selected_spps, 
                       stan_input   = input_data$stan_input)





