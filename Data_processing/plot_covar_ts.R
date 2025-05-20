require(here)
require(dplyr)
require(ggplot2)
require(cmdstanr)
require(GGally)
require(tidyr)

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

# Make input data for model ----------------------------------------------------
input_data = make_input_data(raw_data               = raw_data,
                             species_list           = selected_spps,
                             standardize            = TRUE,
                             log                    = FALSE,
                             shared_trends          = TRUE,
                             standardize_covariates = FALSE,
                             log_covariates         = FALSE,
                             overdispersion         = FALSE,
                             family                 = 1 # 1 = gaussian, 4 = gamma, 5 = lognormal
)

# Plot covariates --------------------------------------------------------------
labels = c(
  days_above = "Days above 75th temperature percentile",
  do = "Dissolved oxygen (mg/L)",
  max_temp = "Maximum temperature (Celsius)",
  sal = "Salinitiy",
  temp = "Temperature (Celsius)"
)

covar_df = input_data$df[,c(2,5,6,7,8,9)]

covar_df %>%
  pivot_longer(cols = 2:6,
               names_to = "covariate",
               values_to = "value") %>%
  group_by(year, covariate) %>%
  summarize(
    mean = mean(value, na.rm = TRUE),
    se   = sd(value, na.rm = TRUE)/sqrt(n())
  ) %>%
  
  ggplot(aes(x = year, y = mean)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean - 2*se, ymax = mean + 2*se), alpha = 0.2) +
  facet_wrap(~ covariate,
             scales   = "free_y",
             labeller = labeller(covariate = labels)) +
  custom_theme() +
  ylab("Covariate value") +
  xlab("Year")

ggsave("covariate_time_series.pdf", path = file.path(here::here(), "Exploratory_plots"))



