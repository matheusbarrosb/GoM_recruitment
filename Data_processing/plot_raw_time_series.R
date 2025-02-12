require(here)
require(dplyr)
require(ggplot2)

data_directory = file.path(here::here(), "Data", "FAMP_trawl.csv")
raw_data       = read.csv(data_directory)

spp_list = levels(as.factor(raw_data$Genus_species))

selected_spps = list(spp_list[33], spp_list[34], spp_list[35], spp_list[70],
                     spp_list[84], spp_list[104], spp_list[235], spp_list[297],
                     spp_list[303], spp_list[480], spp_list[376], spp_list[173],
                     spp_list[84], spp_list[174], spp_list[251], spp_list[124],
                     spp_list[123], spp_list[230], spp_list[283], spp_list[262],
                     spp_list[452])


raw_data %>%
  filter(Genus_species %in% selected_spps) %>%
  group_by(Genus_species, YEAR, MONTH, STATION, DAY) %>%
  summarise(n = Total_NUM) %>%
  na.exclude() %>%
  group_by(Genus_species, YEAR) %>%
  summarise(mean_count = mean(n, na.rm = TRUE),
            sd         = sd(n, na.rm = TRUE),
            n          = n(),
            se         = sd/sqrt(n())) %>%
  
  ggplot(aes(x = YEAR, y = mean_count)) +
  geom_line() +
#  geom_point() +
#  geom_ribbon(aes(ymin = mean_count - 2*se, ymax = mean_count + 2*se), alpha = 0.4) +
  facet_wrap(~Genus_species, scales = "free_y") +
  theme_minimal() +
  xlab("Year") + ylab("Mean count")

ggsave("raw_time_series.pdf", path = file.path(here::here(), "Exploratory_plots"))