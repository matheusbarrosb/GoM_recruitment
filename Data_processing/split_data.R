require(here)
require(dplyr)

data_directory = file.path(here::here(), "Data", "FAMP_trawl.csv")
raw_data       = read.csv(data_directory)

spp_list = levels(as.factor(raw_data$Genus_species))

selected_spps = list(spp_list[33], spp_list[34], spp_list[35], spp_list[70],
                     spp_list[84], spp_list[104], spp_list[235], spp_list[297],
                     spp_list[303], spp_list[480], spp_list[376], spp_list[173],
                     spp_list[84], spp_list[174], spp_list[251], spp_list[124])



data_list = list()
for (i in 1:length(selected_spps)) data_list[[i]] = raw_data %>% filter(Genus_species %in% selected_spps[[i]])  

data_directory2 = file.path(here::here(), "Data/")
file_names      = paste0(selected_spps, ".csv")
file_names      = chartr(" ", "_", file_names)
file_names      = paste0(data_directory2, file_names)
for (i in 1:length(selected_spps)) write.csv(data_list[[i]], file = file_names[i])