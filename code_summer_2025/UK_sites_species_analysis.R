# R Thornley
# 08/04/2025
# For each of the UK sites 
library(tidyverse)
library(viridis) # for the plotting colour palette

# dragnet cover data load
drag <- read.csv("data/full-cover-drag-2024-07-27.csv")
unique(drag$year_trt)

# before filtering the dragnet data set has
drag %>% summarize(unique_values = n_distinct(site_name)) # 47 sites
drag %>% summarize(unique_values = n_distinct(Taxon)) # 1293 species
# BUT these numbers include subspecies etc..

# list some of these non taxon entries to filter out in the following pipe
none_taxa <- c("Fungi", "Other_litter", "Other_standing_water", "Ground", "Bryophyte", 
               "Other_animal_diggings", "Other_woody_overstory", "Lichen",
               "Other_animal_digging", "Other_animal_droppings", "Other_rock")

# mutate the Taxon list in the Drag data so it is formatted well
# filter for the non taxonomic entries
# select the relevant cols
drag <- drag %>%
  mutate(New_taxon = str_to_sentence(Taxon)) %>%
  mutate(New_taxon = str_replace_all(New_taxon, " ", "_")) %>%
  filter(!str_detect(New_taxon, ".sp")) %>% # get rid of entries not to taxon level
  filter(!str_detect(New_taxon, "_x_")) %>% # get rid of any hybrids
  filter(!str_detect(New_taxon, "Unknown")) %>% # get rid of unknown species
  filter(!New_taxon %in% none_taxa) %>% # get rid of non taxa entries 
  mutate(New_taxon = case_when(New_taxon == "Helianthemum_nummularium_var._Grandiflorum" ~ "Helianthemum_nummularium",
                               New_taxon == "Mimosa_quadrivalvis_var._Platycarpa" ~ "Mimosa_quadrivalvis",
                               New_taxon == "Sebaea_sedoides_var._Schoenlandii" ~ "Sebaea_sedoides", 
                               TRUE ~ New_taxon)) %>%
  arrange(New_taxon) %>%
  select(site_name, year_trt, trt, block, plot, max_cover, New_taxon)

# filter for only the UK sites
names(drag)
unique(drag$site_name)
drag <- drag %>% filter(site_name %in% c("Wytham Woods", "Hazelrigg", "Ainsdale Dune Slacks"))

# For each year in each site
# get the percentage of the total plant cover of all species

# look at the total cover per site of each species
taxa_cover <- drag %>% group_by(site_name, year_trt, New_taxon, trt) %>% nest() %>% 
  mutate(total_cover_plot = map(data, ~ .x %>% summarise(total = sum(max_cover)))) %>%
  select(site_name:New_taxon, total_cover_plot) %>%
  unnest(cols = c("total_cover_plot"))

taxa_cover %>% group_by(site_name) %>% summarise(max(year_trt))
# WW = 2
# HR = 1
# AD = 1

taxa_cover %>% filter(site_name == "Wytham Woods" & year_trt == 2) %>%
  group_by(trt, New_taxon) %>% summarise(max_total = max(total)) %>% arrange(-max_total)


see <- taxa_cover %>%
  group_by(site_name) %>%
  group_split()

# Optionally, save each split data frame as a CSV
for (i in seq_along(see)) {
  # Define file name
  file_name <- paste0("results/", unique(see[[i]]$site_name), ".csv")
  
  # Write each group to a separate CSV file
  write.csv(see[[i]], file = file_name, row.names = FALSE)
}


see %>% imap(~ write_csv(.x, paste0("results/", .y, '.csv')))


see <- taxa_cover %>% group_by(site_name, trt) %>% group_split()

all_sites_5 <-
  taxa_cover %>% group_by(site_name, year_trt, trt) %>% mutate(sum = sum(total)) %>% 
  mutate(prop = total/sum *100) %>% # divide total amount by the total veg over recorded
  select(site_name, year_trt, trt, New_taxon, prop) %>%
  group_by(site_name, year_trt, trt) %>% slice_max(order_by = prop, n = 5) %>% 
  summarise(top_05_sum = sum(prop)) 

all_sites_8 <-
  taxa_cover %>% group_by(site_name, year_trt, trt) %>% mutate(sum = sum(total)) %>% 
  mutate(prop = total/sum *100) %>% # divide total amount by the total veg over recorded
  select(site_name, year_trt, trt, New_taxon, prop) %>%
  group_by(site_name, year_trt, trt) %>% slice_max(order_by = prop, n = 8) %>% 
  summarise(top_08_sum = sum(prop)) 

all_sites_10 <-
  taxa_cover %>% group_by(site_name, year_trt, trt) %>% mutate(sum = sum(total)) %>% 
  mutate(prop = total/sum *100) %>% # divide total amount by the total veg over recorded
  select(site_name, year_trt, trt, New_taxon, prop) %>%
  group_by(site_name, year_trt, trt) %>% slice_max(order_by = prop, n = 10) %>% 
  summarise(top_10_sum = sum(prop)) 

all_sites_15 <-
  taxa_cover %>% group_by(site_name, year_trt, trt) %>% mutate(sum = sum(total)) %>% 
  mutate(prop = total/sum *100) %>%
  select(site_name, year_trt, New_taxon, trt, prop) %>%
  group_by(site_name, year_trt, trt) %>% slice_max(order_by = prop, n = 15) %>% 
  summarise(top_15_sum = sum(prop)) 

all_sites_20 <-
  taxa_cover %>% group_by(site_name, year_trt, trt) %>% mutate(sum = sum(total)) %>% 
  mutate(prop = total/sum *100) %>%
  select(site_name, year_trt, New_taxon, trt, prop) %>%
  group_by(site_name, year_trt, trt) %>% slice_max(order_by = prop, n = 20) %>% 
  summarise(top_20_sum = sum(prop)) 

all_sites_30 <-
  taxa_cover %>% group_by(site_name, year_trt, trt) %>% mutate(sum = sum(total)) %>% 
  mutate(prop = total/sum *100) %>%
  select(site_name, year_trt, New_taxon, trt, prop) %>%
  group_by(site_name, year_trt, trt) %>% slice_max(order_by = prop, n = 30) %>% 
  summarise(top_30_sum = sum(prop)) 

dfs <- list(all_sites_5, all_sites_8, all_sites_10, all_sites_15, all_sites_20, all_sites_30)
dat <- reduce(dfs, left_join)
names(dat)
dat <- dat %>% pivot_longer(cols = top_05_sum:top_30_sum)

unique(dat$site_name)
# pick some sites across the evenness / diversity gradient
# Wytham has high richness and evenness
# Piedmont Prairie has low richness and evenness
# Agroscope Changins has medium levels of richness and evenness

site_wanted <- c("Wytham Woods", "Piedmont Prairie", "Agroscope Changins")
levels_wanted <- c("top_05_sum", "top_10_sum", "top_08_sum", "top_15_sum")
dat %>% 
  filter(site_name %in% site_wanted) %>%
  filter(name %in% levels_wanted) %>%
  mutate(plot_label = case_when(site_name == "Piedmont Prairie" ~ "Piedmont: rich 24, even 0.7",
                                site_name == "Agroscope Changins" ~ "Agroscope: rich 43, even 0.8",
                                site_name == "Wytham Woods" ~ "Wytham: rich 84, even 0.9")) %>%
  mutate(plot_label = factor(plot_label, 
                             levels = c("Piedmont: rich 24, even 0.7", 
                                        "Agroscope: rich 43, even 0.8",
                                        "Wytham: rich 84, even 0.9"))) %>%
  mutate(new_names = case_when(name == "top_05_sum" ~ "5 species sampled",
                               name == "top_08_sum" ~ "8 species sampled",
                               name == "top_10_sum" ~ "10 species sampled",
                               name == "top_15_sum" ~ "15 species sampled")) %>%
  mutate(new_names = factor(new_names, levels = c("5 species sampled", "8 species sampled",
                                                  "10 species sampled", "15 species sampled"))) %>%
  ggplot(aes(year_trt, value, group = new_names, colour = new_names)) + 
  geom_point() + geom_line() + theme_bw()+
  facet_grid(trt ~ plot_label)+
  xlim(0, 3)+
  ylim(30, 105)+
  ylab("Percentage of site level cover capture")+
  geom_hline(yintercept = 80, colour = "black", linetype = "dashed")+
  theme(legend.title = element_blank())+
  xlab("Year of experiment")+
  theme(strip.background = element_rect(fill = "white", color = "black"))+
  scale_y_continuous(breaks = seq(30, 100, by = 10))+
  scale_color_viridis_d(option = "D")+
  theme(strip.text = element_text(size = 10))+
  theme(legend.text = element_text(size = 12))

ggsave("Leaf_traits_CSR_number_of_species_needed_3 sites.jpeg", height = 8, width =9)
