# R Thornley 
# 05/02/2025
# Get master data from DRAGNet to use in modelling

library(tidyverse) 

# load and tidy data

# Read in latest DRAGNet data and tidy up vector of unique taxa to match format of compadre
# create vector of none taxa entries in cover data
none_taxa <- c("Fungi", "Other_litter", "Other_standing_water", "Ground", "Bryophyte", 
               "Other_animal_diggings", "Other_woody_overstory", "Lichen",
               "Other_animal_digging", "Other_animal_droppings")

Drag <- read.csv("data/full-cover-drag-2024-07-27.csv") %>%
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
  arrange(New_taxon) 
names(Drag)
years <-
  Drag %>% 
  select(site_name, site_code, trt, year, year_trt) %>% 
  group_by(site_name, year, year_trt) %>%
  summarise()
years$site_name
my_sites <- c("Wytham Woods", "Ainsdale Dune Slacks", "Hazelrigg", "Caracoles", "Algaida")
my_sites <- years %>% filter(site_name %in% my_sites)
unique(my_sites$year)
my_sites$year_factor <- as.factor(my_sites$year)
my_sites$year_factor <- fct_relevel(my_sites$year_factor, "2020", "2021", "2022", "2023", "2024")
my_sites$year_trt<- as.factor(my_sites$year_trt)
my_sites$year_trt <- fct_relevel(my_sites$year_trt, "-1", "0", "1", "2", "3", "4")
names(my_sites)

# we need to impute some values as the current data frame only has data up to 2023
my_sites_2 <- data.frame(site_name = c("Wytham Woods", "Hazelrigg", "Caracoles", "Algaida", "Ainsdale Dune Slacks"),
                         year_factor = c("2025"), 
                         year_trt = c("4", "3", "4", "5", "3"))
my_sites_3 <- data.frame(site_name = c("Wytham Woods", "Hazelrigg", "Caracoles", "Algaida", "Ainsdale Dune Slacks"),
                         year_factor = c("2024"), 
                         year_trt = c("3", "2", "3", "4", "2"))
my_sites_4 <- rbind(my_sites, my_sites_2, my_sites_3)
names(my_sites_4)
my_sites_4 <- my_sites_4 %>% rename("year treatment" = "year_trt")

ggplot(my_sites_4, aes(year_factor, site_name, colour = year_trt))+
  theme_bw()+
  theme(text = element_text(size = 16))+
  geom_point(size = 8)+
  scale_colour_manual(values = c("darkblue", "lightblue", "darkgreen", "lightgreen", "yellow", "orange", "red"),
                      name = "Year of Experiment")+
  ylab("site")+
  xlab("year")
ggsave("figures/DRAGNet_subnetwork_sampling_times.jpeg", height = 5, width = 8)  
