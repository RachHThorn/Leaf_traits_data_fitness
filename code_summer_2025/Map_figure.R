# R Thornley
# 18/08/2025
# Maps of global locations

library(rnaturalearth)
library(rnaturalearthdata)
library(purrr)
library(tidyverse)
library(ggrepel)

sites_wanted <- c("Ainsdale Dune Slacks", "Hazelrigg", "Wytham Woods")
dat <- read_csv("data/site-info-drag-2024-07-29.csv") %>%
  filter(site_name %in% sites_wanted)
write_csv(dat, "figures/UK_site_locations.csv")

world <- ne_countries(scale = "medium", returnclass = "sf")
theme_set(theme_classic())
map_fig <- ggplot(data = world) +
  geom_sf()+
  geom_point(data = dat, aes(x = longitude, y = latitude), size = 3, colour = "red")+
  coord_sf(xlim = c(-12, 5), ylim = c(48, 60), expand = FALSE)+
  geom_text_repel(data = dat, 
             aes(x = longitude, y = latitude, label = site_name),
             size = 3)
ggsave("figures/UK_sites_map.jpeg", map_fig, height = 5, width = 5)

################################################################################