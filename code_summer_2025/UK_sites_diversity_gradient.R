# R Thornley
# 21/01/2024

# get site gradients of species diversity for 3 UK DRAGNet sites
# only select quadrats of control and disturbance
# only select data for T1

library(tidyverse)
library(vegan)

# now get all our dragnet data
data <- read.csv("data/full-cover-drag-2024-07-27.csv")

# vector of uk site names to filter df with
wanted_sites <- c("Ainsdale Dune Slacks", "Wytham Woods", "Hazelrigg")
wanted_trts <- c("Disturbance", "Control")

# filter data, create unique quadrat ID and put it in format for use with vegan package
div_data <- data %>% 
  filter(site_name %in% wanted_sites) %>%
  filter(trt %in% wanted_trts) %>%
  filter(year_trt == 1) %>%
  mutate(unique = paste(site_name, year_trt, trt, block, sep = "_")) %>%
  select(site_name, trt, year_trt, unique, max_cover, Taxon) %>%
  pivot_wider(names_from = Taxon, values_from = max_cover, values_fn = ~ mean(.x, na.rm = TRUE)) %>%
  replace(is.na(.), 0) %>%
  arrange(unique) 

# get vector of unique ids / plot info
data_id <- div_data %>% select(site_name, trt, year_trt, unique)

# remove id columns and perform diversity calculations
# vegan requires a matrix with only numeric data
# Species as columns and quadrats as rows

div_data <- 
  div_data %>%
  select(!unique) %>%
  select(!site_name) %>%
  select(!trt) %>%
  select(!year_trt) %>%
  mutate(shannon = vegan::diversity(., "shannon", MARGIN = 1)) %>%
  mutate(simpson = vegan::diversity(., "simpson", MARGIN = 1)) %>%
  mutate(invsimp = vegan::diversity(., "invsimpson", MARGIN = 1)) %>%
  select(shannon, simpson, invsimp) %>%
  cbind(data_id)


div_data %>% 
  group_by(site_name, trt) %>% 
  mutate(mean_simp = mean(simpson)) %>% 
  ggplot(aes(site_name, mean_simp)) +
  geom_col()+
  facet_wrap(~trt)

div_data$site_name <- factor(div_data$site_name, c("Hazelrigg", "Ainsdale Dune Slacks", "Wytham Woods"))

# look at the difference in diversity gradientss between sites
# quadrat level simpsons diversity with in T1 between control and disturbance
ggplot(div_data, aes(site_name, simpson)) +
  theme_bw()+
  geom_boxplot()+
  facet_wrap(~trt)+
  ggtitle("Site Level Simpson's Index")
