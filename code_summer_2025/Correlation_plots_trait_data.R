# R Thornley
# 20/08/2025
# Create traits and examine correlations etc...

library(tidyverse)
library(reshape2)

# read in the master trait data and create the additional trait values #
traits <- read_csv("data/master_data/leaf_traits_final_2025.csv")
names(traits)

# create the secondary traits from the raw data
traits <- 
  traits %>%
  mutate(Leaf_area_cm_2 = readr::parse_number(Leaf_area_cm_2), # function parse_number converts to numbers from text strings
         Leaf_weight_dry_g = readr::parse_number(Leaf_weight_dry_g),
         Leaf_weight_wet_g = readr::parse_number(Leaf_weight_wet_g)) %>%
  drop_na(Leaf_area_cm_2, Leaf_weight_dry_g) %>%   # drop rows where conversion to numeric  failed
  mutate(LMA = Leaf_weight_dry_g / Leaf_area_cm_2) %>%
  mutate(LDMC = Leaf_weight_dry_g / Leaf_weight_wet_g) %>%
  mutate(SLA = Leaf_area_cm_2 / Leaf_weight_dry_g) %>%
  mutate(LWC = (Leaf_weight_wet_g - Leaf_weight_dry_g) / Leaf_weight_wet_g) %>% 
  mutate(EWT = (Leaf_weight_wet_g - Leaf_weight_dry_g) / Leaf_area_cm_2) %>% 
  rename(AL = Leaf_area_cm_2, LWW = Leaf_weight_wet_g, ML  = Leaf_weight_dry_g)

################################################################################

# get correlations for all species

trait_vars <- traits %>% select(AL, ML, LWW, SLA, LMA, LWC, EWT)
cor_matrix <- cor(trait_vars, use = "pairwise.complete.obs")
cor_melt <- melt(cor_matrix)

cor_plot_all_sp <-
  ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "", y = "", fill = "Correlation")
cor_plot_all_sp
# ggsave("figures/cor_plot_all_sp.jpeg", cor_plot_all_sp, height = 5, width = 5)

################################################################################

# find the correlations for individual species

# function to compute correlations for one species
get_species_cor <- function(df, sp) {
  df_sub <- df %>%
    filter(Species == sp) %>%
    select(where(is.numeric))
  
  # skip if not enough rows
  if (nrow(df_sub) < 3) return(NULL)
  
  cor_mat <- cor(df_sub, use = "pairwise.complete.obs")
  cor_melt <- melt(cor_mat)
  cor_melt$Species <- sp
  return(cor_melt)
}

# apply across all species
all_cors <- bind_rows(
  lapply(unique(traits$Species), function(sp) get_species_cor(traits, sp))
)

# plot faceted heatmap
cor_plot_each_species <-
  ggplot(all_cors, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       limits = c(-1, 1)) +
  facet_wrap(~Species) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text = element_text(face = "bold")) +
  labs(x = "", y = "", fill = "Correlation",
       title = "Trait Correlation Matrices by Species")
cor_plot_each_species
# ggsave("figures/cor_plot_each_species.jpeg", cor_plot_each_species, height = 8, width = 10)

################################################################################

# plot only for selected species - here BS and AE

cor_BS_AE <-
  all_cors %>%
  filter(Species %in% c("BS", "AE"))
cor_plot_BS_AE <-
  ggplot(cor_BS_AE, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       limits = c(-1, 1)) +
  facet_wrap(~Species) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
        strip.text = element_text(face = "bold", size = 14)) +
  labs(x = "", y = "", fill = "Correlation",
       title = "")
cor_plot_BS_AE
# ggsave("figures/cor_plot_BS_AE.jpeg", cor_plot_BS_AE, height = 4, width = 7)


################################################################################

# plot the trait correlations for one species over time

# select species
species_name <- "BS"

# get unique time points
time_points <- unique(traits$Time_point)

# function to compute correlations for one time point
get_cor_time <- function(df, sp, time) {
  df_sub <- df %>%
    filter(Species == sp, Time_point == time) %>%
    select(where(is.numeric))
  
  # skip if not enough rows
  if (nrow(df_sub) < 3) return(NULL)
  
  cor_mat <- cor(df_sub, use = "pairwise.complete.obs")
  cor_melt <- melt(cor_mat)
  
  # keep only upper triangle
  cor_melt <- cor_melt %>%
    filter(as.numeric(factor(Var1, levels = unique(Var1))) <= 
             as.numeric(factor(Var2, levels = unique(Var2))))
  
  cor_melt$Date <- time
  return(cor_melt)
}

# compute correlations for all time points
cor_time_all <- bind_rows(lapply(time_points, function(t) get_cor_time(traits, species_name, t)))

# plot faceted by time
ggplot(cor_time_all, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       limits = c(-1, 1)) +
  facet_wrap(~ Date) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text = element_text(face = "bold", size = 14)) +
  labs(x = "", y = "", fill = "Correlation",
       title = paste("Trait Correlation Matrices Over Time for", species_name))

#################################################################################

