# R Thornley
# 18/08/2025
# Variance partitioning methods
# calculate variance in a data set according to different levels of organisation
# using random effects models

library(lme4)
library(tidyverse)
library(purrr)

################################################################################

# we already have the following hard traits
# Leaf Area
# Leaf Dry Mass
# Leaf Wet Weight

# create some secondary hard traits from our data
# LMA = Leaf Mass Area = dry weight of a leaf (g) / by the area (cm_2)
# SLA = Specific Leaf Area = area of the leaf (cm_2) / dry weight of a leaf (g)
# LWC - Leaf Water Content = Leaf fresh Mass minus leaf dry mass / fresh mass (can also be x 100)
# LDMC = Leaf Dry Matter Content = oven dry mass of a leaf (g) / fresh mass (g) 
# EWT = Equivalent Water Thickness = Leaf wet weight (g) minus leaf dry weight (g) / leaf area ()

# we also have some 'soft' traits that we recorded as part of the demography
# Plant Height
# Plant width_1 
# Plant width_2 
# Plant size = area = width_1 x width_2
# If none rosette forming species this is an area measurement expressed as a percentage
# these soft traits are probably best examined within a species rather than between

################################################################################

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
  mutate(EWT = (Leaf_weight_wet_g - Leaf_weight_dry_g) / Leaf_area_cm_2) 

print(head(traits, n= 10))
  
# create a unique individual ID 
# but this doesn't make sense for this model because we don't have repeated measures on an individual
traits <- traits %>% mutate(Individual = paste0(Species, "_", Treatment, "_", Time_point, "_", Individual_nos))
# traits$unique_id


ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "", y = "", fill = "Correlation")


################################################################################

# Build random effects models across the whole data set for selected traits

# make vector of traits that we are interested in
traits_to_run <- c("LMA", "LDMC", "SLA", "LWC", "EWT") 

# here we want to decompose the variance by the following:
# 1) species (inter-specific variation),
# 2) sampling times within species (intra-specific variation across time)
# 3) individual variation within species (intra-specific variation at the individual level)
# 4) treatment variation within species (intra-specific variation across disturbance regimes)

# template for random effects model
formula_template <- function(trait) {
  as.formula(paste(trait, 
                   "~ 1 + (1|Species) + (1|Species:Date) + (1|Species:Individual) + (1|Species:Treatment)"))
  }

# a function for extracting the varcomp as proportions
extract_varcomp <- function(model, trait_name) {
  vc <- as.data.frame(VarCorr(model)) %>%
    select(grp, vcov)
  
  resid_var <- sigma(model)^2
  
  vc %>%
    bind_rows(data.frame(grp = "Residual", vcov = resid_var)) %>%
    mutate(prop = vcov / sum(vcov),
           trait = trait_name)
}

# put the above together using map_dfr from purrr package
vc_all <- map_dfr(traits_to_run, function(trait_name) {
  # convert to numeric just in case
  traits[[trait_name]] <- as.numeric(traits[[trait_name]])

  # fit model
  mod <- lmer(formula_template(trait_name), data = traits)

  # extract variance decomposition
  extract_varcomp(mod, trait_name)
                        })

# tidy the variance names for plotting
vc_all <- vc_all %>% mutate(new_grp = case_when(grp == "Species:Individual" ~ "Within species",
                                          grp == "Species:Treatment" ~ "Within species between treatments",
                                          grp == "Species:Date" ~ "Within species between sampling times",
                                          grp == "Species" ~ "Between species across time",
                                          grp == "Residual" ~ "Unexplained variation"))

# plot as stacked bar plot
ggplot(vc_all, aes(x = trait, y = prop, fill = new_grp)) +
  geom_col(position = "fill") +       # stacked to 100%
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Trait", y = "Proportion of Variance",
       title = "Variance Partitioning Across Traits",
       fill = "Source of Variation") +
  theme_minimal()

ggsave("figures/variance_decomp_all_species.jpeg", plot, height = 5, width = 8)

################################################################################



# do for one species
unique(traits$Species)
# try for CA
CA <- traits %>% filter(Species == "CA")

# LDMC
LDMC_mod <- lmer(LDMC ~ 1 + 
                   (1|Date) +
                   (1|Treatment), 
                 data = CA)

# calculate the variance decomposition and plot as a stacked bar chart
vc <- as.data.frame(VarCorr(LDMC_mod))
resid_var <- sigma(LDMC_mod)^2
vc_LDMC <- rbind(vc[, c("grp","vcov")],
                 data.frame(grp = "Residual", vcov = resid_var))
vc_LDMC$prop <- vc_LDMC$vcov / sum(vc_LDMC$vcov)
vc_LDMC$trait <- "LDMC"

# LMA
LMA_mod <- lmer(LMA ~ 1 + 
                  (1|Date) +
                  (1|Treatment), 
                data = CA)

# calculate the variance decomposition and plot as a stacked bar chart
vc <- as.data.frame(VarCorr(LMA_mod))
resid_var <- sigma(LMA_mod)^2
vc_LMA <- rbind(vc[, c("grp","vcov")],
                data.frame(grp = "Residual", vcov = resid_var))
vc_LMA$prop <- vc_LMA$vcov / sum(vc_LMA$vcov)
vc_LMA$trait <- "LMA"

# plant height
Height_mod <- lmer(Height ~ 1 + 
                     (1|Date) +
                     (1|Treatment), 
                   data = CA)

# calculate the variance decomposition and plot as a stacked bar chart
vc <- as.data.frame(VarCorr(Height_mod))
resid_var <- sigma(Height_mod)^2
vc_Height <- rbind(vc[, c("grp","vcov")],
                   data.frame(grp = "Residual", vcov = resid_var))
vc_Height$prop <- vc_Height$vcov / sum(vc_Height$vcov)
vc_Height$trait <- "Height"





# bind the results for the models
dat <- rbind(vc_LMA, vc_LDMC, vc_Height)
unique(dat$grp)


# plot out the results
plot <- ggplot(dat, aes(x = trait, y = prop, fill = grp)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(x = "", y = "Proportion of Variance",
       title = "Variance Partitioning of Plant Traits in Cirsium arvense",
       fill = "Source of Variation") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()
plot
ggsave("figures/variance_decomp_CA.jpeg", plot, height = 5, width = 8)

################################################################################

library(tidyverse)
library(purrr)
library(lme4)

# generalise over lots of species
# load and tidy trait data
traits <- 
  read_csv("data/master_data/leaf_traits_final_2025.csv") %>%
  mutate(Leaf_area_cm_2 = readr::parse_number(Leaf_area_cm_2),
         Leaf_weight_dry_g = readr::parse_number(Leaf_weight_dry_g),
         Leaf_weight_wet_g = readr::parse_number(Leaf_weight_wet_g)) %>%
  drop_na(Leaf_area_cm_2, Leaf_weight_dry_g) %>%   # drop rows where conversion failed
  mutate(LMA = Leaf_weight_dry_g / Leaf_area_cm_2) %>%
  mutate(LDMC = Leaf_weight_dry_g / Leaf_weight_wet_g) %>%
  mutate(SLA = Leaf_area_cm_2 / Leaf_weight_dry_g)
names(traits)
head(traits)

traits %>% group_by

traits_list <- c("LDMC", "LMA", "Height")  # traits to model
species_list <- unique(traits$Species) # all species

# function to fit model and extract variance components
get_variance <- function(species_name, trait_name) {
  data_sub <- traits %>% filter(Species == species_name)
  
  # construct formula dynamically
  formula <- as.formula(paste(trait_name, "~ 1 + (1|Date) + (1|Treatment)"))
  
  mod <- lmer(formula, data = data_sub)
  
  vc <- as.data.frame(VarCorr(mod))
  resid_var <- sigma(mod)^2
  
  vc_df <- rbind(vc[, c("grp","vcov")],
                 data.frame(grp = "Residual", vcov = resid_var))
  vc_df$prop <- vc_df$vcov / sum(vc_df$vcov)
  vc_df$trait <- trait_name
  vc_df$Species <- species_name
  
  return(vc_df)
}

# loop over all species and traits
all_vc <- map_df(species_list, function(sp) {
  map_df(traits_list, ~ get_variance(sp, .x))
})

# check unique groups
unique(all_vc$grp)

# plot stacked bar chart for all species
plot <- ggplot(all_vc, aes(x = trait, y = prop, fill = grp)) +
  geom_bar(stat = "identity", width = 0.5) +
  facet_wrap(~Species) +
  labs(x = "", y = "Proportion of Variance",
       title = "Variance Partitioning of Plant Traits Across Species",
       fill = "Source of Variation") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

plot
ggsave("figures/variance_decomp_all_species.jpeg", plot, height = 8, width = 12)
