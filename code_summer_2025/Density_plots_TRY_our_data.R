# R Thornley
# 22/08/2025
# Look at the distribution of data points from TRY and our data for the ket dry leaf traits

library(tidyverse) 

# read in the master trait data and create the additional trait values #
our_traits <- read_csv("data/master_data/leaf_traits_final_2025.csv")

# create the secondary traits from the raw data
our_traits <- 
  our_traits %>%
  mutate(Leaf_area_cm_2 = readr::parse_number(Leaf_area_cm_2), # function parse_number converts to numbers from text strings
         Leaf_weight_dry_g = readr::parse_number(Leaf_weight_dry_g),
         Leaf_weight_wet_g = readr::parse_number(Leaf_weight_wet_g)) %>%
  drop_na(Leaf_area_cm_2, Leaf_weight_dry_g) %>%   # drop rows where conversion to numeric  failed
  mutate(LMA = Leaf_weight_dry_g / Leaf_area_cm_2) %>%
  mutate(LDMC = Leaf_weight_dry_g / Leaf_weight_wet_g) %>%
  mutate(SLA = Leaf_area_cm_2 / Leaf_weight_dry_g) %>%
  mutate(LWC = (Leaf_weight_wet_g - Leaf_weight_dry_g) / Leaf_weight_wet_g) %>% 
  mutate(EWT = (Leaf_weight_wet_g - Leaf_weight_dry_g) / Leaf_area_cm_2)
names(our_traits)

# we have some unrealistic values
range(our_traits$LDMC)


our_traits <- 
  our_traits %>% 
  pivot_longer(cols = c(LMA, LDMC, SLA, LWC, EWT), 
                                          names_to = "New_trait_name", values_to = "StdValue")

print(head(our_traits, n= 10))

our_traits <- our_traits %>% select(Species, New_trait_name, StdValue)
our_traits$data_set <- "DRAGNet"

our_traits <- our_traits %>% filter(New_trait_name %in% c("LDMC", "SLA"))

range(our_traits$StdValue)

################################################################################

# load in the TRY trait data
TRY_traits <- read_csv("results/selected_European_data_TRY_our_species.csv")
names(TRY_traits)

TRY_traits <- TRY_traits %>% select(AccSpeciesName, New_trait_name, StdValue)
TRY_traits %>% group_by(New_trait_name) %>% tally()
ggplot(TRY_traits, aes(StdValue)) + geom_density() + facet_wrap(~ New_trait_name, scales = "free")

# join the two data sets and make a coloured density plot
unique(TRY_traits$AccSpeciesName)
unique(TRY_traits$New_trait_name)
TRY_traits <- TRY_traits %>% filter(New_trait_name %in% c("LDMC", "SLA"))
TRY_traits <- 
  TRY_traits %>% mutate(Species = case_when(AccSpeciesName == "Agrimonia eupatoria" ~ "AE",
                                          AccSpeciesName == "Anthoxanthum odoratum" ~ "AO",
                                          AccSpeciesName == "Brachypodium pinnatum" ~ "BP",
                                          AccSpeciesName == "Brachypodium sylvaticum" ~ "BS",
                                          AccSpeciesName == "Cirsium arvense" ~ "CA",
                                          AccSpeciesName == "Clinopodium vulgare" ~ "CV",
                                          AccSpeciesName == "Equisetum palustre" ~ "EP",
                                          AccSpeciesName == "Hydrocotyle vulgaris" ~ "HV",
                                          AccSpeciesName == "Lotus corniculatus" ~ "LC",
                                          AccSpeciesName == "Mentha aquatica" ~ "MA",
                                          AccSpeciesName == "Primula veris" ~ "PV",
                                          AccSpeciesName == "Pulicaria dysenterica"  ~ "PD",
                                          AccSpeciesName == "Ranunculus repens" ~ "RR",
                                          AccSpeciesName == "Rumex acetosa"  ~ "RA",
                                          AccSpeciesName == "Salix repens"  ~ "SR",
                                          AccSpeciesName == "Taraxacum campylodes" ~ "TA",
                                          AccSpeciesName == "HYACINTHOIDES NON-SCRIPTA" ~ "HN"))
names(TRY_traits)
TRY_traits$data_set <- "TRY"
TRY_traits<- TRY_traits %>% dplyr::select(New_trait_name:data_set)

# join the DRAGNet and the TRY data sets
all <- rbind(TRY_traits, our_traits)

max(all$StdValue)

# visualise this
ggplot(all, aes(StdValue, fill = data_set)) + geom_density() + 
  facet_wrap(~ New_trait_name, scales = "free")

################################################################################

# There are some unrealistic values in this data set
  
# this function lists the outliers from the boxplot
boxplot.stats(our_traits$LDMC)$out

library(rstatix)
our_traits %>% identify_outliers(LDMC)
