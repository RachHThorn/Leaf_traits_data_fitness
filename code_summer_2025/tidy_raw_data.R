# Rachael Thornley
# 29/07/2025
# Leaf traits 
# Tidy Raw data from 2025 field season into long tidy format 
# for evodemos first stage modelling

library(tidyverse)



# read in the names of the excel tabs
dat <- read_csv("data/raw_field_data_2025/demography/Ainsdale_demography_2025.csv")
