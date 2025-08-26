# R Thornley
# 19/01/2024

# get 3 site gradients of rainfall and max temperature

# process all HadUK ceda files to data frame for monthly weather 
# NOTE: ceda files are large and are stored outside the R project
# produce tidy df with coordinates, x, y, site names, date, rainfall, max and min surface temp

library(tidyverse)
library(terra)
library(data.table)



# FUNCTION #

################################################################################

# function to import folder of monthly files - with daily data - as raster and extract a data frame with values per location
# requires a vector and netcdf filename as input, plus the climate variable name (eg rainfall)

netcdf_to_df <- function(netcdf_filename, vector_filename, var_name) {
  
  dat <- terra::rast(netcdf_filename)
  time <- terra::time(dat)
  raster_crs <- terra::crs(dat, proj = TRUE)
  sites_new <- terra::project(vector_filename, raster_crs)
  small_raster <- terra::crop(dat, sites_new, snap = "out")
  df <- terra::extract(small_raster, sites_new, snap = "out", xy = TRUE)
  sites <- sites_new$field
  x_coords <- df$x
  y_coords <- df$y
  df$ID <- NULL
  df$x <- NULL
  df$y <- NULL
  names(df) <- time
  df$sites <- sites
  df$x <- x_coords
  df$y <- y_coords
  df <- df %>% pivot_longer(cols = c(-x, -y), names_to = "date", values_to = var_name)
  df$date <- as_date(df$date)
  return(df)
  
}

################################################################################

# RAINFALL #

################################################################################

# RAINFALL AINSALE

# function applied to folder of files with same weather variable

# read in list of weather nc files downloaded from CEDA
netcdf_filename <- list.files("~/Documents/Hadley_climate_data/Rainfall", full.names = TRUE)
# read in a vector of the area of the country you are interested in
# here it is the raindrop and dragnet site
vector_filename <- vect("data/polygons/ainsdale/ainsdale.shp")
# create a var name for the climate data type here
var_name = "rainfall"
# apply the netdcf to df function across the list fo file names
rainfall <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
# convert list to df
rainfall <- rbindlist(rainfall)
# add site name
rainfall$site <- "Ainsdale"
# add year and month as separate variables for plotting
rainfall <- rainfall %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

unique(rainfall$x) 
# there are several 1km grid cells that overlap at the site 
# we need an average for the site

# quick data visualisation
# year as a facet 
rainfall %>% 
  ggplot(aes(date, rainfall))+
  geom_point()+
  geom_line()+
  facet_wrap(~year, scales = "free")

# get averages across the sites
ainsdale_rain <-
  rainfall %>% 
  drop_na() %>% 
  mutate(cell = paste(x, y)) %>%
  group_by(year, month) %>% 
  mutate(rain_monthly_average_cells = mean(rainfall)) %>% 
  distinct(rain_monthly_average_cells, .keep_all = TRUE) %>%
  group_by(year) %>% 
  mutate(rain_annual_total = sum(rain_monthly_average_cells)) %>%
  select(-rainfall)
  
# quick look at the data

ggplot(ainsdale_rain, aes(month, rain_monthly_average_cells)) + 
  geom_point()+
  geom_line()+
  facet_wrap(~ year)

ggplot(ainsdale_rain, aes(year, rain_annual_total)) + 
  geom_point()+
  geom_line()

# to get average over the 25 year recording period

ainsdale_rain %>%
  ungroup() %>%
  summarise(site_average = mean(rain_annual_total)) # 883mm

################################################################################

# RAINFALL HAZELRIGG

# function applied to folder of files with same weather variable

# read in list of weather nc files downloaded from CEDA
netcdf_filename <- list.files("~/Documents/Hadley_climate_data/Rainfall", full.names = TRUE)
# read in a vector of the area of the country you are interested in
# here it is the raindrop and dragnet site
vector_filename <- vect("data/polygons/Hazelrigg/Hazelrigg.shp")
# create a var name for the climate data type here
var_name = "rainfall"
# apply the netdcf to df function across the list fo file names
rainfall <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
# convert list to df
rainfall <- rbindlist(rainfall)
# add site name
rainfall$site <- "Hazelrigg"
# add year and month as separate variables for plotting
rainfall <- rainfall %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# quick data visualisation
# year as a facet 
rainfall %>% 
  ggplot(aes(date, rainfall))+
  geom_point()+
  geom_line()+
  facet_wrap(~year, scales = "free")

# get averages across the sites
hazelrigg_rain <-
  rainfall %>% 
  drop_na() %>% 
  mutate(cell = paste(x, y)) %>%
  group_by(year, month) %>% 
  mutate(rain_monthly_average_cells = mean(rainfall)) %>% 
  distinct(rain_monthly_average_cells, .keep_all = TRUE) %>%
  group_by(year) %>% 
  mutate(rain_annual_total = sum(rain_monthly_average_cells)) %>%
  select(-rainfall)

# quick look at the data

ggplot(hazelrigg_rain, aes(month, rain_monthly_average_cells)) + 
  geom_point()+
  geom_line()+
  facet_wrap(~ year)

# to get average over the 25 year recording period

hazelrigg_rain %>%
  ungroup() %>%
  summarise(site_average = mean(rain_annual_total)) # 1199mm

# plot annual means with mean over 20 year period
ggplot(hazelrigg_rain, aes(year, rain_annual_total)) + 
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 1199, colour = "red")


################################################################################

# RAINFALL WYTHAM

# function applied to folder of files with same weather variable

# read in list of weather nc files downloaded from CEDA
netcdf_filename <- list.files("~/Documents/Hadley_climate_data/Rainfall", full.names = TRUE)
# read in a vector of the area of the country you are interested in
# here it is the raindrop and dragnet site
vector_filename <- vect("data/polygons/wytham/wytham.shp")
# create a var name for the climate data type here
var_name = "rainfall"
# apply the netdcf to df function across the list fo file names
rainfall <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
# convert list to df
rainfall <- rbindlist(rainfall)
# add site name
rainfall$site <- "Wytham"
# add year and month as separate variables for plotting
rainfall <- rainfall %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

# get averages across the sites
wytham_rain <-
  rainfall %>% 
  drop_na() %>% 
  mutate(cell = paste(x, y)) %>%
  group_by(year, month) %>% 
  mutate(rain_monthly_average_cells = mean(rainfall)) %>% 
  distinct(rain_monthly_average_cells, .keep_all = TRUE) %>%
  group_by(year) %>% 
  mutate(rain_annual_total = sum(rain_monthly_average_cells)) %>%
  select(-rainfall)

# quick look at the data

ggplot(wytham_rain, aes(month, rain_monthly_average_cells)) + 
  geom_point()+
  geom_line()+
  facet_wrap(~ year)

# to get average over the 25 year recording period

wytham_rain %>%
  ungroup() %>%
  summarise(site_average = mean(rain_annual_total)) # 770 mm

# plot annual means with mean over 20 year period
ggplot(wytham_rain, aes(year, rain_annual_total)) + 
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 770, colour = "red")

################################################################################

rain <- rbind(ainsdale_rain, wytham_rain, hazelrigg_rain)

rain$site <- factor(rain$site, levels = c("Wytham", "Ainsdale", "Hazelrigg"))

ggplot(rain, aes(year, rain_annual_total))+
  theme_bw()+
  geom_point()+
  geom_line()+
  geom_smooth(method="lm",formula=y~1,se=FALSE)+
  facet_wrap(~site)+
  ggtitle("Rainfall Gradient UK DRAGnet sites")
  

################################################################################

# MAX TEMP #

################################################################################

# MAX TEMP AINSALE

# function applied to folder of files with same weather variable

# read in list of weather nc files downloaded from CEDA
netcdf_filename <- list.files("~/Documents/Hadley_climate_data/Tasmax", full.names = TRUE)
# read in a vector of the area of the country you are interested in
# here it is the raindrop and dragnet site
vector_filename <- vect("data/polygons/ainsdale/ainsdale.shp")
# create a var name for the climate data type here
var_name = "tasmax"
# apply the netdcf to df function across the list fo file names
tasmax <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
# convert list to df
tasmax <- rbindlist(tasmax)
# add site name
tasmax$site <- "Ainsdale"
# add year and month as separate variables for plotting
tasmax <- tasmax %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

unique(tasmax$x) 
# there are several 1km grid cells that overlap at the site 
# we need an average for the site

# quick data visualisation
# year as a facet 
tasmax %>% 
  ggplot(aes(date, tasmax))+
  geom_point()+
  geom_line()+
  facet_wrap(~year, scales = "free")

# get averages across the sites
ainsdale_tasmax <-
  tasmax %>% 
  drop_na() %>% 
  mutate(cell = paste(x, y)) %>%
  group_by(year, month) %>% 
  mutate(tasmax_monthly_average_cells = mean(tasmax)) %>% 
  distinct(tasmax_monthly_average_cells, .keep_all = TRUE) %>%
  group_by(year) %>% 
  mutate(tasmax_annual_mean = mean(tasmax_monthly_average_cells)) %>%
  select(-tasmax)

# quick look at the data

ggplot(ainsdale_tasmax, aes(month, tasmax_monthly_average_cells)) + 
  geom_point()+
  geom_line()+
  facet_wrap(~ year)

# to get average annual max temp over the 25 year recording period
ainsdale_tasmax %>%
  ungroup() %>%
  summarise(site_average = mean(tasmax_annual_mean)) # 13.6

ggplot(ainsdale_tasmax, aes(year, tasmax_annual_mean)) + 
  geom_point()+
  geom_line()

################################################################################

# MAX TEMP Hazelrigg

# function applied to folder of files with same weather variable

# read in list of weather nc files downloaded from CEDA
netcdf_filename <- list.files("~/Documents/Hadley_climate_data/Tasmax", full.names = TRUE)
# read in a vector of the area of the country you are interested in
# here it is the raindrop and dragnet site
vector_filename <- vect("data/polygons/Hazelrigg/Hazelrigg.shp")
# create a var name for the climate data type here
var_name = "tasmax"
# apply the netdcf to df function across the list fo file names
tasmax <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
# convert list to df
tasmax <- rbindlist(tasmax)
# add site name
tasmax$site <- "Hazelrigg"
# add year and month as separate variables for plotting
tasmax <- tasmax %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

unique(tasmax$x) 
# there are several 1km grid cells that overlap at the site 
# we need an average for the site

# quick data visualisation
# year as a facet 
tasmax %>% 
  ggplot(aes(date, tasmax))+
  geom_point()+
  geom_line()+
  facet_wrap(~year, scales = "free")

# get averages across the sites
hazelrigg_tasmax <-
  tasmax %>% 
  drop_na() %>% 
  mutate(cell = paste(x, y)) %>%
  group_by(year, month) %>% 
  mutate(tasmax_monthly_average_cells = mean(tasmax)) %>% 
  distinct(tasmax_monthly_average_cells, .keep_all = TRUE) %>%
  group_by(year) %>% 
  mutate(tasmax_annual_mean = mean(tasmax_monthly_average_cells)) %>%
  select(-tasmax)

# quick look at the data
ggplot(hazelrigg_tasmax, aes(month, tasmax_monthly_average_cells)) + 
  geom_point()+
  geom_line()+
  facet_wrap(~ year)

# to get average annual max temp over the 25 year recording period
hazelrigg_tasmax %>%
  ungroup() %>%
  summarise(site_average = mean(tasmax_annual_mean)) # 13

ggplot(hazelrigg_tasmax, aes(year, tasmax_annual_mean)) + 
  geom_point()+
  geom_line()

################################################################################

# MAX TEMP Wytham

# function applied to folder of files with same weather variable

# read in list of weather nc files downloaded from CEDA
netcdf_filename <- list.files("~/Documents/Hadley_climate_data/Tasmax", full.names = TRUE)
# read in a vector of the area of the country you are interested in
# here it is the raindrop and dragnet site
vector_filename <- vect("data/polygons/wytham/wytham.shp")
# create a var name for the climate data type here
var_name = "tasmax"
# apply the netdcf to df function across the list fo file names
tasmax <- lapply(netcdf_filename, netcdf_to_df, vector_filename, var_name)
# convert list to df
tasmax <- rbindlist(tasmax)
# add site name
tasmax$site <- "Wytham"
# add year and month as separate variables for plotting
tasmax <- tasmax %>% 
  mutate(year = lubridate::year(date)) %>%
  mutate(month = lubridate::month(date))

unique(tasmax$x) 
# there are several 1km grid cells that overlap at the site 
# we need an average for the site

# quick data visualisation
# year as a facet 
tasmax %>% 
  ggplot(aes(date, tasmax))+
  geom_point()+
  geom_line()+
  facet_wrap(~year, scales = "free")

# get averages across the sites
wytham_tasmax <-
  tasmax %>% 
  drop_na() %>% 
  mutate(cell = paste(x, y)) %>%
  group_by(year, month) %>% 
  mutate(tasmax_monthly_average_cells = mean(tasmax)) %>% 
  distinct(tasmax_monthly_average_cells, .keep_all = TRUE) %>%
  group_by(year) %>% 
  mutate(tasmax_annual_mean = mean(tasmax_monthly_average_cells)) %>%
  select(-tasmax)

# quick look at the data
ggplot(wytham_tasmax, aes(month, tasmax_monthly_average_cells)) + 
  geom_point()+
  geom_line()+
  facet_wrap(~ year)

# to get average annual max temp over the 25 year recording period
wytham_tasmax %>%
  ungroup() %>%
  summarise(site_average = mean(tasmax_annual_mean)) # 14.5

ggplot(wytham_tasmax, aes(year, tasmax_annual_mean)) + 
  geom_point()+
  geom_line()

################################################################################

tasmax <- rbind(ainsdale_tasmax, wytham_tasmax, hazelrigg_tasmax)

tasmax$site <- factor(tasmax$site, levels = c("Wytham", "Ainsdale", "Hazelrigg"))

ggplot(tasmax, aes(year, tasmax_annual_mean))+
  theme_bw()+
  geom_point()+
  geom_line()+
  geom_smooth(method="lm",formula=y~1,se=FALSE)+
  facet_wrap(~site)+
  ggtitle("Mean Annual Temperature Gradient UK DRAGnet sites")





