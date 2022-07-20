# Data retrieval and cleaning

# Load required packages
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(RSocrata)) install.packages('RSocrata')
if (!require(stringi)) install.packages('Stringi')

library(tidyverse)
library(RSocrata)

API_2016 <- 'https://data.cityofnewyork.us/resource/utpj-74fz.json'
API_2017 <- 'https://data.cityofnewyork.us/resource/4t62-jm4m.json'
API_2018 <- 'https://data.cityofnewyork.us/resource/4tys-3tzj.json'
API_2019 <- 'https://data.cityofnewyork.us/resource/wcm8-aq5w.json'
  
# Retrieve all data for 2016 through 2019 and select the desired colummns
# Takes about 1-2 minutes

# Data for 2016 Calendar Year
NYC_LL84_2016 <- RSocrata::read.socrata(API_2016) %>%
  select("property_id" = property_id, 
         "property_name" = property_name, 
         "bin_number" = nyc_building_identification,
         "year" = year_ending,
         "borough" = borough, 
         "city" = city, 
         "postcode" = postcode, 
         "primary_property_type" = primary_property_type, 
         "year_built" = year_built, 
         "number_of_buildings" = number_of_buildings,
         "energy_star_score" = energy_star_score, 
         "site_eui" = site_eui_kbtu_ft, 
         "site_energy_use" = site_energy_use_kbtu,
         "natural_gas_use" = natural_gas_use_kbtu,
         "electricity_use" = electricity_use_grid_purchase,
         "total_ghg_emissions" = total_ghg_emissions_metric, 
         "property_gfa" = property_gfa_calculated, 
         "parking_gfa" = parking_gross_floor_area,
         "leed_project" = leed_us_project_id,
         "default_values" = default_values, 
         "temporary_values" = temporary_values) %>%
  distinct(property_id, .keep_all = TRUE)

# Data for 2017 Calendar Year
NYC_LL84_2017 <- RSocrata::read.socrata(API_2017) %>%
  select("property_id" = property_id, 
         "property_name" = property_name, 
         "bin_number" = nyc_building_identification,
         "year" = year_ending,
         "borough" = borough, 
         "city" = city, 
         "postcode" = postcode, 
         "primary_property_type" = primary_property_type, 
         "year_built" = year_built, 
         "number_of_buildings" = number_of_buildings,
         "energy_star_score" = energy_star_score, 
         "site_eui" = site_eui_kbtu_ft, 
         "site_energy_use" = site_energy_use_kbtu,
         "natural_gas_use" = natural_gas_use_kbtu,
         "electricity_use" = electricity_use_grid_purchase,
         "total_ghg_emissions" = total_ghg_emissions_metric, 
         "property_gfa" = property_gfa_calculated, 
         "parking_gfa" = parking_gross_floor_area,
         "leed_project" = leed_us_project_id,
         "default_values" = default_values, 
         "temporary_values" = temporary_values) %>%
  distinct(property_id, .keep_all = TRUE)

# Data for 2018 Calendar Year
NYC_LL84_2018 <- RSocrata::read.socrata(API_2018) %>%
  select("property_id" = property_id, 
         "property_name" = property_name, 
         "bin_number" = nyc_building_identification,
         "year" = year_ending,
         "borough" = borough, 
         "city" = city, 
         "postcode" = postcode, 
         "primary_property_type" = primary_property_type, 
         "year_built" = year_built, 
         "number_of_buildings" = number_of_buildings,
         "energy_star_score" = energy_star_score, 
         "site_eui" = site_eui_kbtu_ft, 
         "site_energy_use" = site_energy_use_kbtu,
         "natural_gas_use" = natural_gas_use_kbtu,
         "electricity_use" = electricity_use_grid_purchase,
         "total_ghg_emissions" = total_ghg_emissions_metric, 
         "property_gfa" = property_gfa_calculated, 
         "parking_gfa" = parking_gross_floor_area,
         "leed_project" = leed_us_project_id,
         "default_values" = default_values, 
         "temporary_values" = temporary_values) %>%
  distinct(property_id, .keep_all = TRUE)

# Data for 2019 Calendar Year
NYC_LL84_2019 <- RSocrata::read.socrata(API_2019) %>%
  select("property_id" = property_id, 
         "property_name" = property_name, 
         "bin_number" = nyc_building_identification,
         "year" = year_ending,
         "borough" = borough, 
         "city" = city, 
         "postcode" = postcode, 
         "primary_property_type" = primary_property_type, 
         "year_built" = year_built, 
         "number_of_buildings" = number_of_buildings,
         "energy_star_score" = energy_star_score, 
         "site_eui" = site_eui_kbtu_ft, 
         "site_energy_use" = site_energy_use_kbtu,
         "natural_gas_use" = natural_gas_use_kbtu,
         "electricity_use" = electricity_use_grid_purchase,
         "total_ghg_emissions" = total_ghg_emissions_metric, 
         "property_gfa" = property_gfa_calculated, 
         "parking_gfa" = parking_gross_floor_area,
         "leed_project" = leed_us_project_id,
         "default_values" = default_values, 
         "temporary_values" = temporary_values) %>%
  distinct(property_id, .keep_all = TRUE)

# Merge all Local Law 84 reporting years
NYC_LL84 <- rbind.data.frame(NYC_LL84_2016,
                             NYC_LL84_2017,
                             NYC_LL84_2018,
                             NYC_LL84_2019)


# Clean data
# Recode any primary property types with < 10 occurances as "Other"
# Remove rows with NAs and change not available to 0 for parking gfa
NYC_LL84 <- NYC_LL84 %>%
  mutate(year = substr(year, start = 1, stop = 4),
         city = stringi::stri_trans_totitle(city),
         borough = stringi::stri_trans_totitle(borough),
         leed_project = ifelse(leed_project == "Not Available", 0, 1),
         parking_gfa = ifelse(parking_gfa == "Not Available", 0, parking_gfa)) %>%
  filter(site_eui != "Not Available",
         property_gfa != "Not Available",
         temporary_values != "Yes",
         default_values != "Yes",
         energy_star_score != "Not Available",
         total_ghg_emissions != "Not Available") %>%
  left_join(NYC_LL84 %>%
              group_by(primary_property_type) %>%
              summarise(Count = n()), 
            by = "primary_property_type") %>%
  mutate(primary_property_type = case_when(
    Count > 10 ~ primary_property_type,
    Count <= 10 ~ "Other"),
    mutate(primary_property_type = gsub(pattern = "Immeuble à logements multiples",
                                        replacement = "Multifamily Housing",
                                        x = primary_property_type))) %>%
  select( -temporary_values, -default_values, -Count) %>%
  replace_na(list(parking_gfa = 0,
                  leed_project = 0)) %>%
  drop_na(natural_gas_use, electricity_use) 

glimpse(NYC_LL84)


# Read in annual weather data 
# Source NOAA, National Weather Service
# https://www.weather.gov/wrh/Climate?wfo=okx
Weather_Data <- read.csv("Data/NYCWeatherData.csv")

# Join with cleaned up NYC LL84 data
NYC_LL84 <- left_join(NYC_LL84, 
          Weather_Data %>%
            mutate(year = as.character(year)) %>%
            group_by(year) %>%
            summarise(CDD = mean(CDD),
                      HDD = mean(HDD),
                      MeanMaxTemp = mean(MeanMaxTemp),
                      MeanMinTemp = mean(MeanMinTemp)),
          by = "year")


# Filter out errors and outliers
summary(NYC_LL84$site_eui)

ggplot(NYC_LL84, aes(y = site_eui)) +
  geom_boxplot() 

# There are clear errors for the same building over the years
flag <- NYC_LL84 %>% slice_max(order_by = site_eui, n = 20) %>% .$property_id

NYC_LL84 %>%
  filter(property_id %in% flag) %>%
  select(property_id, year, site_eui) %>%
  pivot_wider(names_from = year,
              values_from = site_eui) %>%
  knitr::kable()

# Filter out the errors and outliers 
# I use a cutoff of 2 x the median Site EUI for fast food restaurants (402.7 kBtu/sqft), 
# the property type with the highest median EUI
NYC_LL84 <- NYC_LL84 %>%
  filter(site_eui < 2*402.7)

# Write to folder and read from folder for future uses 
write.csv(NYC_LL84, "Data/NYC_LL84_Clean.csv", row.names = FALSE) 








