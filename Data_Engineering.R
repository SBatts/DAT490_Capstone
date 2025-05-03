install.packages("sf")
install.packages("spdep")

library(sf)
library(tidyverse)
library(tidyr)
library(stringr)
library(dplyr)
library(spdep)

## working directory
setwd("C:/Users/sydne/OneDrive/Desktop/School/DAT 490/Data/")

## data
svi = read.csv("SVI_2022.csv")
mortality = read.csv("NCHS_Mortality.csv")
treatment = read.csv("treatment_geocoded.csv")

## shapefiles
states = st_read("us_states.shp")
counties = st_read("tl_2023_us_county.shp")

## tidy up
mortality = mortality %>%
  rename(MBDR = Model.based.Death.Rate)
states = states %>%
  rename(STATEFP = STATE_FIPS)

## new 5 digit FIPS code for merging
counties$FIPS = paste(counties$STATEFP, counties$COUNTYFP, sep = "")

## make FIPS 5 digits in frames where leading zero is missing
svi$FIPS = str_pad(as.character(svi$FIPS), width = 5, side = "left", pad = "0")
mortality$FIPS = str_pad(as.character(mortality$FIPS), width = 5, 
                         side = "left", pad = "0")

## get 5 year average mortality
mor_avg =  mortality %>% filter(Year >= 2016)
mor_avg = mor_avg %>%
  group_by(FIPS) %>%
  summarise(avg_mor = mean(MBDR, na.rm = TRUE)) %>%
  mutate(avg_mor = ifelse(is.na(avg_mor), median(avg_mor, na.rm = TRUE), avg_mor))

## merge shp and svi data with mor_avg, remove MOE
merged_df = merge(counties, svi, by = "FIPS")
merged_df = merged_df %>%
  left_join(mor_avg, by = "FIPS") %>%
  mutate(avg_mor = ifelse(is.na(avg_mor), median(avg_mor, na.rm = TRUE), avg_mor)) %>%
  select(-starts_with("M"))

## merge subregion into df
state_subregion = states %>%
  st_drop_geometry() %>%
  select(STATEFP, SUB_REGION) %>%
  distinct()
merged_df = merged_df %>%
  left_join(state_subregion, by = "STATEFP")

## convert to spatial points
treatment_sf = treatment %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

## project everything
merged_df = st_transform(merged_df, crs = 5070)
counties = st_transform(counties, crs = 5070)
states = st_transform(states, crs = 5070)
treatment_sf = st_transform(treatment_sf, crs = 5070)

## join county shapefile & count
facilities = st_join(treatment_sf, counties["FIPS"], join = st_within)
facility_counts = facilities %>%
  st_drop_geometry() %>%
  count(FIPS, name = "facility_count")

## merge count
merged_df = merged_df %>%
  left_join(facility_counts, by = "FIPS") %>%
  mutate(facility_count = replace_na(facility_count, 0))

## calculate & save facility density
merged_df = merged_df %>%
  mutate(facilities_per_10k = ifelse(E_TOTPOP > 0, 
                                     (facility_count / E_TOTPOP) * 10000, 0))

## find neighbors & calculate weight
merged_sf = merged_df %>% st_as_sf()
neighbors = poly2nb(merged_sf, row.names = merged_sf$FIPS, snap = 1e-6)
weights = nb2listw(neighbors, style = "W", zero.policy = TRUE)

## calculate mortality lag & fill in missing values (Hawaii, Alaska)
merged_df$lag_avg_mor = lag.listw(weights, merged_df$avg_mor)
merged_df$lag_density = lag.listw(weights, merged_df$facilities_per_10k)
merged_df <- merged_df %>%
  mutate(lag_avg_mor = ifelse(is.na(lag_avg_mor), avg_mor, lag_avg_mor),
         lag_density = ifelse(is.na(lag_density), 
                              facilities_per_10k, lag_density))

## count individual service codes & join df
service_diversity_by_fips = facilities %>%
  filter(!is.na(Service_code_info), !is.na(FIPS)) %>%
  mutate(service_list = str_split(Service_code_info, "\\s+")) %>%
  unnest(service_list) %>%
  mutate(service_list = str_trim(service_list)) %>%
  filter(service_list != "") %>%
  group_by(FIPS) %>%
  summarise(service_diversity = n_distinct(service_list))

## merge
merged_df = merged_df %>%
  st_drop_geometry() %>%
  left_join(service_diversity_by_fips, by = "FIPS") %>%
  mutate(service_diversity = replace_na(service_diversity, 0))

## calcuate population density
merged_df$pop_density = merged_df$E_TOTPOP / merged_df$AREA_SQMI

## select variables for model
short_df = subset(merged_df, 
                  select = c("FIPS", "COUNTY", "AREA_SQMI", "E_TOTPOP",
                             "EP_POV150", "EP_UNEMP", "EP_HBURD", "EP_NOHSDP",
                             "EP_UNINSUR", "EP_DISABL", "EP_SNGPNT" ,
                             "EP_LIMENG", "EP_MINRTY", "EP_NOVEH", "EP_MUNIT", 
                             "EP_CROWD","RPL_THEME1", "RPL_THEME2", 
                             "RPL_THEME3", "RPL_THEME4", "RPL_THEMES", 
                             "avg_mor", "SUB_REGION","facility_count", 
                             "facilities_per_10k", "lag_avg_mor", "lag_density", 
                             "service_diversity", "pop_density"))

merged_df$geometry = NULL

## export both
write.csv(short_df, "short_data.csv", row.names = FALSE, 
          quote = TRUE, na = "")

write.csv(merged_df, "merged_data.csv", row.names = FALSE, 
          quote = TRUE, na = "")