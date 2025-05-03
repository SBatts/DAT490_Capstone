## packages
install.packages("sf")
install.packages("ggplot2")
install.packages("tidygeocoder")

library(sf)
library(ggplot2)
library(tidyverse)
library(tidygeocoder)

## files
states = st_read("us_states.shp")
counties = st_read("tl_2023_us_county.shp")
mortality = read.csv("NCHS_Mortality.csv")
trtmnt = read.csv("treatment_geocoded.csv")

## clean up
mortality = mortality %>%
  rename(MBDR = Model.based.Death.Rate)
states = states %>%
  rename(STATEFP = STATE_FIPS)

## new 5 digit FIPS code for merging
counties$FIPS = paste(counties$STATEFP, counties$COUNTYFP, sep = "")
mortality$FIPS = str_pad(as.character(mortality$FIPS), width = 5, 
                         side = "left", pad = "0")

## project
counties = st_transform(counties, crs = 5070)
states = st_transform(states, crs = 5070)

## conus only
counties = counties[!substr(counties$FIPS, 1, 2) %in% c("02", "15", "60", "66", 
                                                        "69", "72", "78"), ]
mortality = mortality[!substr(mortality$FIPS, 1, 2) %in% c("02", "15", "60", "66", 
                                                           "69", "72", "78"), ]
states = states %>%
  filter(!STATE_ABBR %in% c("AK", "HI", "PR", "GU", "VI", "MP", "AS"))
trtmnt = trtmnt %>%
  filter(!State %in% c("AK", "HI", "PR", "GU", "VI", "MP", "AS"))

## find missing
to_recode = trtmnt %>%
  filter(is.na(latitude) | is.na(longitude))

## geocode missing
to_recode = to_recode %>%
  geocode(address = full_address,
          method = "osm",
          lat = latitude,
          long = longitude,
          verbose = TRUE)

## clean it up
to_recode = to_recode %>%
  select(-latitude...16, -longitude...17) %>% 
  rename(latitude = latitude...18,
         longitude = longitude...19)

## do it again
still_missing = to_recode %>%
  filter(is.na(latitude) | is.na(longitude))

## don't let me down arcgis
still_missing = still_missing %>%
  geocode(address = full_address,
          method = "arcgis",
          lat = latitude,
          long = longitude,
          verbose = TRUE)

## clean it up again
still_missing = still_missing %>%
  select(-latitude...16, -longitude...17) %>% 
  rename(latitude = latitude...18,
         longitude = longitude...19)

## keep success from osm & add arcgis fixes
osm_success = to_recode %>%
  filter(!is.na(latitude) & !is.na(longitude))
to_recode_final = bind_rows(osm_success, still_missing)

## rebuild treatment centers
trtmnt_fixed = trtmnt %>%
  filter(!is.na(latitude) & !is.na(longitude))
trtmnt_final = bind_rows(trtmnt_fixed, to_recode_final)

## save it just in case
write.csv(trtmnt_final, "treatment_final.csv", row.names = FALSE)

## trtmnt as sf, project to match
trtmnt_sf = st_as_sf(trtmnt_final, coords = c("longitude","latitude"), crs = 5070)

## conus
trtmnt_sf = trtmnt_sf %>%
  filter(!State %in% c("AK", "HI", "PR", "GU", "VI", "MP", "AS"))

## avg_mor
mor_avg =  mortality %>% filter(Year >= 2016)
mor_avg = mor_avg %>%
  group_by(FIPS) %>%
  summarise(avg_mor = mean(MBDR, na.rm = TRUE)) %>%
  mutate(avg_mor = ifelse(is.na(avg_mor), median(avg_mor, na.rm = TRUE), avg_mor))

## join mortality to county shapefile
counties_mor = counties %>%
  left_join(mor_avg, by = "FIPS")

##
ggplot(data = counties_mor) +
  geom_sf(aes(fill = avg_mor), color = "black", size = 0.1) + 
  geom_sf(data = states, fill = NA, color = "gray80", size = 0.1) + 
  geom_sf(data = trtmnt_sf, shape = 21, fill = "white", color = "black",
          size = 2, stroke = 0.6, alpha = 0.8) + 
  scale_fill_viridis_c(option = "viridis", direction = 1) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 10), 
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 8),
        legend.position = "bottom") +
  labs(title = "Annual Average Drug-Related Deaths (2016–2021)",
       subtitle = "Treatment center locations shown in white",
       fill = "Per 100,000",
       x = "Longitude",
       y = "Latitude")