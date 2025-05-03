install.packages("ggplot2")
install.packages("haven")
install.packages("readr")
install.packages("sf")
install.packages("cowplot")
install.packages("tidygeocoder")
install.packages("ggcorrplot")

library(ggplot2)
library(haven)
library(readr)
library(sf)
library(tidyverse)
library(stringr)
library(dplyr)
library(viridisLite)
library(cowplot)
library(tidygeocoder)
library(broom)
library(ggcorrplot)

## working directory
setwd("C:/Users/sydne/OneDrive/Desktop/School/DAT 490/Data/")

## dataframes
svi = read.csv("SVI_2022.csv")
mortality = read.csv("NCHS_Mortality.csv")
treatment = read.csv("Treatment.csv")

##shapefiles
states = st_read("us_states.shp")
counties = st_read("tl_2023_us_county.shp")

## clean up
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

## remove everything except continental us
counties = counties[!substr(counties$FIPS, 1, 2) %in% c("02", "15", "60", "66", 
                                                        "69", "72", "78"), ]
svi = svi[!substr(svi$FIPS, 1, 2) %in% c("02", "15", "60", "66", 
                                                        "69", "72", "78"), ]
mortality = mortality[!substr(mortality$FIPS, 1, 2) %in% c("02", "15", "60", "66", 
                                                        "69", "72", "78"), ]
states = states %>%
  filter(!STATE_ABBR %in% c("AK", "HI", "PR", "GU", "VI", "MP", "AS"))
treatment = treatment %>%
  filter(!state %in% c("AK", "HI", "PR", "GU", "VI", "MP", "AS"))

## get 5 year average mortality
mor_avg =  mortality %>% filter(Year >= 2016)
mor_avg = mor_avg %>%
  group_by(FIPS) %>%
  summarise(avg_mor = mean(MBDR, na.rm = TRUE)) %>%
  mutate(avg_mor = ifelse(is.na(avg_mor), median(avg_mor, na.rm = TRUE), avg_mor))

## merge shp and svi data with mor_avg, remove MOE
merged_df = merge(counties, svi, by = "FIPS")
merged_df = merged_df %>%
  left_join(mor_avg, by = "FIPS")
merged_df = merged_df %>%
  select(-starts_with("M"))

## check projection
st_crs(merged_df)
st_crs(states)

## match the projection
merged_df = st_transform(merged_df, crs = 5070)
states = st_transform(states, crs = 5070)

## merge subregion into df
state_subregion = states %>%
  st_drop_geometry() %>%
  select(STATEFP, SUB_REGION) %>%
  distinct()
merged_df = merged_df %>%
  left_join(state_subregion, by = "STATEFP")

## regression model between mortality and SVI/Regions
model = lm(avg_mor ~ RPL_THEMES + SUB_REGION, data = merged_df)
tidy_model = tidy(model, conf.int = TRUE)
tidy_model = tidy_model %>%
  mutate(term = case_when(
    term == "RPL_THEMES" ~ "SVI Score",
    grepl("SUB_REGION", term) ~ gsub("SUB_REGION", "", term),
    term == "(Intercept)" ~ "Intercept",
    TRUE ~ term))
plot_data = tidy_model %>% filter(term != "Intercept")

## regression coefficients
ggplot(plot_data, aes(x = reorder(term, estimate), y = estimate, fill = estimate > 0)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "gray30", linetype = "dashed") +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "firebrick", "FALSE" = "steelblue"),
                    labels = c("Negative Effect", "Positive Effect"),
                    name = "Direction") +
  labs(title = "Effect of Predictors on Drug Mortality Rates",
       x = "Predictor",
       y = "Estimated Coefficient") +
  theme_minimal(base_size = 12)

## plot overall SVI score
map1 = ggplot(data = merged_df) +
  geom_sf(aes(fill = RPL_THEMES), color = "black", size = 0.1) +
  geom_sf(data = states, fill = NA, color = "white", size = 0.3) +
  geom_sf_text(data = states, aes(label = STATE_ABBR), color = 'white',
               size = 2, fontface = 'bold', inherit.aes = FALSE) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  theme_minimal() +
  theme(plot.title = element_text(size = 18)) +
  labs(title = "Social Vulnerability Index in US Counties",
       fill = "SVI Score",
       x = "Longitude",
       y = "Latitude")
map1

## plot mortality rates
map2 = ggplot(data = merged_df) +
  geom_sf(aes(fill = avg_mor), color = "black", size = 0.1) +
  geom_sf(data = states, fill = NA, color = "white", size = 0.3) +
  geom_sf_text(data = states, aes(label = STATE_ABBR), color = 'white',
               size = 2, fontface = 'bold', inherit.aes = FALSE) +
  scale_fill_viridis_c(option = "rocket", direction = 1) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18)) +
  labs(title = "Annual Average Drug-Related Deaths (2016-2021)",
       fill = "Per 100,000",
       x = "Longitude",
       y = "Latitude")
map2

## create terciles (low, medium, high)
merged_df = merged_df %>%
  mutate(svi_tertile = ntile(RPL_THEMES, 3),
         mor_tertile = ntile(avg_mor, 3),
         bivar_class = paste0(svi_tertile, "-", mor_tertile))

## choosing colors to represent correlation
bi_colors = c(
  "1-1" = "#e6f5e6",
  "2-1" = "#a6dba0",
  "3-1" = "#1b7837",
  
  "1-2" = "#f4cae4",
  "2-2" = "#b358a5",
  "3-2" = "#7b3294",
  
  "1-3" = "#fddbc7",
  "2-3" = "#e08214",
  "3-3" = "#b2182b"
  )

## create legend
legend_data = expand.grid(svi = 1:3, mortality = 1:3) %>%
  mutate(bivar_class = paste0(svi, "-", mortality), 
         fill_color = bi_colors[bivar_class])

## plot legend
bivar_legend = ggplot(legend_data, aes(x = svi, y = mortality)) +
  geom_tile(aes(fill = bivar_class), color = "white") +
  scale_fill_manual(values = bi_colors, guide = "none") +
  coord_fixed() +
  labs(x = "Higher SVI →", y = "↑ Higher Mortality") +
  theme_void() +
  theme(
    axis.title = element_text(size = 10, face = "bold"),
    plot.margin = margin(5, 5, 5, 5)
  )
bivar_legend

## Bivariate map
map3 = ggplot(merged_df) +
  geom_sf(aes(fill = bivar_class), color = "navy", size = 0.05) +
  scale_fill_manual(values = bi_colors, guide = "none") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18),
        legend.position = "right") +
  labs(title = "Bivariate Map of SVI and Drug-Related Mortality",
       fill = "SVI (→) / Mortality (↑)",
       x = "Longitude",
       y = "Latitude")
bi_map = plot_grid(map3, bivar_legend, ncol = 2, rel_widths = c(4, 1))
print(bi_map)

## remove treatment centers without addresses
treatment <- treatment %>%
  mutate(across(c(street1, city, state, zip),
                ~ ifelse(str_trim(.x) %in% c("", "- - -", "--", "---"), NA, .x)))
treatment <- treatment %>%
  filter(!is.na(street1), !is.na(city), !is.na(state), !is.na(zip),
         street1 != "", city != "", state != "", zip != "")

## filter out na
treatment = treatment %>%
  filter(
    !is.na(street1),
    !is.na(city),
    !is.na(state),
    !is.na(zip))

## full address for geocoding
treatment = treatment %>%
  mutate(full_address = paste(street1, city, state, zip, sep = ", "))

## smaller scale
sw_df = merged_df %>% filter(STATEFP %in% c("04", "06", "32", "49"))
sw_states = states %>% filter(STATE_ABBR %in% c("AZ", "CA", "UT", "NV"))
sw_trtmnt = treatment %>% filter(state %in% c("AZ", "CA", "UT", "NV"))

svi_long = sw_df %>%
  select(GEOID, STATEFP, geometry,
         RPL_THEME1, RPL_THEME2, RPL_THEME3, RPL_THEME4) %>%
  pivot_longer(
    cols = starts_with("RPL_THEME"),
    names_to = "SVI_theme",
    values_to = "SVI_score")

## maps for SVI themes
map4 = ggplot(svi_long) +
  geom_sf(aes(fill = SVI_score), color = "slateblue", size = 0.1) +
  geom_sf(data = sw_states, fill = NA, color = "white", size = 0.75) +
  geom_sf_text(data = sw_states, aes(label = STATE_ABBR), color = 'white',
               size = 2, fontface = 'bold', inherit.aes = FALSE) +
  facet_wrap(~ SVI_theme, ncol = 2, labeller = as_labeller(c(
    RPL_THEME1 = "1. Socioeconomic",
    RPL_THEME2 = "2. Household Composition",
    RPL_THEME3 = "3. Minority Status",
    RPL_THEME4 = "4. Housing/Transportation"
  ))) +
  scale_fill_viridis_c(option = "viridis", direction = -1, name = "SVI Score") +
  theme_minimal() +
  theme(strip.text = element_text(size = 6, face = "bold"),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position = "bottom") +
  labs(title = "SVI Theme Maps in the Southwest",
       x = "Longitude",
       y = "Latitude")
map4

## geocode
sw_trtmnt = sw_trtmnt %>%
  geocode(address = full_address, method = "arcgis", 
          lat = latitude, long = longitude)
sw_trtmnt = st_as_sf(sw_trtmnt, coords = c("longitude", "latitude"), crs = 4326)

## transform projection
sw_df = st_transform(sw_df, crs = 4326)
sw_states = st_transform(sw_states, crs = 4326)

## map mortality
map5 = ggplot(data = sw_df) +
  geom_sf(aes(fill = avg_mor), color = "black", size = 0.1) + 
  geom_sf(data = sw_states, fill = NA, color = "gray80", size = 0.4) + 
  geom_sf_text(data = sw_states, aes(label = STATE_ABBR), color = 'white',
               size = 3, fontface = 'bold', inherit.aes = FALSE) + 
  geom_sf(data = sw_trtmnt, shape = 24, fill = "navy", color = "skyblue",
          size = 1, stroke = 0.4, alpha = 0.6) + 
  scale_fill_viridis_c(option = "viridis", direction = 1) +
  coord_sf(xlim = c(-125, -105), ylim = c(30, 43), expand = FALSE) + 
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 10), 
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 8),
        legend.position = "bottom") +
  labs(title = "Annual Average Drug-Related Deaths (2016–2021)",
       subtitle = "Treatment center locations shown in green",
       fill = "Per 100,000",
       x = "Longitude",
       y = "Latitude")
map5

## check correlation with svi factors & mortality
corr_df <- merged_df %>%
  select(avg_mor,starts_with("EP_")) %>%
  select(where(is.numeric)) %>%
  st_drop_geometry() %>%
  drop_na()
cor_matrix = cor(corr_df, use = "complete.obs")
round(cor_matrix["avg_mor", ], 3)

## correlation vectors for plotting
corr_vector = cor_matrix["avg_mor", ]
top_corr = sort(abs(corr_vector), decreasing = TRUE)[2:11]  
tibble(variable = names(top_corr),
       correlation = corr_vector[names(top_corr)]) %>%
  ggplot(aes(x = reorder(variable, correlation), y = correlation)) +
  geom_col(fill = "plum4") +
  coord_flip() +
  labs(title = "Top Correlations Between SVI Variables and Drug-Related Mortality",
       x = "SVI Variable", y = "Pearson Correlation")

## heatmap
ggcorrplot(cor_matrix,
           type = "lower",
           colors = c("navy", "white", "firebrick"),
           outline.color = "white",
           show.diag = FALSE, lab = FALSE,
           title = "Correlation Matrix of SVI Variables and Drug Mortality",
           ggtheme = theme_minimal()) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5))