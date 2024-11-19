# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(lwgeom)
library(RcppRoll)

# 2. Load data ----

# 2.1 Site coordinates --

data_sites <- st_read("data/04_site-coords/site-coords_all.shp") %>% 
  st_transform(crs = 4326)

# 2.2 Cyclones lines --

load("data/05_cyclones/01_cyclones_lines.RData")

data_ts_lines <- data_ts_lines %>% 
  st_transform(crs = 4326)

# 2.3 Cyclones points --

load("data/05_cyclones/01_cyclones_points.RData")

data_ts_points <- data_ts_points %>% 
  st_transform(crs = 4326)

# 2.4 Coral reef distribution 100 km buffer --

data_reef_buffer <- st_read("data/03_reefs-area_wri/clean_buffer/reef_buffer.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline() %>% 
  st_make_valid()

# 3. Extract cyclones passing within 100 km from coral reefs ----

data_ts_lines_reef <- st_intersection(data_reef_buffer, data_ts_lines)

data_ts_points <- data_ts_points %>% 
  filter(ts_id %in% unique(data_ts_lines_reef$ts_id)) %>% 
  filter(wind_speed >= 119)

# 4. Create buffer of 111 km around sites with coral reefs ----

data_sites_buffer <- data_sites %>%  
  # Create the buffer around the site (1 degree ~ 111 km)
  st_buffer(dist = 1) %>% 
  st_wrap_dateline() %>% 
  st_make_valid()

# 5. Extract cyclones passing within 111 km from each site ----

pred_cyclones <- st_intersection(data_sites_buffer, data_ts_points) %>% 
  mutate(year = year(time)) %>% 
  st_drop_geometry() %>% 
  select(site_id, type, year, ts_id, name, wind_speed) %>% 
  distinct() %>% 
  group_by(site_id, type, year, ts_id, name) %>% 
  filter(wind_speed == max(wind_speed)) %>% 
  ungroup()

# 6. Transform data to create predictors ----

pred_cyclones <- pred_cyclones %>% 
  group_by(site_id, type, year) %>% 
  mutate(wind_speed = max(wind_speed),
         nb_cyclones = n()) %>% 
  ungroup() %>% 
  select(-ts_id, -name) %>%
  distinct() %>% 
  tidyr::complete(year = seq(1980, 2023), nesting(site_id, type), 
                  fill = list(ts_id = NA, name = NA, wind_speed = NA)) %>% 
  # Wind speed of cyclones over year n-5 (five past years)
  arrange(site_id, type, year) %>% 
  mutate(wind_speed_y5 = roll_max(wind_speed, n = 5, align = "right", fill = NA, na.rm = TRUE),
         wind_speed_y5 = if_else(wind_speed_y5 == -Inf, 0, wind_speed_y5)) %>% 
  # Number of cyclones over year n-5 (five past years)
  mutate(nb_cyclones_y5 = roll_sum(nb_cyclones, n = 5, align = "right", fill = NA, na.rm = TRUE)) %>% 
  group_by(site_id, type) %>% 
  mutate(nb_cyclones = sum(nb_cyclones, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(-wind_speed)

# 7. Export the results ----

write.csv(pred_cyclones, file = "data/10_predictors/pred_cyclones.csv", row.names = FALSE)
