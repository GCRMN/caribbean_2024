# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)

# 2. Load data ----

## 2.1 Coral reef distribution ----

data_reef <- st_read("data/01_maps/02_clean/02_reefs/reefs.shp")

## 2.2 Cyclones lines ----

load("data/07_cyclones/01_cyclones_lines.RData")

## 2.3 Cyclones points ----

load("data/07_cyclones/01_cyclones_points.RData")

## 2.4 EEZ ----

data_eez <- st_read("data/01_maps/02_clean/03_eez/caribbean_eez.shp")

# 2.5 Coral reef distribution 100 km buffer --

data_reef_buffer <- st_read("data/01_maps/02_clean/02_reefs/reefs_buffer_100.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid() %>% 
  group_by(territory) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  ungroup()

# 3. Visual check ----

ggplot() +
  geom_sf(data = data_reef) +
  geom_sf(data = data_reef_buffer, alpha = 0.1) +
  geom_sf(data = data_eez, alpha = 0.1) +
  geom_sf(data = data_ts_lines)

# 4. Extract tropical storms ----

## 4.1 Create a function to extract tropical storm for a given EEZ ----

map_event <- function(ts_id_i, data_reef_i){
  
  data_ts_lines_i <- data_ts_lines %>% 
    filter(ts_id == ts_id_i)
  
  data_ts_points_i <- data_ts_points %>% 
    filter(ts_id == ts_id_i)
  
  dist <- as.numeric(st_distance(data_reef_i, data_ts_lines_i))/1000
  
  nearest <- st_nearest_feature(data_reef_i, data_ts_points_i)

  wind_speed <- data_ts_points_i[nearest,]
  
  result <- wind_speed %>% 
    mutate(dist = dist) %>% 
    st_drop_geometry()
  
  return(result)
  
}

## 4.2 Create a function to extract tropical storms within 100 km from a reef ----

map_cyclone <- function(territory_i){
  
  data_reef_buffer_i <- data_reef_buffer %>% 
    filter(territory == territory_i)
  
  data_reef_i <- data_reef %>% 
    filter(territory == territory_i)
  
  # Extract tropical storms passing within 100 km from a reef ----
  
  data_ts_lines_i <- st_filter(data_ts_lines, data_reef_buffer_i, .predicate = st_intersects)
  
  results <- map_dfr(unique(data_ts_lines_i$ts_id), ~map_event(ts_id_i = ., data_reef_i)) %>% 
    mutate(territory = territory_i)
  
  return(results)
  
}

## 4.3 Map over the function ----

data_cyclones <- map_dfr(unique(data_eez$territory), ~map_cyclone(territory_i = .)) %>% 
  filter(dist < 100) # Remove rows with dist > 100, due to two particular cases (see notebook)

# 5. Export the results ----

save(data_cyclones, file = "data/07_cyclones/02_cyclones_extracted.RData")
