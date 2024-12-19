# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(sf)
sf_use_s2(FALSE)

# 2. Load gcrmndb_benthos data ----

load("C:/Users/jwicquart/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/09_gcrmndb_benthos.RData")

# 3. Filter required data ----

# -------------------------------------------------------- #
# /!\     MAKE SURE TO REMOVE DATASET ID WHOSE USE     /!\ #
# /!\        IS NOT AUTHORISED FOR THIS REPORT         /!\ #
# -------------------------------------------------------- #

data_benthic <- synthetic_data %>% 
  # Filter GCRMN region
  filter(region == "Caribbean") %>% 
  # Remove useless datasets
  #filter(!(datasetID %in% c())) %>% 
  # Filter data on the period of interest
  filter(year >= 1980 & year <= 2024) %>% 
  # Filter depth of shallow coral reefs
  filter(is.na(verbatimDepth) | verbatimDepth <= 30)

# 4. Add area variable ----

data_area <- st_read("data/01_maps/02_clean/03_eez/caribbean_area.shp")

## 4.1 Visual check the size of the buffer -----

ggplot() +
  geom_sf(data = data_area %>% filter(area == "Costa Rica") %>% st_buffer(0.05), col = "red") +
  geom_sf(data = data_area %>% filter(area == "Costa Rica"))

ggplot() +
  geom_sf(data = data_area %>% filter(area == "Costa Rica") %>% st_buffer(0.05), col = "red") +
  geom_sf(data = data_area %>% filter(area == "Costa Rica")) +
  coord_sf(xlim = c(-83.2, -82.8), ylim = c(9.8, 10.2))

## 4.2 First assignation ----

data_benthic_coords <- data_benthic %>% 
  select(decimalLatitude, decimalLongitude) %>% 
  distinct() %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>% 
  st_join(., data_area)

## 4.3 Second assignation ----

data_benthic <- data_benthic_coords %>% 
  filter(is.na(area)) %>% 
  select(-area) %>% 
  st_join(., st_buffer(data_area, 0.05)) %>% 
  bind_rows(data_benthic_coords %>% filter(!(is.na(area))), .) %>% 
  mutate(decimalLatitude = st_coordinates(.)[,2],
         decimalLongitude = st_coordinates(.)[,1]) %>% 
  st_drop_geometry() %>% 
  group_by(decimalLongitude, decimalLatitude) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  mutate(area = if_else(n == 1, area, NA_character_)) %>% 
  distinct() %>% 
  select(-n) %>% 
  left_join(data_benthic, .) %>% 
  relocate("area", .after = "territory") %>% 
  drop_na(area)

# 5. Save the data ----

save(data_benthic, file = "data/02_misc/data-benthic.RData")

# 6. Export site coordinates (for predictors extraction) ----

data_benthic %>% 
  select(decimalLatitude, decimalLongitude) %>% 
  distinct() %>% 
  mutate(type = "obs",
         site_id = as.character(row_number(.)-1)) %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>% 
  st_write(., dsn = "data/03_site-coords/site-coords_obs.shp", delete_dsn = TRUE)
