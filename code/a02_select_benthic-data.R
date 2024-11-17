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
  filter(year >= 1980 & year <= 2024)

# 4. Add area variable ----

data_area <- st_read("data/01_maps/02_clean/03_eez/caribbean_area.shp")

data_benthic <- data_benthic %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>% 
  st_join(., data_area) %>% 
  mutate(decimalLatitude = st_coordinates(.)[,2],
         decimalLongitude = st_coordinates(.)[,1]) %>% 
  st_drop_geometry()

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
