# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(sf)
sf_use_s2(FALSE)

# 2. Load gcrmndb_benthos data ----

load("C:/Users/jwicquart/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/09_gcrmndb_benthos.RData")

# 3. Filter required data ----

data_benthic <- synthetic_data %>% 
  # Filter GCRMN region
  filter(higherGeography == "Caribbean") %>% 
  # Remove useless datasets
  #filter(!(datasetID %in% c("0009", "0020", "0042"))) %>% 
  # Filter the depth of shallow coral reefs
  filter(verbatimDepth < 31) %>% 
  # Filter data on the period of interest
  filter(year >= 1980 & year <= 2023)

# 4. Save the data ----

save(data_benthic, file = "data/02_misc/data-benthic.RData")

# 5. Export site coordinates (for predictors extraction) ----

data_benthic %>% 
  select(decimalLatitude, decimalLongitude) %>% 
  distinct() %>% 
  mutate(type = "obs",
         site_id = as.character(row_number(.)-1)) %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>% 
  st_write(., dsn = "data/03_site-coords/site-coords_obs.shp", delete_dsn = TRUE)
