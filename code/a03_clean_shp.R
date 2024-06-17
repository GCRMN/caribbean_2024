# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(sf)
sf_use_s2(FALSE) # Switch from S2 to GEOS
library(nngeo)

# 2. Bathymetry ----

## 2.1 List path of shapefiles ----

list_shp <- list.files(path = "data/01_shp/01_raw/04_natural-earth/ne_10m_bathymetry_all/",
                       pattern = ".shp", full.names = TRUE)

## 2.2 Combine shapefiles ----

data_bathy <- map_dfr(list_shp, ~st_read(., quiet = TRUE)) %>% 
  mutate(fill_color = case_when(depth == 0 ~ "#e1f5fe",
                                depth == 200 ~ "#b3e5fc",
                                depth == 1000 ~ "#81d4fa",
                                depth == 2000 ~ "#4fc3f7",
                                depth == 3000 ~ "#29b6f6",
                                depth == 4000 ~ "#03a9f4",
                                depth == 5000 ~ "#039be5",
                                depth == 6000 ~ "#0288d1",
                                depth == 7000 ~ "#0288d1",
                                depth == 8000 ~ "#0277bd",
                                depth == 9000 ~ "#01579b",
                                depth == 10000 ~ "black")) %>% 
  mutate(color = fct_reorder(fill_color, depth)) %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline() %>% 
  st_make_valid() %>% 
  st_transform(crs = 4326) %>% 
  select(depth, fill_color)

## 2.3 Export the data ----

save(data_bathy, file = "data/01_shp/02_clean/04_natural-earth/ne_10m_bathymetry_all.RData") # RData

rm(list_shp, data_bathy)

# 3. Coral reefs ----

## 3.1 Load data ----

data_regions <- read_sf("data/01_shp/02_clean/01_gcrmn-regions/gcrmn_regions.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid()

data_reefs <- read_sf("data/01_shp/01_raw/01_reefs/reef_500_poly.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid()

data_eez <- read_sf("data/01_shp/01_raw/02_eez/eez_v12.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid()

## 3.2 Filter reefs falling within Caribbean GCRMN region ----

data_reefs <- st_intersection(data_reefs, data_regions) %>%
  filter(region == "Caribbean")

# 4. EEZ ----

## 4.1 Filter EEZ with coral reefs ----

data_eez_intersects <- st_intersection(data_eez, data_reefs)

data_eez <- data_eez %>% 
  filter(TERRITORY1 %in% unique(data_eez_intersects$TERRITORY1))

## 4.2 Remove non-caribbean EEZ ----

### 4.2.1 United States ----

data_eez <- data_eez %>% 
  filter(SOVEREIGN1 == "United States" & GEONAME == "United States Exclusive Economic Zone") %>%
  st_cast(., "POLYGON") %>% 
  filter(row_number() == 1) %>% 
  bind_rows(data_eez %>% 
              filter(!(SOVEREIGN1 == "United States" & 
                         GEONAME == "United States Exclusive Economic Zone")), .) %>% 
  filter(GEONAME != "Joint regime area: United States / Russia")

### 4.2.2 Mexico ----

data_eez <- data_eez %>% 
  filter(SOVEREIGN1 == "Mexico") %>% 
  st_cast(., "POLYGON") %>% 
  filter(row_number() == 2) %>% 
  bind_rows(data_eez %>% 
              filter(!(SOVEREIGN1 == "Mexico")), .)
  
### 4.2.3 Colombia ----

data_eez <- data_eez %>% 
  filter(SOVEREIGN1 == "Colombia" & GEONAME == "Colombian Exclusive Economic Zone") %>% 
  st_cast(., "POLYGON") %>% 
  filter(row_number() == 2) %>% 
  bind_rows(data_eez %>% 
              filter(!(SOVEREIGN1 == "Colombia" & 
                         GEONAME == "Colombian Exclusive Economic Zone")), .)

### 4.2.4 Nicaragua ----

data_eez <- data_eez %>% 
  filter(SOVEREIGN1 == "Nicaragua") %>% 
  st_cast(., "POLYGON") %>% 
  filter(row_number() == 2) %>% 
  bind_rows(data_eez %>% 
              filter(!(TERRITORY1 == "Nicaragua")), .)

### 4.2.5 Costa Rica ----

data_eez <- data_eez %>% 
  filter(SOVEREIGN1 == "Costa Rica" & GEONAME == "Costa Rican Exclusive Economic Zone") %>% 
  st_cast(., "POLYGON") %>% 
  filter(row_number() == 2) %>% 
  bind_rows(data_eez %>% 
              filter(!(SOVEREIGN1 == "Costa Rica" & 
                         GEONAME == "Costa Rican Exclusive Economic Zone")), .) %>% 
  filter(GEONAME != "Joint regime area: Costa Rica / Ecuador (Galapagos)")

### 4.2.6 Panama ----

data_eez <- data_eez %>% 
  filter(SOVEREIGN1 == "Panama") %>% 
  st_cast(., "POLYGON") %>% 
  st_intersection(., data_regions) %>% 
  filter(region == "Caribbean") %>% 
  bind_rows(data_eez %>% 
              filter(!(TERRITORY1 == "Panama")), .)

## 4.3 Add EEZ to reefs ----

#data_reefs <- st_intersection(data_reefs, data_eez) %>% 
#  select(-MRGID)

#st_write(data_reefs, "data/01_shp/02_clean/02_reefs/reefs.shp", append = TRUE)

## 4.4 Remove EEZ without reefs ----

data_eez <- data_eez %>% 
  filter(TERRITORY1 %in% unique(data_reefs$TERRITORY1))

## 4.5 Remove holes in EEZ ----

# todo

st_write(data_eez, "data/01_shp/02_clean/03_eez/caribbean_eez.shp", append = TRUE)

# 5. Land (Princeton) ----

list_shp <- list.files(path = "data/01_shp/01_raw/05_princeton",
                       pattern = ".shp$", full.names = TRUE, recursive = TRUE)

data_land <- map_dfr(list_shp, ~st_read(.)) %>% 
  st_transform(crs = 4326)

st_write(data_land, "data/01_shp/02_clean/05_princeton/land.shp", append = TRUE)
