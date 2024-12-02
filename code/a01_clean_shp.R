# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(sf)
sf_use_s2(FALSE) # Switch from S2 to GEOS
library(nngeo)
library(terra)
library(tidyterra)

# 2. Coral reefs ----

## 2.1 Load data ----

data_regions <- read_sf("data/01_maps/02_clean/01_gcrmn-regions/gcrmn_regions.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid()

data_reefs <- read_sf("data/01_maps/01_raw/01_reefs/reef_500_poly.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid()

data_eez <- read_sf("data/01_maps/01_raw/02_eez/eez_v12.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid()

## 2.2 Filter reefs falling within Caribbean GCRMN region ----

data_reefs <- st_intersection(data_reefs, data_regions) %>%
  filter(region == "Caribbean")

## 2.3 Visual check ----

ggplot() +
  geom_sf(data = data_reefs, fill = "red")

# 3. Define areas to be used for countries and territories chapters ----

## 3.1 Filter EEZ with coral reefs ----

data_eez_intersects <- st_intersection(data_eez, data_reefs)

data_eez <- data_eez %>% 
  filter(TERRITORY1 %in% c(unique(data_eez_intersects$TERRITORY1),
                           "Guatemala")) # Add Guatemala, no coral reefs based on WRI data

ggplot() +
  geom_sf(data = data_eez, fill = "lightblue") + 
  coord_sf(xlim = c(-100, -55), ylim = c(7.5, 35))

## 3.2 Identify overclaimed territories and joint regimes ----

data_eez2 <- data_eez %>% 
  mutate(overclaimed = ifelse(str_detect(GEONAME, "Over"), TRUE, FALSE))

plot <- ggplot() +
  geom_sf(data = data_eez2, aes(fill = overclaimed)) +
  scale_fill_manual(breaks = c(TRUE, FALSE),
                    values = c("red", "white")) +
  coord_sf(xlim = c(-100, -55), ylim = c(7.5, 35))

ggsave("figs/06_additional/overclaimed-territories.png", height = 4)

data_eez2 <- data_eez %>% 
  mutate(joint_regime = ifelse(str_detect(GEONAME, "Joint"), TRUE, FALSE))

plot <- ggplot() +
  geom_sf(data = data_eez2, aes(fill = joint_regime)) +
  scale_fill_manual(breaks = c(TRUE, FALSE),
                    values = c("red", "white")) +
  coord_sf(xlim = c(-100, -55), ylim = c(7.5, 35))

ggsave("figs/06_additional/joint-regime-areas.png", height = 4)

rm(data_eez2, plot)

## 3.3 Remove non-caribbean EEZ ----

### 3.3.1 United States ----

data_eez <- data_eez %>% 
  filter(SOVEREIGN1 == "United States" & GEONAME == "United States Exclusive Economic Zone") %>%
  st_cast(., "POLYGON") %>% 
  filter(row_number() == 1) %>% 
  bind_rows(data_eez %>% 
              filter(!(SOVEREIGN1 == "United States" & 
                         GEONAME == "United States Exclusive Economic Zone")), .) %>% 
  filter(!(GEONAME %in% c("Joint regime area: United States / Russia",
                          "Overlapping claim: United States (Puerto Rico) / Dominican Republic")))

### 3.3.2 Mexico ----

data_eez <- data_eez %>% 
  filter(SOVEREIGN1 == "Mexico") %>% 
  st_cast(., "POLYGON") %>% 
  filter(row_number() == 2) %>% 
  bind_rows(data_eez %>% 
              filter(!(SOVEREIGN1 == "Mexico")), .)

### 3.3.3 Colombia ----

data_eez <- data_eez %>% 
  filter(SOVEREIGN1 == "Colombia" & GEONAME == "Colombian Exclusive Economic Zone") %>% 
  st_cast(., "POLYGON") %>% 
  filter(row_number() == 2) %>% 
  bind_rows(data_eez %>% 
              filter(!(SOVEREIGN1 == "Colombia")), .)

### 3.3.4 Nicaragua ----

data_eez <- data_eez %>% 
  filter(SOVEREIGN1 == "Nicaragua") %>% 
  st_cast(., "POLYGON") %>% 
  filter(row_number() == 2) %>% 
  bind_rows(data_eez %>% 
              filter(TERRITORY1 != "Nicaragua"), .)

### 3.3.5 Costa Rica ----

data_eez <- data_eez %>% 
  filter(SOVEREIGN1 == "Costa Rica" & GEONAME == "Costa Rican Exclusive Economic Zone") %>% 
  st_cast(., "POLYGON") %>% 
  filter(row_number() == 2) %>% 
  bind_rows(data_eez %>% 
              filter(!(SOVEREIGN1 == "Costa Rica" & 
                         GEONAME == "Costa Rican Exclusive Economic Zone")), .) %>% 
  filter(GEONAME != "Joint regime area: Costa Rica / Ecuador (Galapagos)")

### 3.3.6 Panama ----

data_eez <- data_eez %>% 
  filter(SOVEREIGN1 == "Panama") %>% 
  st_cast(., "POLYGON") %>% 
  st_intersection(., data_regions) %>% 
  filter(region == "Caribbean") %>% 
  bind_rows(data_eez %>% 
              filter(!(TERRITORY1 == "Panama")), .)

### 3.3.7 Honduras ----

data_eez <- data_eez %>% 
  filter(GEONAME == "Honduran Exclusive Economic Zone") %>% 
  st_cast(., "POLYGON") %>% 
  filter(row_number() == 2) %>% 
  nngeo::st_remove_holes(.) %>% 
  bind_rows(data_eez %>% 
              filter(GEONAME != "Honduran Exclusive Economic Zone"), .) %>% 
  filter(!(GEONAME %in% c("Joint regime area: Honduras / United Kingdom (Cayman Islands)",
                          "Overlapping claim: Belize / Honduras")))

### 3.3.8 Guatemala ----

data_eez <- data_eez %>% 
  filter(GEONAME == "Guatemalan Exclusive Economic Zone") %>% 
  st_cast(., "POLYGON") %>% 
  filter(row_number() == 2) %>% 
  nngeo::st_remove_holes(.) %>% 
  bind_rows(data_eez %>% 
              filter(GEONAME != "Guatemalan Exclusive Economic Zone"), .)

## 3.4 Remove useless columns ----

data_eez <- data_eez %>% 
  rename(country = SOVEREIGN1, territory = TERRITORY1) %>% 
  select(country, territory) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid()

## 3.5 Particular cases ----

### 3.5.1 Flower Garden Banks ----

data_fgb <- st_read("data/01_maps/01_raw/06_noaa/fgbnms_py/FGBNMS_py.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_buffer(0.3) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  mutate(country = "United States", territory = "Flower Garden Banks")

### 3.5.2 Florida ----

data_fk <- st_read("data/01_maps/01_raw/06_noaa/fknms_py2/fknms_py.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_buffer(0.3) %>% 
  summarise(geometry = st_union(geometry))

data_fk <- tibble(long = c(-80.1, -80), 
                  lat = c(25.5, 27.1)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  summarise() %>% 
  st_cast("LINESTRING") %>% 
  st_buffer(0.3) %>% 
  bind_rows(data_fk, .) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  mutate(country = "United States", territory = "Florida") %>% 
  st_buffer(0.4) %>% 
  st_difference(., data_eez %>% 
                  filter(country %in% c("Bahamas", "Cuba")) %>% 
                  summarise(geometry = st_union(geometry))) %>% 
  st_difference(., st_read("data/01_maps/01_raw/05_princeton/usa/USA_adm0.shp"))

data_eez <- data_eez %>% 
  filter(territory != "United States") %>% 
  bind_rows(., data_fgb) %>% 
  bind_rows(., data_fk)

rm(data_eez_intersects, data_fgb, data_fk, data_regions)

### 3.5.3 Collectivity of Saint Martin and Sint Maarten ----

data_eez <- data_eez %>% 
  filter(territory %in% c("Collectivity of Saint Martin", "Sint-Maarten")) %>% 
  mutate(territory = "Sint-Maarten - Saint-Martin") %>% 
  group_by(territory) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  ungroup() %>% 
  nngeo::st_remove_holes(.) %>% 
  bind_rows(data_eez %>% filter(!(territory %in% c("Collectivity of Saint Martin", "Sint-Maarten"))), .)

### 3.5.4 Mexico (Caribbean Sea) ----

data_meow <- st_read("data/01_maps/01_raw/07_meow/Marine_Ecoregions_Of_the_World__MEOW_.shp") %>% 
  filter(ECO_CODE_X %in% c(68)) %>% 
  st_transform(crs = 4326)

data_mex_cs <- data_eez %>% 
  filter(territory == "Mexico") %>% 
  st_intersection(., data_meow) %>% 
  mutate(territory = "Mexico (Caribbean Sea)") %>% 
  st_remove_holes()

### 3.5.5 Mexico (Gulf of Mexico) ----

data_eez <- data_eez %>% 
  filter(territory == "Mexico") %>% 
  st_difference(., data_meow) %>% 
  st_remove_holes() %>% 
  st_cast(., "POLYGON") %>% 
  filter(row_number() != 3) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  mutate(territory = "Mexico (Gulf of Mexico)") %>% 
  bind_rows(data_eez %>% filter(territory != "Mexico"), .) %>% 
  bind_rows(., data_mex_cs)

rm(data_meow, data_mex_cs)

### 3.5.6 Seaflower Biosphere Reserve ----

data_seaflower <- st_read("data/01_maps/01_raw/08_seaflower/LimiteSeaflower.shp") %>% 
  mutate(territory = "Seaflower Biosphere Reserve") %>% 
  select(territory)

data_eez <- st_difference(data_eez, data_seaflower) %>% 
  bind_rows(., data_seaflower) %>% 
  rename(area = territory) %>% 
  select(area)

## 3.6 Minor corrections ----

### 3.6.1 Dominican Republic ----

data_eez <- data_eez %>% 
  filter(area == "Dominican Republic") %>% 
  st_cast(., "POLYGON") %>% 
  filter(row_number(.) == 1) %>% 
  bind_rows(., data_eez %>% filter(area != "Dominican Republic"))

### 3.6.2 Nicaragua ----

data_eez <- data_eez %>% 
  filter(area == "Nicaragua") %>% 
  st_cast(., "POLYGON") %>% 
  filter(row_number(.) == 1) %>% 
  bind_rows(., data_eez %>% filter(area != "Nicaragua"))

### 3.6.3 Costa Rica ----

data_eez <- data_eez %>% 
  filter(area == "Costa Rica") %>% 
  st_cast(., "POLYGON") %>% 
  filter(row_number(.) == 1) %>% 
  bind_rows(., data_eez %>% filter(area != "Costa Rica"))

### 3.6.4 Venezuela ----

data_eez <- data_eez %>% 
  filter(area == "Venezuela") %>% 
  st_cast(., "POLYGON") %>% 
  filter(row_number(.) == 2) %>% 
  bind_rows(., data_eez %>% filter(area != "Venezuela"))

### 3.6.5 Barbados ----

data_eez <- data_eez %>% 
  filter(area == "Barbados") %>% 
  group_by(area) %>% 
  summarise(geometry = st_union(geometry)) %>%
  ungroup() %>% 
  bind_rows(., data_eez %>% filter(area != "Barbados"))

### 3.6.6 Colombia ----

data_polygon <- tibble(lat = c(11, 15),
                       lon = c(-79.5, -78)) %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc()

data_eez <- data_eez %>% 
  filter(area == "Colombia") %>% 
  st_cast(., "POLYGON") %>% 
  filter(row_number(.) == 1) %>% 
  st_difference(., data_polygon) %>% 
  bind_rows(., data_eez %>% filter(area != "Colombia"))

### 3.6.7 Holes for all areas ----

data_area <- data_eez %>%
  group_by(area) %>% 
  nngeo::st_remove_holes() %>% 
  ungroup()

## 3.7 Export the data ----

st_write(data_area, "data/01_maps/02_clean/03_eez/caribbean_area.shp", append = TRUE, delete_dsn = TRUE)

# 4. Intersect with reef data ----

data_reefs <- data_reefs %>% 
  select(-GRIDCODE) %>% 
  st_intersection(., data_area) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid() %>% 
  group_by(area) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  ungroup()

st_write(data_reefs, "data/01_maps/02_clean/02_reefs/reefs.shp", append = TRUE, delete_dsn = TRUE)

# 5. Land (Princeton) ----

## 5.1 List of shp to combine ----

list_shp <- list.files(path = "data/01_maps/01_raw/05_princeton",
                       pattern = ".shp$", full.names = TRUE, recursive = TRUE)

## 5.2 Combine shp ----

data_land <- map_dfr(list_shp, ~st_read(.)) %>% 
  rename(TERRITORY1 = NAME_ENGLI) %>% 
  st_transform(crs = 4326) %>% 
  select(TERRITORY1)

## 5.3 Correct issue for grouped territories ----

data_land <- data_land %>%
  filter(TERRITORY1 == "Bonaire, Saint Eustatius and Saba") %>% 
  st_cast(., "POLYGON") %>% 
  mutate(TERRITORY1 = case_when(row_number() == 1 ~ "Bonaire",
                                row_number() == 2 ~ "Bonaire",
                                row_number() == 3 ~ "Saint Eustatius",
                                row_number() == 4 ~ "Saba")) %>% 
  bind_rows(data_land, .) %>% 
  filter(TERRITORY1 != "Bonaire, Saint Eustatius and Saba") %>% 
  rename(territory = TERRITORY1) %>% 
  mutate(territory = str_replace_all(territory, c("Virgin Islands, U.S." = "United States Virgin Islands",
                                                  "Saint Eustatius" = "Sint-Eustatius",
                                                  "Saint-Martin" = "Sint-Marteen - Saint-Martin")))

## 5.4 Export the data ----

st_write(data_land, "data/01_maps/02_clean/05_princeton/land.shp", append = FALSE, delete_dsn = TRUE)

# 6. Topography ----

data_topo <- terra::rast("data/01_maps/01_raw/09_topography/topography.tif")

data_topo <- terra::crop(data_topo, as_spatvector(data_land))

data_topo <- terra::mask(data_topo, as_spatvector(data_land), touches = TRUE)

terra::writeRaster(data_topo, "data/01_maps/02_clean/06_topography/topography.tif")
